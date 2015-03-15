-module(access).
% -compile(export_all).
-export([ meta/1,
          discavering/2,
          lookup/2,
          is_allow/3,
          define/2,
          define/6,
          % restrict/2,
          acl/1,
          default_access/1,
          update_elements_access_list/1]).

% Модель защиты – это всегда эффективная функция проверки check к двум множествам: списку доступа объекта и списку возможностей пользователя

-include_lib("db/include/board.hrl").
-include_lib("db/include/thread.hrl").
-include_lib("db/include/db.hrl").
-include("erlach.hrl").

% -ifndef(SESSION).
% -define(SESSION, (wf:config(n2o,session,erlach_session))).
% -endif.

-define(DEFAULT_ACCESS_RULES, [{write,post},{write,blog},{write,thread},{write,message},{read,request}]).
-define(FULL_ACCESS_RULES, [{moderate,post},{moderate,blog},{moderate,thread},{moderate,message},{moderate,request}]).

default_access(board) -> [
    {anonymous,  write, post},
    {anonymous,  read,  blog},
    {anonymous,  write, thread},
    {anonymous,  write, message},
    {anonymous,  read,  request},
    {registered, write, post},
    {registered, read,  blog},
    {registered, write, thread},
    {registered, write, message},
    {registered, read,  request},
    {private,    write, post},
    {private,    read,  blog},
    {private,    write, thread},
    {private,    write, message},
    {private,    read,  request},
    {moderator,  moderate, post},
    {moderator,  moderate, blog},
    {moderator,  moderate, thread},
    {moderator,  moderate, message},
    {moderator,  moderate, request}
    ];
default_access(_) -> [].

% redefining default #db_element.access for tables in db
update_elements_access_list(Table) ->
    lists:foreach(fun(E) -> E2=setelement(#db_element.access,E,default_access(Table)), kvs:put(E2) end, kvs:all(Table)).

% Action: read|write|moderate

% Type: any|blog|message|etc...
check_time({Begins,Expire}) -> allow;
check_time(none) -> none.

% define(Uid,#group{id=Gid}) -> define(Uid,private,group,Gid,infinity,infinity);
define(Uid,#board{id=Bid}) -> define(Uid,private,board,Bid,infinity,infinity);
define(Uid,#thread{id=Tid}) -> define(Uid,private,thread,Tid,infinity,infinity).

% access:define(1,private,thread,10,infinity,infinity).
define(Uid,Group,Level,Lid,Begins,Expire) ->
    kvs_acl:define_access({user,Uid}, {Group,Level,Lid}, {Begins,Expire}).
    
% restrict(Table,Id) ->
%     {ok, E} = kvs:get(Table,Id),
%     E2 = case E of
%         #thread{}=T -> T#thread{access=[{anonymous, read, blog},{private, write, blog}]};
%         #board{}=B -> B#board{access=[{anonymous, read, blog},{private, write, blog}]}
%     end,
%     kvs:put(E2).

acl({{user,_}, {anonymous,_,_}}) -> {infinity,infinity};
acl({{user,1}, {moderator,board,1}}) -> {infinity,infinity};
acl({{user,1}, {moderator,board,4}}) -> {infinity,infinity};
acl({{user,1}, {moderator,thread,2}}) -> {infinity,infinity};
% acl({{user,1}, {private,board,1}}) -> {infinity,infinity};
% acl({{user,1}, {private,board,6}}) -> {infinity,infinity};
% acl({{user,1}, {private,thread,10}}) -> {infinity,infinity};
% acl({{user,2}, {private,global,undefined}}) -> {infinity,infinity}; % for root access
% acl({{user,2}, {private,board,2}}) -> {infinity,infinity};
% acl({{user,2}, {private,thread,4}}) -> {infinity,infinity};
% acl({{user,2}, {private,group,8}}) -> {infinity,infinity};
% acl(_) -> none.
acl(Key) -> kvs_acl:check([ Key ]).

weight(nothing) -> 0;
weight(read) -> 1;
weight(write) -> 2;
weight(moderate) -> 3.
 
% is_rising(Action1,Action2) -> weight(Action1) < weight(Action2).
compare(Action,Action) -> 0;
compare(Action1,Action2) -> weight(Action2) - weight(Action1).
% u_is_temp() -> false.

% meta(Level,Lid) -> {Level,Lid,access(Level,Lid)}.
% meta(#group{id=Id,access=Access}) -> {group,Id,Access};
meta(#board{id=Id,access=Access}) -> {board,Id,Access};
meta(#thread{id=Id,access=Access}) -> {thread,Id,Access}.

% t() -> discavering(2, [meta(group,8), meta(board,2), meta(thread,4)]).

discavering(User, AccessMetaList) ->
    case dive_access(AccessMetaList, ?FULL_ACCESS_RULES, u:to_id(User)) of
        none -> []; % {error, access_denied};
        Value -> Value % {ok, Value}
    end.

dive_access(_Access, [], _Uid) -> none;
dive_access([], Acc, _Uid) -> Acc;
dive_access([{Level,Lid,Access}=AccessMeta|Tail], Acc, Uid) ->
    wf:info(?MODULE, "dive_access: Acc: ~p",[Acc]),
    case check_access(Uid, Level, Lid, Access) of
        none -> none;
        skip -> dive_access(Tail, Acc, Uid);
        Checked -> % find first level and return
            wf:info(?MODULE, "dive_access: Checked: ~p",[Checked]),
            NewAccess=lists:foldl(fun({Action,Type}=AT, AccAnd) ->
                wf:info(?MODULE, "dive_access: Acc: ~p, AT: ~p, AccAnd: ~p",[Acc,AT,AccAnd]),
                case access_merge(AT, Acc, logical_and) of nothing -> AccAnd; N -> [N|AccAnd] end
            end,[],Checked),
            wf:info(?MODULE, "dive_access: NewAccess: ~p",[NewAccess]),
            dive_access(Tail, NewAccess, Uid)
    end.

% none, skip or [{Action,Type}, ...]
check_access(_Uid, _Level, _Lid, []) -> skip;
check_access(Uid, Level, Lid, Access) ->
    A = lists:foldl(fun(E, Acc) ->
            AT = case E of
                {anonymous,Action,Type} -> case u:is_temp(Uid) of true -> {Action,Type}; _ -> skip end;
                {registered,Action,Type} -> case u:is_temp(Uid) of false -> {Action,Type}; _ -> skip end;
                {Group,Action,Type} ->
                    Acl = acl({{user,Uid},{Group,Level,Lid}}),
                    case check_time(Acl) of
                        allow -> {Action,Type};
                        _ -> skip
                    end
            end,
            wf:info(?MODULE,"E: ~p Acc: ~p AT: ~p / To: ~p", [E,Acc,AT, {Level,Lid}]),
            access_merge(AT,Acc,logical_or)
        end,[],Access),
    wf:info(?MODULE,"check_access: ~p", [A]),
    case A of [] -> none; _ -> A end.
 
access_merge(skip, List, _Operation) -> List;
access_merge({Action,Type}=AT, List, Operation) ->
    case lists:keyfind(Type,2,List) of
        false ->                % new
             case Operation of logical_or -> [AT|List]; logical_and -> nothing end;
        % {Action,Type} -> List;  % duplicate
        {OldA,Type} ->          % comparsion
            case {Operation,compare(OldA,Action)} of
                {logical_or, C} when C > 0 -> lists:keyreplace(Type,2,List,AT);
                {logical_and, C} when C < 0 -> {Action,Type};
                {logical_and, C} -> {OldA,Type};
                _ -> List
            end
    end.


lookup(Type,Access) -> lists:keyfind(Type,2,Access).
is_allow(Type,Action,Access) ->
    case lookup(Type,Access) of
        {Value,Type} -> case compare(Action,Value) of C when C < 0 -> false; _ -> true end;
        _ -> false end.
% case_lookup(Fun, Type, Access) -> case lookup(Type, Access) of {Value, Type} -> Fun(); _ -> [] end.