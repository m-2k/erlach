-module(access).
% -compile(export_all).
-export([t/0, meta/1, discavering/2]).

-include_lib("db/include/board.hrl").
-include_lib("db/include/thread.hrl").
-include("erlach.hrl").

% -ifndef(SESSION).
% -define(SESSION, (wf:config(n2o,session,erlach_session))).
% -endif.

-define(DEFAULT_ACCESS_RULES, [{read,default},{write,blog},{read,blog}]).

% Action: read|write|moderate

% Type: any|blog|message|etc...
check_time({Begins,Expire}) -> allow;
check_time(none) -> none.
 
acl({{user,2}, {private,global,undefined}}) -> {infinity,infinity}; % for root access
acl({{user,2}, {private,board,2}}) -> {infinity,infinity};
acl({{user,2}, {private,thread,4}}) -> {infinity,infinity};
acl({{user,2}, {private,group,8}}) -> {infinity,infinity};
acl(_) -> none.

% access(group,7) -> [];
access(group,8) -> [{anonymous, read, blog},{private, write, blog}];
% access(board,1) -> [];
access(board,2) -> [{anonymous, read, blog},{private, read, blog}];
% access(thread,1) -> [{private, read, blog}];
% access(thread,2) -> [{private, read, blog}];
% access(thread,3) -> [{private2, write, blog},{private, write, blog},{private, write, blog},{anonymous, read, default}];
access(thread,4) -> [{anonymous, read, default}, {private, write, blog},{private, read, blog}];
access(_,_) -> [].
 
weight(read) -> 0;
weight(write) -> 1;
weight(moderate) -> 2.
 
% is_rising(Action1,Action2) -> weight(Action1) < weight(Action2).
compare(Action,Action) -> 0;
compare(Action1,Action2) -> weight(Action2) - weight(Action1).
% u_is_temp() -> false.

meta(Level,Lid) -> {Level,Lid,access(Level,Lid)}.
% meta(#group{id=Id,access=Access}) -> {group,Id,Access};
meta(#board{id=Id,access=Access}) -> {board,Id,Access};
meta(#thread{id=Id,access=Access}) -> {thread,Id,Access}.

t() -> discavering(2, [meta(group,8), meta(board,2), meta(thread,4)]).

discavering(User, AccessMetaList) ->
    case dive_access(AccessMetaList, ?DEFAULT_ACCESS_RULES, u:to_id(User)) of
        none -> {error, access_denied};
        Value -> {ok, Value}
    end.

dive_access(_Access, [], _Uid) -> none;
dive_access([], Acc, _Uid) -> Acc;
dive_access([{Level,Lid,Access}=AccessMeta|Tail], Acc, Uid) ->
    % wf:info(?MODULE, "dive_access: Acc: ~p",[Acc]),
    case check_access(Uid, Level, Lid, Access) of
        none -> none;
        skip -> dive_access(Tail, Acc, Uid);
        Checked -> % find first level and return
            % wf:info(?MODULE, "dive_access: Checked: ~p",[Checked]),
            NewAccess=lists:foldl(fun({Action,Type}=AT, AccAnd) ->
                % wf:info(?MODULE, "dive_access: Acc: ~p, AT: ~p, AccAnd: ~p",[Acc,AT,AccAnd]),
                case access_merge(AT, Acc, logical_and) of nothing -> AccAnd; N -> [N|AccAnd] end
            end,[],Checked),
            % wf:info(?MODULE, "dive_access: NewAccess: ~p",[NewAccess]),
            dive_access(Tail, NewAccess, Uid)
    end.

% none, skip or [{Action,Type}, ...]
check_access(_Uid, _Level, _Lid, []) -> skip;
check_access(Uid, Level, Lid, Access) ->
    A = lists:foldl(fun(E, Acc) ->
            AT = case E of
                {anonymous,Action,Type} -> case u:is_temp(Uid) of true -> {Action,Type}; _ -> skip end;
                {Group,Action,Type} ->
                    Acl = acl({{user,Uid},{Group,Level,Lid}}),
                    case check_time(Acl) of
                        allow -> {Action,Type};
                        _ -> skip
                    end
            end,
            % wf:info(?MODULE,"E: ~p Acc: ~p AT: ~p / To: ~p", [E,Acc,AT, {Level,Lid}]),
            access_merge(AT,Acc,logical_or)
        end,[],Access),
    % wf:info(?MODULE,"check_access: ~p", [A]),
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