-module(u).
-compile(export_all).

-include_lib("n2o/include/wf.hrl").
% -include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/feed.hrl").
-include_lib("db/include/user.hrl").
-include("erlach.hrl").

% -ifndef(SESSION).
% -define(SESSION, (wf:config(n2o,session,n2o_session))).
% -endif.

-define(SESSION_USER, session_user).

new_user(SessionID) ->
	new_user(SessionID, calendar:local_time()).
new_user(SessionID, Created) ->
	U = #user3{id={?T,SessionID},created=Created},
	% wf:info(?MODULE, "TIME: ~p",[timer:tc(wf,user,[U])]),
	% wf:info(?MODULE, "TIME: ~p",[timer:tc(ets,insert,[cookies,{{SessionID,?USER},U}])]),
	% ets:insert(cookies,{{SessionID,?USER},U}), % speed 5-6 ms
	wf:user(U), % speed 6-9 ms
	wf:wire(#transfer{events={server,{temp_user_created,U}}}),
	% Fun = erlang:get(user_test),
	% case is_function(Fun) of true -> Fun(); _ -> ok end,
	U.
ensure_user() ->
	wf:info(?MODULE, "Ensure user: ~p", [?CTX#cx.session]),
	User2 = case ?CTX#cx.session of
		{{SID, _Key}, #cookie{status=new, issued=Issued}} ->
			new_user(SID, Issued);
		{{SID, _Key}, #cookie{status=actual}} ->
			User = case wf:user() of
				#user3{}=U -> U;
				undefined -> new_user(SID);
				UID ->
					case kvs:get(user3, UID) of
						{ok, U} -> U;
						_NotFound -> new_user(SID)
					end
			end,
			wf:info(?MODULE, "User exist: ~p", [User]),
			User;
		{{SID, _Key}, _Cookie} ->
			new_user(SID)
	end,
	u:put(User2),
	User2.

logout() ->
	u:put(undefined),
	wf:logout(). % TODO: testing, maybe add temp creation

join() ->
	case wf:user() of
		#user3{id={?T, _SID}}=U ->
			case kvs:add(U#user3{id=kvs:next_id(user3, 1), password=new_password()}) of
				{ok, Stored} ->
					wf:user(Stored#user3.id),
					{ok, Stored};
				Err -> Err
			end;
		Wrong -> {error, {wrong_user_type, Wrong}}
	end.

new_password() -> list_to_binary(unique:to_hex(crypto:rand_bytes(8))).

check_access(Feature) -> check_access(u:id(), Feature).
check_access(U, Feature) -> % User or Uid
	Uid = u:to_id(U),
	kvs_acl:check([ {{user,Uid},Feature} ]).
% check_access(U, Feature) -> % User or Uid
% 	Uid = u:to_id(U),
% 	case is_temp(Uid) of
% 		false -> kvs_acl:check([ {{user,Uid},Feature} ]);
% 		% _X -> kvs_acl:check([ {{user,id(User)},Feature} ]); % TODO: удалить после допила avz
% 		_ -> none
% 	end.
define_access(U, Feature, Action) -> % User or Uid
	Uid = u:to_id(U),
	case is_temp(Uid) of
		false -> kvs_acl:define_access({user,Uid}, Feature, Action);
		_ -> skip
	end.
put(User) -> erlang:put(?SESSION_USER, User), wf:user(User).
get() ->
	case erlang:get(?SESSION_USER) of
		undefined -> u:ensure_user();
		U -> U
	end.
store(User) -> update(User).
update(User) ->
	{ok, _} = kvs:put(User),
	u:put(User).
reload() -> u:ensure_user().

id() -> u:id(u:get()).
id(#user3{id=Id}) -> Id.
to_id(#user3{}=U) -> u:id(U);
to_id(Id) -> Id.
is_temp() -> is_temp(u:id()).
is_temp(U) -> case u:to_id(U) of {?T, _} -> true; _ -> false end.
restricted_call(Fun, Feature, Default) ->
	% wf:info(?MODULE, "Restricted call ~p ~p ~p", [u:get(), Feature, Default]),
	case u:check_access(u:get(), Feature) of
		allow -> Fun();
		_ -> Default
	end.
restricted_call(Fun, Feature) -> restricted_call(Fun, Feature, undefined).
is_admin() -> is_admin(u:get()).
is_admin(UserOrId) ->
	% wf:info(?MODULE, "Check for admin: ~p ~p", [u:id(), kvs_acl:check([{{user,u:id()}, {feature,admin}}])]),
	case kvs_acl:check([{{user,u:to_id(UserOrId)}, {feature,admin}}]) of allow -> true; _ -> false end.

store_temp_user() ->
	User = u:get(),
	{ok, Stored} = kvs:add(User#user3{id=kvs:next_id(user3, 1)}),
	u:put(Stored),
	Stored.

update_prop(TupleList) ->
	lists:foldl(fun({Key, Value}, User) ->
			case Key of
				id		-> User#user3{id=Value};
				name	-> User#user3{name=Value};
			    expired	-> User#user3{expired=Value};
			    enabled	-> User#user3{enabled=Value};
			    % banned	-> User#user3{banned=Value};
			    deleted	-> User#user3{deleted=Value};
			    % role	-> User#user3{role=Value};
			    password-> User#user3{password=Value};
			    salt	-> User#user3{salt=Value};
			    tokens	-> User#user3{tokens=Value};
			    % access	-> User#user3{access=Value};
			    % names	-> User#user3{names=Value};
			    created	-> User#user3{created=Value};
			    % date	-> User#user3{date=Value};
			    % zone	-> User#user3{zone=Value};
			    % type	-> User#user3{type=Value};
			    email	-> User#user3{email=Value};
			    phone	-> User#user3{phone=Value}
			end
		end, u:get(), TupleList).
		
t() -> %% testing
	erlang:put(?SESSION_USER, #user3{id={?T,111}}),
	update_prop([{name,"ololosh"}]).

grant(Id) ->
	kvs_acl:define_access({user,Id}, {feature,admin}, allow).
clone(Id) ->
	{ok,U1} = kvs:get(user3,Id),
	U2 = setelement(19,U1,wf:to_binary(wf:temp_id())),
	U3 = setelement(30,U2,[{twitter,wf:to_binary(wf:temp_id())}|undefined]),
	kvs:put(U3).
	
names() -> names(u:get()).
names(User) ->
	% kvs:entries(kvs:get(feed, {user,u:id(User)}), name, undefined).
    case kvs:get(feed, {user, u:id(User)}) of
        {ok, Uf} -> kvs:traversal(name, Uf#feed.top, Uf#feed.entries_count, #iterator.prev);
		_ -> []
	end.
put_name() ->
	case u:is_temp() of
		false ->
			% storing
			{ok, result};
		_ -> {error, prevent_for_temp_user}
	end.

% {read, {board,3, thread,Type}} -> {allow,infinity,infinity}
% u:ca({board,write,{default,7},{thread,4}}).-> {allow,infinity,infinity}
pa() ->
	kvs_acl:define_access({user,1}, {read, {board, 3, thread, default}}, {allow,infinity,infinity}).

get_initial_access(Uid, {Action, {_Purpose, _Section, Object, Type}}=Feature) ->
	% Read -> {allow, infinity},	% as {read, global}
	% Write -> {allow, infinity},	% as {write, global}
	% {undefined,Read,Write,none}.
	Allow={allow,infinity,infinity},
	Deny = none,
	Access=u:check_access(Uid, Feature),
	case Access of
		Deny ->
			case {Action,Object,Type} of
				{read,thread,_} -> Allow;
				{read,board,_} -> Allow;
				{write,thread,default} -> Allow;
				{write,thread,request} -> Allow;
				{write,board,default} -> Allow;
				_ -> Deny
			end;
		_ -> Access
	end.

ca({Object, OAction, {OType, _Data}, Parent}) ->
	% User = u:get(),
	% IsAdmin = u:is_admin(User),
	% IsTemp = u:is_temp(User),
	% Uid = u:id(User),
	
	Purpose = element(1,Parent),
	Action = case OAction of delete -> moderate; _ -> OAction end,
	
	Section = element(2,Parent),
	Type = OType,
	
	Feature = {Action,{Purpose,Section,Object,Type}},
	% {Value,Start,Expire}=
	get_initial_access(1,Feature).
	
% модель защиты это всегда эффективная функция проверки check к двум множествам (списком доступа объекта и списком возможностей пользователя)