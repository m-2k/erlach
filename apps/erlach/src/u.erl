-module(u).
-author('andy').
-compile(export_all).

-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/feed.hrl").
-include_lib("db/include/user.hrl").
-include("erlach.hrl").

-define(SESSION_USER, session_user).

new_user(SessionID) ->
    new_user(SessionID, calendar:local_time()).
new_user(SessionID, Created) ->
    U = #user3{id={?T,SessionID},created=Created},
    % ets:insert(cookies,{{SessionID,?USER},U}), % speed 5-6 ms
    wf:user(U),                                  % speed 6-9 ms
    wf:wire(#transfer{events={server,{temp_user_created,U}}}),
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

new_password() -> list_to_binary(utils:to_hex(crypto:rand_bytes(8))).

check_access(Feature) -> check_access(u:id(), Feature).
check_access(U, Feature) -> % User or Uid
    Uid = u:to_id(U),
    kvs_acl:check([ {{user,Uid},Feature} ]).
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
    case u:check_access(u:get(), Feature) of
        allow -> Fun();
        _ -> Default
    end.
restricted_call(Fun, Feature) -> restricted_call(Fun, Feature, undefined).
is_admin() -> is_admin(u:get()).
is_admin(UserOrId) ->
    case kvs_acl:check([{{user,u:to_id(UserOrId)}, {feature,admin}}]) of allow -> true; _ -> false end.

store_temp_user() ->
    User = u:get(),
    {ok, Stored} = kvs:add(User#user3{id=kvs:next_id(user3, 1)}),
    u:put(Stored),
    Stored.

clone(Id) ->
    {ok,U1} = kvs:get(user3,Id),
    U2 = setelement(19,U1,wf:to_binary(wf:temp_id())),
    U3 = setelement(30,U2,[{twitter,wf:to_binary(wf:temp_id())}|undefined]),
    kvs:put(U3).

names() -> names(u:get()).
names(User) ->
    case kvs:get(feed, {name, u:id(User)}) of
        {ok, Fn} ->
            NameList = kvs:traversal(name, Fn#feed.top, Fn#feed.entries_count, #iterator.prev);
        _ -> []
    end.
put_name() ->
    case u:is_temp() of
        false ->
            % storing
            {ok, result};
        _ -> {error, prevent_for_temp_user}
    end.
