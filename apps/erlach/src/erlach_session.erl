-module(erlach_session).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-export(?SESSION_API).
-compile(export_all).
%-record(state, {unique, node}).

-include("erlach.hrl").
-include_lib("db/include/user.hrl").

init(State, Ctx) ->
    Request = case wf:cookie_req(session_cookie_name(), Ctx#cx.req) of
        undefined ->
            {new, new_cookie_value()};
        SessionId ->
            case lookup_ets({SessionId,?AUTH}) of
                undefined ->
					{new, new_cookie_value()};
                {{SessionId, ?AUTH},#cookie{expire=Expire}} = C ->
                    case expired(Expire) of
                        true -> {new, new_cookie_value()};
                        false -> {exist, C}
                    end;
                _ ->
                    wf:error(?MODULE, "Cookie Error", []),
                    {new, new_cookie_value()}
			end
	end,
    {{Id, Key}, Value} = case Request of
        {new, SID} ->
            {Now_GMT, Expire_GMT} = expire(),
            Cookie = {{SID,?AUTH},#cookie{status=new,path=?COOKIE_PATH,ttl=ttl(),issued=Now_GMT,expire=Expire_GMT}},
			wf:info(?MODULE, "New cookie: ~p", [Cookie]),
			% User = #user3{id={?T,SID},created=Now_GMT},
			% wf:info(?MODULE, "New user: ~p", [User]),
			ets:insert(cookies,Cookie),
			% ets:insert(cookies,{{SID,?USER},User}),
            Cookie;
        {exist, {{SID, _K}, #cookie{issued=Issued}} = _Cookie} ->
            {_Now_GMT_2, Expire_GMT_2} = expire(),
            Cookie2 = {{SID,?AUTH},#cookie{status=actual,path=?COOKIE_PATH,ttl=ttl(),issued=Issued,expire=Expire_GMT_2}},
			wf:info(?MODULE, "Cookie exist: ~p", [Cookie2]),
			% {{SID, ?USER}, User} = lookup_ets({SID,?USER}),
			% wf:info(?MODULE, "User exist: ~p", [User]),
            ets:insert(cookies,Cookie2),
            Cookie2
    end,
    wf:info(?MODULE,"Current cookie SID: ~p",[Id]),
    {ok, State, Ctx#cx{session={{Id, Key}, Value}}}.

finish(State, Ctx) -> 
    wf:info(?MODULE,"Finish Cookie Set ~p",[State]),
    NewReq = case Ctx#cx.session of
         {{Session,_Key},#cookie{path=Path,ttl=TTL}} -> 
              wf:cookie_req(session_cookie_name(),Session,Path,TTL,Ctx#cx.req);
         _ -> Ctx#cx.req end,
    {ok, State, Ctx#cx{req=NewReq}}.
	
ttl() -> 60*60*24. % 1 day
now_time() ->
    calendar:local_time().
expire() -> expire(ttl()).
expire(TTL) ->
    Now_GMT = now_time(),
    Now_GMT_In_Seconds = calendar:datetime_to_gregorian_seconds(Now_GMT),
    Expire_GMT = calendar:gregorian_seconds_to_datetime(Now_GMT_In_Seconds + TTL),
    {Now_GMT, Expire_GMT}.
cookie_expire(Expire_GMT) ->
    httpd_util:rfc1123_date(Expire_GMT).
expired(Expire_GMT) ->
    Now_GMT_In_Seconds = calendar:datetime_to_gregorian_seconds(now_time()),
    Expire_GMT_In_Seconds = calendar:datetime_to_gregorian_seconds(Expire_GMT),
    Now_GMT_In_Seconds > Expire_GMT_In_Seconds.

lookup_ets(Key) ->
    Res = ets:lookup(cookies,Key),
    wf:info(?MODULE,"Lookup ETS: ~p",[{Res,Key}]),
    case Res of 
         [] -> undefined;
         [Value] -> Value;
         Values -> Values end.

clear() -> clear(session_id()).
clear(Session) ->
    [ ets:delete(cookies,X) || X <- ets:select(cookies,
        ets:fun2ms(fun(A) when (element(1,element(1,A)) == Session) -> element(1,A) end)) ].

session_id() -> {{Id, _Key}, _Value} = ?CTX#cx.session, Id. % Speed fix:  -20 ms
% session_id() -> wf:cookie_req(session_cookie_name(),?REQ).

	
new_cookie_value() -> base64:encode(erlang:md5(term_to_binary({now(), make_ref()}))).
new_state() -> #state{unique=new_cookie_value()}.
session_cookie_name() -> wf:config(n2o, session_cookie_name, <<"n2o-sid">>).
set_value(Key, Value) -> ets:insert(cookies,{{session_id(),Key},Value}), Value.
get_value(Key, DefaultValue) ->
	
    Res = case lookup_ets({session_id(),Key}) of
               undefined -> DefaultValue;
               {_,Value} -> Value end,
    wf:info(?MODULE,"Session Lookup Key ~p Value ~p",[Key,Res]),
    Res.



% temp_key() -> crypto:rand_bytes(4).
% temp_key() -> unique:random().
% temp_key() -> {_,_,C} = now(), list_to_binary(wf:f("~.36b", [C])).
% temp_key([Num]) -> temp_key(Num);
% temp_key([Num|T]) -> temp_key(Num) ++ temp_key(T);
% temp_key(Num) -> case Num of _ when Num > 1295 -> "~.36B"; _ -> "~.36b" end.
% temp_key() ->
% 	<<A1,A2,A3,A4,A5,B1,B2,B3,B4,B5,C1,C2,C3,C4,C5,D1,D2,D3,D4,D5>> = crypto:rand_bytes(20),
% 	{A,B,C,D} = {A1+A2+A3+A4+A5,B1+B2+B3+B4+B5,C1+C2+C3+C4+C5,D1+D2+D3+D4+D5},
% 	S = temp_key([A,B,C,D]),
% 	wf:to_binary(wf:f(S, [A,B,C,D])).

% temp_key(<<>>,S,N) -> {S,N};
% temp_key(<<A,B,C,D,E,F,G,H,I,K,Tail/binary>>,S,N) ->
% 	Num = A+B+C+D+E+F+G+H+I+K,
% 	{Sx, Nx} = case Num of
% 		_ when Num < 36 -> {"0~.36B",Num};
% 		_ when Num > 1310 -> {"~.36B",Num-1275};
% 		_ when Num > 1275 -> {"0~.36B",Num-1275};
% 		_ -> {"~.36b",Num}
% 	end,
% 	temp_key(Tail,Sx ++ S, [Nx|N]).
%
% temp_key(Length) when is_number(Length)->
% 	{S,N} = temp_key(crypto:rand_bytes(Length*10),[],[]),
% 	wf:to_binary(wf:f(S,N)).
% temp_key() -> temp_key(2).
% set_param(Value) -> Key = temp_key(), set_value(Key, Value), Key.
% get_param(Key) -> get_value(Key, undefined).
% erase_param(Key) ->
% 	Value = get_value(Key, undefined),
% 	ets:delete(cookies, {session_id(),Key}),
% 	Value.

set_param(DestanationModule,Value) ->
	{Now_GMT, Expire_GMT} = expire(?SESSION_REDIRECT_TIME_LIMIT),
	V = {Value, Expire_GMT},
	set_value(list_to_binary(atom_to_list(DestanationModule)), V), V.
get_param(Module) ->
	case get_value(list_to_binary(atom_to_list(Module)), undefined) of
		undefined -> undefined;
		{Value, Expire_GMT} -> case expired(Expire_GMT) of false -> Value; _ -> undefined end
	end.
erase_param(Module) ->
	Value = get_param(Module),
	ets:delete(cookies, {session_id(),list_to_binary(atom_to_list(Module))}),
	Value.