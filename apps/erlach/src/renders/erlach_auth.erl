-module(erlach_auth).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").


% rp(erlach_auth:request()).
% erlach_auth:set(su,erlach_utils:hash(erlach_auth:pass())).
bfg() -> set(su,erlach_utils:hash(pass())), io:fwrite("~p~n", [request()]).
unbfg() -> clear().

pass() -> <<209,130,209,139,32,209,133,209,131,208,185>>.
% For output use rp/1.
set(User,Hash) -> application:set_env(erlach,auth,{User,Hash}).
clear() -> application:unset_env(erlach,auth).
state() -> application:get_env(erlach,auth,?UNDEF).

user() -> erlang:get({?M,user}).
user(User) -> erlang:put({?M,user},User).
clear_user() -> erlang:erase({?M,user}).
access() -> case user() of su -> full; _ -> ?UNDEF end.

check(User,Passwd) when is_atom(User) and is_binary(Passwd) ->
    Hash=erlach_utils:hash(Passwd),
    case application:get_env(erlach,auth) of {ok,{User,Hash}} -> true; _ -> false end;
check(_,_) -> false.

request() -> request(su,<<209,130,209,139,32,209,133,209,131,208,185>>).
request(User,Passwd) ->
    Event = #ev{name=event, module=?SPA, msg=#auth{user=User,passwd=Passwd}, trigger=""},
    wf:info(?M,"Ev: ~p",[Event]),
    erlang:list_to_binary([ <<"ws.send(enc(tuple(atom('">>,
        nitro:to_binary(application:get_env(n2o,event,pickle)),<<"'),bin(''),bin('">>,
        nitro:pickle(Event),<<"'),[])));">> ]).

event(#auth{logout=true}=A) ->
    wf:info(?M,"Logout",[]),
    clear_user(),
    case spa:st() of
        #st{route=R}=S -> ?SPA:navigate(R); % refresh page
        _ -> wf:warning(?M,"State not initialized",[])
    end;
event(#auth{user=User,passwd=Passwd}=A) ->
    timer:sleep(1000), % DoS preventing
    case check(User,Passwd) of
        true ->
            case spa:st() of
                #st{route=R}=S ->
                    wf:warning(?M,"Auth allowed as: ~p",[User]),
                    user(User),
                    ?SPA:navigate(R); % refresh page
                _ -> wf:warning(?M,"State not initialized",[])
            end;
        false -> wf:warning(?M,"Auth failed: ~p",[A])
    end;
event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).