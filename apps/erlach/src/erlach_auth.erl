-module(erlach_auth).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").

auth(#auth{logout=true}=A) ->
    wf:warning(?M,"Logout",[]),
    wf:clear_user(),
    case spa:st() of
        #st{route=R}=S -> ?SPA:navigate(R); % refresh page
        _ -> wf:warning(?M,"State not initialized",[])
    end;
auth(#auth{user=User,passwd=Passwd}=A) ->
    timer:sleep(1000), % DoS preventing
    case check(User,Passwd) of
        true ->
            case spa:st() of
                #st{route=R}=S ->
                    wf:warning(?M,"Auth allowed as: ~p",[User]),
                    wf:user(User),
                    ?SPA:navigate(R); % refresh page
                _ -> wf:warning(?M,"State not initialized",[])
            end;
        false -> wf:warning(?M,"Auth failed: ~p",[A])
    end.
    
check(User,Passwd) when is_atom(User) and is_binary(Passwd) ->
    Hash=hash(Passwd),
    case application:get_env(erlach,auth) of {ok,{User,Hash}} -> true; _ -> false end;
check(_,_) -> false.
    
    
    
request(User,Passwd) ->
    Event = #ev{name=event, module=?SPA, msg=#auth{user=User,passwd=Passwd}, trigger=""},
    erlang:list_to_binary([ <<"ws.send(enc(tuple(atom('">>,
        nitro:to_binary(application:get_env(n2o,event,pickle)),<<"'),bin(''),bin('">>,
        nitro:pickle(Event),<<"'),[])));">> ]).

% For output use rp/1.
hash(Pass) -> crypto:hash(sha512,Pass).
set(User,Hash) -> application:set_env(erlach,auth,{User,Hash}).
clear() -> application:unset_env(erlach,auth).
state() -> application:get_env(erlach,auth,?UNDEF).

access() ->
    case wf:user() of
        su -> full;
        _ -> ?UNDEF
    end.