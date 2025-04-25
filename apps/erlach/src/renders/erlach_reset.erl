-module(erlach_reset).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").
-include_lib("eauth/include/eauth_user.hrl").

title(#st{}) -> <<"Resetting password – Erlach"/utf8>>.
urn() -> <<"reset">>.


init(#route{level=?UNDEF}=Route) ->
    wf:info(?M,"init ~p",[self()]),
    {redirect,erlach_qs:mp(main)};
init(#route{level=Key}=_R) -> % reset passwd by link
    wf:info(?M,"init: resetting passwd +KEY ~p ~p",[Key, self()]),
    case erlach_signin:is_allow_signin() of
        false -> wf:warning(?M,"Fail password resetting: not allow signin, key: ~p ~p",[Key, self()]);
        true ->
            case eauth:activate(Key) of
                {error,activation_not_found} -> skip;
                {ok,{Mail,PasswordHash,Salt}} ->
                    case kvs:index(user,email,Mail) of
                        [] -> wf:warning(?M,"Fail password resetting: user mail <~p> not found, key: ~p ~p",[Mail,Key,self()]);
                        [#user{id=Uid,email=Mail}=User] ->
                            {ok,#user{}}=erlach_feeds:update(User,fun(#user{}=U) ->
                                {ok,U#user{password=PasswordHash,salt=Salt}}
                            end),
                            wf:user(Uid),
                            erlach_markup:highlight(succ),
                            wf:info(?M,"Password updated for user ~p",[Uid])
                    end
            end
    end,
    {redirect,erlach_qs:mp(main)}.

finalize(#st{}) -> wf:info(?M,"finalize ~p",[self()]).
terminate() -> wf:info(?M,"terminate ~p",[self()]).

render(content=Panel,#st{}=S) -> [].

event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).