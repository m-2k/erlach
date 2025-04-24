-module(erlach_services).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").

title(#st{thread=#post{}=T}) -> <<(topic(T))/binary," – Erlach Services"/utf8>>.
urn() -> ?URN_PAGE_DYNAMIC.

init(#route{}=Route) ->
    case erlach_thread:init(Route) of
        {ok,#st{}=S} -> {ok,S#st{services=comments}};
        Redirect -> Redirect
    end.

finalize(S) -> erlach_thread:finalize(S).
terminate() -> erlach_thread:terminate().

topic(#post{urn=Urn,name_escaped=Name}) ->
    case spa_utils:strip(Name) of
        <<>> -> <<"thread#",Urn/binary>>;
        Text -> Text
    end.

render(content,S) -> erlach_thread:render(content,S).

event(#pubsub{target=content}=E) -> erlach_thread:event(E);
event(#pubsub{target=image}=E) -> erlach_thread:event(E);
event(#view{target=post}=E) -> erlach_thread:event(E);
event(#render_event{target=input}=E) -> erlach_thread:event(E);

event(#add{}=E) -> erlach_thread:event(E);

event({server,_}=E) -> erlach_thread:event(E);
event(#ftp{}=E) -> erlach_thread:event(E);

event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).
