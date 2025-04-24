-module(erlach_services).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").

title(#st{thread=#post{}=T}) -> <<(topic(T))/binary," – Erlach Services"/utf8>>.
urn() -> ?UNDEF.

init(#route{}=Route) ->
    case erlach_thread:init(Route) of
        {ok,#st{}=S} -> {ok,S#st{services=comments}};
        Redirect -> Redirect
    end.
finalize(#st{thread=#post{id=Tid},route=#route{option=highlight}}) ->
    wf:info(?M,"finalize with option (~p)",[self()]),
    scroll_to(Tid);
finalize(#st{}) ->
    wf:info(?M,"finalize (~p)",[self()]),
    % wf:wire("console.log(window.parent);"),
    ok.
terminate() ->
    wf:info(?M,"TERMINATE ~p",[self()]),
    #st{thread=#post{id=Tid}}=spa:st(),
    wf:unreg({thread,Tid}),
    wf:info(?M,"TERMINATE ~p OK",[self()]).

topic(#post{urn=Urn,name_escaped=Name}) ->
    case spa_utils:strip(Name) of
        <<>> -> <<"thread#",Urn/binary>>;
        Text -> Text
    end.

scroll_to(Pid) ->
    case spa:st() of #st{thread=#post{id=Pid}} -> wf:wire(wf:f("scrollToTop();")); _ -> skip end, % if thread
    wf:wire(wf:f("scrollToPost('~s');",[spa:id({post,Pid})])).

render(content=Panel,#st{board=#board{}=B,thread=#post{id=Tid}=T}=S) ->
    PostList=kvs:entries(kvs:get(feed,{post,Tid}),post,undefined),    
    Container= <<"posts">>,
    Instant=erlach_utils:partial(#view{target=post,element=Container,queue=PostList,option=S,partial=true}),
    #panel{id=Panel,body=[
        #panel{id=Container,body=[ erlach_thread:render(P,#hes{board=B,thread=T},S) || P <- Instant ]},
        #panel{id= <<"posts-new">>},
        erlach_thread:render('posts-new-controls',S),
        case erlach_thread:is_read_only(S) of true -> []; false -> #media_input{id=input,target=post,disabled=true} end
    ]}.

event(#pubsub{target=content}=E) -> erlach_thread:event(E);
event(#pubsub{target=image}=E) -> erlach_thread:event(E);
event(#view{target=post}=E) -> erlach_thread:event(E);
event(#render_event{target=input}=E) -> erlach_thread:event(E);

event(#add{}=E) -> erlach_thread:event(E);

event({server,_}=E) -> erlach_thread:event(E);
event(#ftp{}=E) -> erlach_thread:event(E);

event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).
