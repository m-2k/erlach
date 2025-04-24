-module(erlach_thread).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").
-include("erlach_image.hrl").

title(#st{thread=#post{}=T}) -> <<(topic(T))/binary," – Erlach"/utf8>>.
urn() -> ?UNDEF.

init(#route{board=Burn,thread=Turn,post=Purn}=Route) ->
    wf:info(?M,"init ~p ~p ~p",[Burn,Turn,Purn]),
    case {kvs:index(board,urn,Burn),kvs:get(post,erlach_qs:urn_to_id(Turn))} of
        {[#board{id=Bid}=B],{ok,#post{type=thread,feed_id={thread,Bid},id=Tid}=T}} ->
            wf:reg({board,Bid}),
            wf:reg({thread,Tid}),
            P=case Purn of
                ?UNDEF -> ?UNDEF;
                _ ->
                    case kvs:get(post,erlach_qs:urn_to_id(Purn)) of
                        {ok,#post{type=post,feed_id={post,Tid}}=Post} -> Post;
                        _ -> ?UNDEF
                    end
            end,
            {ok,#st{route=Route,action=view,board=B,thread=T,post=P}};
        _ -> {redirect,erlach_qs:mp(main)}
    end.
finalize(#st{thread=#post{id=Tid},route=#route{option=highlight}}) ->
    wf:info(?M,"finalize with option (~p)",[self()]),
    scroll_to(Tid);
finalize(#st{}) ->
    wf:info(?M,"finalize (~p)",[self()]),
    ok.
terminate() ->
    wf:info(?M,"TERMINATE ~p",[self()]),
    #st{thread=#post{id=Tid},board=#board{id=Bid}}=spa:st(),
    wf:unreg({board,Bid}),
    wf:unreg({thread,Tid}),
    wf:info(?M,"TERMINATE ~p OK",[self()]).

topic(#post{urn=Urn,name_escaped=Name}) ->
    case spa_utils:strip(Name) of
        <<>> -> <<"thread#",Urn/binary>>;
        Text -> Text
    end.

up_state(Pid) ->
    wf:info(?M,"Up state ~p",[Pid]),
    case kvs:get(post,Pid) of {ok,P} -> spa:st((spa:st())#st{post=P}), erlach_qs:history_update(); _ -> skip end.
scroll_to(Pid) ->
    wf:info(?M,"Scroll to ~p",[Pid]),
    up_state(Pid),
    case spa:st() of #st{thread=#post{id=Pid}} -> wf:wire(wf:f("scrollToTop();")); _ -> skip end, % if thread
    wf:wire(wf:f("scrollToPost('~s');",[spa:id({post,Pid})])).

render(content=Panel,#st{thread=#post{id=Tid}=T,board=B}=S) ->
    T1=erlang:system_time(),
    PostList=kvs:entries(kvs:get(feed,{post,Tid}),post,undefined),
    T2=erlang:system_time(),
    wf:info(?M,"TIMINGS DB ops in ~p ms",[(T2-T1)/1000000]),
    Container= <<"posts">>,
    Instant=erlach_utils:partial(#view{target=post,element=Container,queue=PostList,option=S,partial=true}),
    #panel{id=Panel,body=[
        #panel{class= <<"content-title">>,body=topic(T)},
        render(T,#hes{board=B},S),
        #panel{id=Container,body=[ render(P,#hes{thread=T,board=B},S) || P <- Instant ]},
        #panel{id= <<"posts-new">>},
        render('posts-new-controls',S),
        case is_read_only(S) of true -> []; false -> #media_input{id=input,target=post,disabled=true} end
    ]};
render('posts-new-controls'=Panel,#st{}) ->
    Hidden=(erlach_utils:rst())#rst.hidden_elements,
    wf:info(?M,"posts-new-controls: ~p ~p",[Hidden,self()]),
    E= <<"posts-new">>,
    
    #panel{id=Panel,class= <<"center">>,
        body=case Hidden of
            [] -> [];
            _ -> [ #button{class= <<"control-show">>,postback=#view{target=post,option=expand,element=E},
                body=[<<"Show ">>,wf:to_binary(length(Hidden)),<<" new">>]},
                #button{postback=#view{target=post,option=auto,element=E},body= <<"Auto">>}]
        end }.

render({imagePanel,?UNDEF},#hes{},#st{}) -> [];
render({imagePanel,Aid},#hes{board=B},#st{}=S) ->
    Render=spa:render(S),
    Fields=fun(#attachment{board=AB,thread=AT,post=AP}) ->
        case {Render,AB,AT,AP,B} of
            {?STREAM,Bid,Tid,?UNDEF,#board{id=Bid,urn=Burn}} -> [{<<"data-link-related">>,erlach_qs:ml({thread,Burn,Tid})}];
            {?STREAM,Bid,Tid,Pid,#board{id=Bid,urn=Burn}} -> [{<<"data-link-related">>,erlach_qs:ml({post,Burn,Tid,Pid})}];
            {?SERVICES,Bid,Tid,?UNDEF,#board{id=Bid,urn=Burn}} -> [{<<"data-open-external">>,erlach_qs:ml({thread,Burn,Tid})}];
            {?SERVICES,Bid,Tid,Pid,#board{id=Bid,urn=Burn}} -> [{<<"data-open-external">>,erlach_qs:ml({post,Burn,Tid,Pid})}];
            _ -> []
        end ++ case Render of ?STREAM -> [{<<"data-stream">>,<<"true">>}]; _ -> [] end
    end,
    Class=fun(View) -> case proplists:get_value(image,View) of thumbnail -> "thumbnail"; _ -> [] end end,
    
    case kvs:get(attachment,Aid) of
        {ok,#attachment{path=?UNDEF,name=UploadName,view=View,width=JsWidth,height=JsHeight}=A} -> % deferred
            element_media_input:image_panel(image_panel_id(UploadName),Class(View),true,JsWidth,JsHeight,
                Fields(A),?UNDEF,?UNDEF);
        {ok,#attachment{path=Path,name=Name,view=View,width=Width,height=Height,info=Info}=A} ->
            element_media_input:image_panel(image_panel_id(Name),Class(View),true,Width,Height,
                Fields(A),url(A),erlach_image:type(Info));
        _ -> wf:warning(?M,"Attachment db record not found ~p",[Aid]), []
    end;
render({'post-header',#post{type=thread,id=Tid}=T},#hes{}=Hes,#st{}=S) ->
    #panel{id=spa:id({'post-header',Tid}),body=#panel{class= <<"post-header">>,body=[
        render({'post-manage',T},Hes,S)
        ]}};
render({'post-header',#post{type=post}=P},#hes{}=Hes,#st{}=S) ->
    render({'post-manage',P},Hes,S);
render({'post-manage',#post{type=Type,id=Id}=P},#hes{},#st{access=full}) ->
    #span{class= <<"manage">>,body=#button{class=warn,body= <<"Remove">>,postback=#delete{target=Type,value=Id}}};
render({'post-manage',#post{}},#hes{},#st{}) ->
    [];
render(message,#hes{post=#post{message=Raw,message_escaped=Esc}=P}=Hes,#st{}=S) ->
    erlach_markup:html(P,Hes,S);
render(reply,#hes{post=P,board=B,thread=T},#st{}) ->    
    {Meta,X}=case P of #post{} -> {{post,B,T,P},P}; _ -> {{thread,B,T},T} end,
    #a{class= <<"l lemon reply-action">>,body=?RPL_A,
        href=erlach_qs:ml(Meta),
        postback=erlach_qs:mp(Meta),
        % postback=#render_event{target=page_navigation,event={element(1,X),element(2,X)}},
        onclick=wf:jse(wf:f("append(qs('#input .post-message'),'>>~s ');",[erlach_qs:urn(X)])) };
render(links,#hes{post=#post{id=Pid,links=Links}},#st{}=S) ->
    Panel=spa:id({{post,answers},Pid}),
    LinkElements=spa_utils:map_join(fun(LinkId) ->
        case kvs:get(post,LinkId) of
            {ok,L} -> erlach_utils:link(#a{class=l},L,S);
            _ -> #span{body=[?RPL,erlach_qs:id_to_urn(LinkId)]}
        end
        end,#span{body= <<", ">>},lists:reverse(Links)),
    case LinkElements of
        [] -> #panel{id=Panel,body=[]}; % for updating
        _ -> #panel{id=Panel,class= <<"post-answers">>,body=[#span{body= <<"Replies: ">>},LinkElements]}
    end;
render(#post{type=thread,id=Tid,urn=Urn,links=Links,message_escaped=Message,image=Image}=T,#hes{board=B}=Hes,#st{}=S) ->
    Panel=spa:id(T),
    #panel{id=Panel,class= <<"post head">>,
        body=#panel{class= <<"post-content">>,body=[
            render({imagePanel,Image},Hes,S),
            #panel{class= <<"post-message">>,body=[
                render({'post-header',T},Hes,S),
                render(message,#hes{post=T},S),
                render(reply,Hes#hes{thread=T},S)
            ]},
            render(links,#hes{post=T},S)
    ]}};
render(#post{type=post,urn=Urn,image=Image,sage=Sage}=P,#hes{board=B,thread=T}=Hes,#st{}=S) ->
    Panel=spa:id(P),
    Hes2=Hes#hes{post=P},
    % ImagePanel=spa:id({image,Pid}),
    #panel{id=Panel,class=[post,case Sage of true -> sage; _ -> [] end],
        body=#panel{class= <<"post-content">>,body=[
            render({imagePanel,Image},Hes,S),
            #panel{class= <<"post-message">>,body=[
                render({'post-header',P},Hes2,S),
                render(message,Hes2,S),
                render(reply,Hes2,S)
            ]},
            render(links,#hes{post=P},S)
    ]}}.

event(#add{target=Target,forms=[TopicOrSage,Input,Selector]}) when Target =:= thread orelse Target =:= post ->
    wf:info(?M,"Adding post ~p",[Target]),
    M=spa_utils:strip(wf:q(Input)),
    {T,IsSage}=case {Target,wf:q(TopicOrSage)} of
        {thread,X}        -> {spa_utils:strip(X),false};
        {post,<<"true">>} -> {<<>>,true};
        {post,_}          -> {<<>>,false}
    end,
    wf:info(?M,"Sage: ~p",[IsSage]),
    View=case wf:q(Selector) of
        <<"on">> ->
            wf:wire("qs('#input .post-image').classList.add('thumbnail');"), [{image,thumbnail}];
        _ -> [{image,fullwidth}] end,
    
    LIMT=wf:config(erlach,topic_char_limit,100),
    LIMM=wf:config(erlach,post_char_limit,5000),
    
    Warning=fun(Text) ->
        spa:warning(Text),
        wf:wire(["var x=qs('#input .store-button'); x && x.removeAttribute('disabled');"])
    end,

    % #rst.image for overwriting image in post
    case {Target,size(T),size(M),erlach_utils:rst()} of
        {thread,SZT,_,_} when SZT > LIMT -> Warning(<<"Very long topic, bro">>);
        {_,_,SZM,_} when SZM > LIMM -> Warning(<<"Very long message, LOL">>);
        {_,_,0,#rst{image_ftp=?UNDEF}} -> Warning(<<"Nothing publish">>);
        {_,SZT,SZM,Rst} ->
            #st{board=#board{id=Bid,feed_id={board,Party}}=StateBoard,thread=StateThread}=S=spa:st(),
            Pid=kvs:next_id(post,1),
            Tid=case Target of post -> StateThread#post.id; thread -> Pid end,
            
            EscapedT=case Target of thread -> wf:to_binary(wf:html_encode(wf:to_list(T))); post -> <<>> end,
            RawM=wf:to_list(spa_utils:squish(M)),
            EscapedM=wf:to_binary(wf:html_encode(RawM)),
            
            Image=case Rst#rst.image_ftp of
                #ftp{filename=FileName,size=_Size,meta=Meta}=Ftp ->
                    Aid=kvs:next_id(attachment,1),
                    
                    #rst{image_stack=Stack}=Rst=erlach_utils:rst(),
                    erlach_utils:rst(Rst#rst{image_ftp=?UNDEF}),
                    
                    {Width,Height,ConvertedPath,InfoA,InfoB}=case Meta of
                        {process,_Key,JsW,JsH} -> % wait converting
                            UpdateFun=fun(E) -> E#entry{meta=Aid} end,
                            erlach_image:update(FileName,default,UpdateFun),
                            {JsW,JsH,?UNDEF,?UNDEF,?UNDEF};
                        {converted,_Key,_Storage,Path,_FileNameExt,IiA,IiB} -> % already converted
                            {erlach_image:w(IiB),erlach_image:h(IiB),Path,IiA,IiB}
                    end,
                    A=#attachment{id=Aid,
                        feed_id={attachment,Bid},
                        party=Party,
                        board=Bid,
                        thread=Tid,
                        post=case Target of thread -> ?UNDEF; post -> Pid end,
                        
                        created=erlang:timestamp(),
                        name=FileName,
                        path=ConvertedPath,
                        type=erlach_image:type(InfoB),
                        info=InfoB,
                        original_info=InfoA,
                        view=View,
                        width=Width,
                        height=Height},
                    {ok,_}=erlach_feeds:append(A),
                    erlach_stat:incr(attachment,{Bid,Tid,A}),
                    Aid;
                _ ->
                    ?UNDEF
            end,
            wf:info(?M,"Adding post ~p",[1]),
            
            case re:run(RawM,erlach_markup:re_compiled(post_link),[global,{capture,[1],binary}]) of
                nomatch -> [];
                {match,Match} -> % {match,[[<<"234">>],[<<"23434">>],[<<"23434343">>]]}
                    lists:foreach(fun([Idx]) ->
                        FunUpdate=fun(#post{links=Links}=P) -> {ok,P#post{links=lists:usort([Pid | Links])}} end,
                        case kvs:get(post,erlach_qs:urn_to_id(Idx)) of
                            {ok,P} ->
                                case erlach_feeds:update(P,FunUpdate) of
                                    {ok,#post{type=thread,id=I,feed_id={thread,F}}=P2} ->
                                            wf:send({board,F},
                                                {server,#pubsub{target=content,action=put,data=P2,from=self()}}),
                                            wf:send({subscription,thread,I},
                                                {server,#pubsub{render=?SUB,target=subscription,action=put,element=P2,
                                                    data=(spa:st())#st{level=self()},from=self()}});
                                    {ok,#post{type=post,feed_id={post,F}}=P2} ->
                                        wf:send({thread,F},
                                            {server,#pubsub{target=content,action=put,data=P2,from=self()}}),
                                        wf:send({subscription,thread,F},
                                            {server,#pubsub{render=?SUB,target=subscription,action=put,element=P2,
                                                data=(spa:st())#st{level=self()},from=self()}});
                                    _ -> skip
                                end;
                            _ -> skip
                        end
                    end,lists:usort(Match))
            end,
            Addition=case Target of
                thread ->
                    #post{type=thread,id=Tid,feed_id={thread,Bid},
                        created=erlang:timestamp(),urn=erlach_qs:id_to_urn(Pid),
                        party=Party,board=Bid,thread=?UNDEF,
                        name=T,name_escaped=EscapedT,message=M,message_escaped=EscapedM,image=Image};
                post ->
                    #post{type=post,id=Pid,feed_id={post,Tid},created=erlang:timestamp(),
                        party=Party,board=Bid,thread=Tid,
                        urn=erlach_qs:id_to_urn(Pid),message=M,message_escaped=EscapedM,sage=IsSage,image=Image}
            end,
            
            {ok,Post}=erlach_feeds:append(Addition),
            case Target of thread -> erlach_stat:incr(thread,{Bid,Post}); post -> erlach_stat:incr(post,{Bid,Tid,Post}) end,
            erlach_subscription:put(Post,S#st{level=self()}),
            
            case {Target,spa:render(S)} of
                {post,?SERVICES} when is_integer(Image) ->
                    wf:wire(["bindExternalUri('",erlach_qs:ml({post,erlach_qs:urn(StateBoard),Tid,Pid}),"');"]);
                _ -> skip
            end,
            
            wf:reg({subscription,thread,Tid}),
            {ok,XT}=kvs:get(post,Tid),
            wf:send({subscription,thread,Tid},{server,#pubsub{render=?SUB,target=subscription,action=put,element=XT,
                data=S#st{level=self()},from=self()}}),
            
            wf:wire("textErase();"), % clean
            wf:wire(#jq{target=Input,method=["removeAttribute"],args=["'contentEditable'"]}),
            case Target of thread ->
                wf:wire(#jq{target=TopicOrSage,method=["removeAttribute"],args=["'contentEditable'"]}); post -> skip end,
            wf:wire("qs('#input .input-selector').remove();"),
            wf:wire("qs('#input label').removeAttribute('for');"),
            wf:wire("var x=qs('#input .image-manage'); x.parentNode.removeChild(x);"),
            wf:wire("qs('.input-controls').remove();"),
            wf:wire(#jq{target=input,property=className,right="post"}),
            case Target of
                thread ->
                    wf:wire(#jq{target="qs('#input .post-message')",property=outerHTML,args=simple,format="'~s'",
                        right=[#panel{class= <<"post-message">>,
                            body=render(message,spa:setoption(limit,wf:config(erlach,board_char_limit,300),#hes{post=Post}),S)}]});
                post ->
                    wf:wire(#jq{target="qs('#input .post-message')",property=outerHTML,args=simple,format="'~s'",
                        right=[#panel{class= <<"post-message">>,body=[
                                render({'post-header',Post},#hes{},S),
                                render(message,#hes{post=Post},S),
                                render(reply,#hes{post=Post,thread=StateThread,board=StateBoard},S) ]},
                            render(links,#hes{post=Post},S)
                        ]})
            end,
            wf:wire(#jq{target=input,property=id,right=spa:id(Post)}),
            % ImageContainer=spa:id({image,Pid}),
            % wf:wire(#jq{target= <<"input-image">>,property=id,right=ImageContainer}),
            case Target of
                thread ->
                    wf:send({board,Bid},{server,#pubsub{target=content,action=add,data=Post,from=self()}}),
                    wf:insert_top(threads,erlach_board:render(controls,spa:st())); % only for erlach_board
                post ->
                    {ok,XT2}=kvs:get(post,Tid),
                    wf:send({board,Bid},{server,#pubsub{target=content,action=update,data=XT2,from=self()}}), % update post count
                    wf:send({thread,Tid},{server,#pubsub{target=content,action=add,data=Post,from=self()}})
            end
    end, [];
event(#pubsub{target=content,action=add,data=#post{type=post,feed_id={post,Tid},sage=Sage}=P,from=Proc}) ->
    wf:info(?M,"RECEIVE add #post{} ~p",[self()]),
    
    % bump limit check
    #st{view=View,board=B,thread=T}=S=spa:st(),
    ReadOnly=is_read_only(S),
    
    AutoExpand=case lists:keyfind('auto-expand', 1, View) of {'auto-expand',true} -> true; _ -> false end,
    PostsNew=#panel{id= <<"posts-new">>,class=case AutoExpand of true -> <<"auto">>; false -> <<>> end},
    
    case self() of
        Proc ->
            erlach_utils:hidden(), % clear

            wf:wire(#multi{actions=[
                % case Sage of true -> #wire{actions=["qi('",spa:id(P),"').classList.add('sage');"]}; _ -> [] end,
                case Sage of true -> #jq{target=spa:id(P),method=["classList.add"],args=["'sage'"]}; _ -> [] end,
                #jq{target= <<"posts-new">>,method=["removeAttribute"],args=["'id'"]},
                #wire{actions="qi('posts-new-controls').remove();"},
                #insert{target=content,elements=PostsNew},
                #insert{target=content,elements=render('posts-new-controls',S)},
                case ReadOnly of
                    false -> #insert{target=content,elements=#media_input{id=input,target=post}};
                    true -> [] end,
                #wire{actions="window.scrollToElement('input',true);"},
                #focus{target={qs,"#input .post-message"}}
            ]});
        _ ->
            case ReadOnly of false -> skip; true -> wf:remove('input') end,
            wf:insert_bottom(<<"posts-new">>,render(P,#hes{board=B,thread=T},S)),
            case AutoExpand of
                true -> skip;
                false -> erlach_utils:hidden(spa:id(P))
            end,
            wf:update('posts-new-controls',render('posts-new-controls',S))
    end,
    case ReadOnly of false -> skip; true -> spa:success(<<"Bump limit raised, bro">>) end;


event(#pubsub{target=content,action=put,data=#post{id=Id}=PT,from=_Proc}=E) ->
    wf:info(?M,"Pubsub: ~p (~p)",[E,self()]),
    S2=case spa:st() of #st{thread=#post{id=Id}}=S -> spa:st(S#st{thread=PT}); S -> S end, % update state (thread)
    wf:update(spa:id({{post,answers},Id}),render(links,#hes{post=PT},S2));
event(#view{target=post,option=expand,element=E}) -> % show new posts
    wf:wire(wf:f("var x=qi('~s'); x && x.removeAttribute('id');",[E])), % guarded style istead #jq{}
    erlach_utils:hidden(), % clear
    wf:update('posts-new-controls',[#panel{id=E},render('posts-new-controls',spa:st())]);
event(#view{target=post,option=auto,element=E}=Event) -> % show new posts AUTO
    S=spa:st(),
    View=lists:keystore('auto-expand',1,S#st.view,{'auto-expand',true}),
    S2=spa:st(S#st{view=View}),
    wf:wire(#jq{target=E,method=["removeAttribute"],args=["'id'"]}),
    erlach_utils:hidden(), % clear
    wf:update('posts-new-controls',[#panel{id=E,class=auto},render('posts-new-controls',S2)]);
% partially view loader implementation
event(#view{target=post,queue=[],option=#st{post=P},partial=true}) ->
    wf:info(?M,"Finalize post?=~p ~p",[P=/=?UNDEF,self()]),
    case P of #post{id=Pid} -> scroll_to(Pid); _ -> skip end,
    wf:wire("var x=qs('#input .input-controls [disabled]'); x && x.removeAttribute('disabled');");
event(#view{target=post,element=Container,option=#st{board=B,thread=T}=S,partial=true}=V) ->
    wf:info(?M,"Partially view ~p",[self()]),
    Instant=erlach_utils:partial(V),
    wf:insert_adjacent(beforeend,Container,[render(P,#hes{board=B,thread=T},S) || P <- Instant]);

event(#render_event{target=input,event={sage,Boolean,Sage}}) when is_boolean(Boolean) ->
    wf:update(Sage,#button{id=Sage,class=case Boolean of true -> <<"black checked">>; false -> <<"black">> end,
        postback=#render_event{target=input,event={sage,not Boolean,Sage}},
        body= <<"Sage">>,value=Boolean});
    
event(#delete{target=thread,value=Tid}) -> % thread
    wf:info(?M,"Removing thread ~p",[Tid]),
    case kvs:get(post,Tid) of
        {ok,#post{type=thread,feed_id={thread,Bid}}=T} ->
            erlach_feeds:delete(T),
            wf:send({board,Bid},{server,#pubsub{target=content,action=delete,data=T,from=self()}});
        _ -> skip
    end;
event(#delete{target=post,value=Pid}) -> % posts
    wf:info(?M,"Removing post ~p",[Pid]),
    case kvs:get(post,Pid) of
        {ok,#post{type=post,feed_id={post,Tid},image=Im}=P} ->
            erlach_feeds:delete(P),
            wf:send({thread,Tid},{server,#pubsub{target=content,action=delete,data=P,from=self()}});
        _ -> skip
    end;
event(#pubsub{target=content,action=delete,data=#post{type=post}=P,from=Proc}) ->
    wf:info(?M,"RECEIVE delete #post{} ~p",[self()]),
    wf:remove(spa:id(P));
event(#pubsub{target=content,action=delete,data=#post{type=thread}=T,from=Proc}) ->
    wf:info(?M,"RECEIVE delete #thread{} ~p",[self()]),
    case spa:st() of
        #st{board=B,thread=T} ->
            self() ! { server, erlach_qs:mp({board,B}) },
            Link=erlach_qs:ml({thread,B,T}),
            spa:notify(<<"Thread ",Link/binary," removed">>, <<>>);
        _ -> skip
    end;

event({server,{upload_state,ImageContainer,Text}}) ->
    wf:wire("uploadFileShowState('"++wf:to_list(ImageContainer)++"','"++wf:to_list(Text)++"');");
event(#ftp{sid=Sid,status={event,init},filename=FileName,meta={meta,TempContainer,Width,Height},size=Size}=Ftp) ->
    wf:info(?M,"IMAGE INIT ~p ~p ~p ~p",[self(),FileName,Size,TempContainer]),
    
    case (erlach_utils:rst())#rst.image_ftp of
        #ftp{filename=OldKey} -> wf:info(?M,"Aborting temp ~p",[OldKey]), erlach_image:abort(OldKey,default);
        Skip -> wf:info(?M,"Skip temp ~p",[Skip])
    end,
    wf:wire(["var x=qi('",TempContainer,"'); x && ( x.id='",image_panel_id(FileName),"');"]),
    erlach_utils:rst((erlach_utils:rst())#rst{image_ftp=Ftp#ftp{meta={process,FileName,Width,Height}}});
event(#ftp{sid=Sid,status={event,stop},filename=FileName,meta={meta,ImageContainer,_Width,_Height},size=Size}=Ftp) ->
    wf:info(?M,"IMAGE ~p ~p ~p ~p",[self(),FileName,Size,ImageContainer]),
    Pid=self(),
    Pid ! {server,{upload_state,ImageContainer,"Processing"}},
    
    Key=key(Ftp),
    FilePath=upload_path(FileName),
    Target=case spa:st() of
        #st{route=#route{render=erlach_board},board=#board{id=Bid}} -> {board,Bid};
        #st{route=#route{render=erlach_thread},thread=#post{id=Tid}} -> {thread,Tid};
        #st{route=#route{render=?SERVICES},thread=#post{id=Tid}} -> {thread,Tid}
    end,
    FinallyFun=fun(#entry{meta=Aid,infoA=InfoA,infoB=InfoB,path=P}) when is_integer(Aid) ->

            {ok,Attachment}=kvs:get(attachment,Aid),

            W=erlach_image:w(InfoB),
            H=erlach_image:h(InfoB),
            IType=erlach_image:type(InfoB),

            {ok,_}=erlach_feeds:update(Attachment,fun(#attachment{}=A) ->
                {ok,A#attachment{name=FileName,path=P,width=W,height=H,type=IType,original_info=InfoA,info=InfoB}}
            end),
            wf:info(?M,"DEBUG ~p",[5]),
            ok;
        (UnknownEnrty) ->
            wf:warning(?M,"DEBUG ~p ~p",[6,UnknownEnrty]),
            skip
    end,
    ErrorFun=fun(#entry{meta=Aid}=E) when is_integer(Aid) ->
            wf:info(?M,"[DEBUG] Error fun",[E]),
            case kvs:get(attachment,Aid) of
                {ok,#attachment{thread=AT,post=AP}=A} ->
                    wf:info(?M,"Clear attachment with convert error ~p",[Aid]),
                    case kvs:get(post,wf:coalesce([AP,AT])) of
                        {ok,#post{message= <<>>}=ToDelete} -> erlach_feeds:delete(ToDelete);
                        _ -> skip
                    end,
                    erlach_feeds:delete(A);
                Err -> wf:warning(?M,"Error fun, attachment not found ~p ~p",[Err,E]), skip
            end,
            false;
        (E) ->
            wf:info(?M,"Error fun: post not created yet for entry~n  ~p",[E]),
            false
    end,
    Meta=?UNDEF,
    Group=default,
    Hash=fun(Term) -> spa_utils:hash(Term,16777216) end,
    {{_,Month,Day},_}=calendar:universal_time(),
    Destination=filename:join([storage(),Hash({m,Month}),Hash({d,Day})]),
    Path=filename:join([Hash({m,Month}),Hash({d,Day})]),
    
    wf:info(?M,"Convert ~p",[Key]),
    
    erlach_image:convert(Key,Group,FilePath,storage(),Path,Meta,Target,FinallyFun,ErrorFun);

event(#pubsub{target=image,action=convert,element=Key,meta=Meta,data={error,Reason},from=From}=Evt) ->
    wf:warning(?M,"Convert error: ~p ~p",[Reason,Key]),
    case self() of
        From ->
            Rst=erlach_utils:rst(),
            CurrentKey=case Rst#rst.image_ftp of #ftp{}=Ftp -> key(Ftp); _ -> ?UNDEF end,
            case CurrentKey of
                Key -> erlach_utils:rst(Rst#rst{image_ftp=?UNDEF});
                _ -> skip
            end,
            wf:wire(["uploadFileError('",image_panel_id(Key),"');"]), spa:warning(<<"Upload error, bad image">>);
        _ -> skip % TODO: remove image panel
    end;
event(#pubsub{target=image,action=convert,element=Key,meta=Meta,data={ok,Storage,Path,FileNameExt,InfoA,InfoB}=D,from=From}=Evt) ->
    wf:info(?M,"RECEIVE converted image ~p ~p ~p",[self(),erlach_utils:rst(),Evt]),

    case erlach_utils:rst() of
        #rst{image_ftp=Ftp=#ftp{meta={process,Key,_,_}}}=Rst ->
            wf:info(?M,"RECEIVE converted image ~p ~p",[self(),<<"rst ok">>]),
            erlach_utils:rst(Rst#rst{image_ftp=Ftp#ftp{meta={converted,Key,Storage,Path,FileNameExt,InfoA,InfoB}}});
        _ ->
            wf:info(?M,"RECEIVE converted image ~p ~p ",[self(),skip]),
            skip
    end,
    Wire=["imgLoad('",image_panel_id(Key),"','",url(Path,FileNameExt),"');"],
    wf:info(?M,"RECEIVE converted image ~p ~p ",[self(),{wire,Wire}]),
    wf:wire(Wire),
    wf:info(?M,"RECEIVE converted image ~p ~p ",[self(),{wire,ok}]);

event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).

key(#ftp{filename=Key}) -> Key.

count(Tid) -> case kvs:get(feed,{post,Tid}) of {ok,#feed{count=Pc}} -> Pc; _ -> 0 end.
limit(#board{limit=L}) -> case L of L when is_integer(L) -> L; _ -> wf:config(erlach,bump_limit,500) end.

is_read_only(#st{thread=#post{readonly=true}}) -> true;
is_read_only(#st{board=B,thread=#post{id=Tid}}=S) -> is_read_only(count(Tid),limit(B));
is_read_only(_) -> false.

is_read_only(Count,Limit) -> Count >= Limit.

storage() -> wf:to_binary(wf:config(erlach,storage,code:priv_dir(erlach))).
upload_path(FileName) -> filename:join(wf:config(n2o,upload),FileName).


url(Path,FileName) -> filename:join([wf:config(erlach,storage_urn),wf:to_binary(Path),wf:to_binary(FileName)]).
url(#attachment{name=N,path=P,info=I}) -> url(P,[N,erlach_image:ext(I)]).

path(#attachment{name=N,path=P,info=I}) -> filename:join([storage(),P,[N,erlach_image:ext(I)]]). % for erlach_feeds

image_panel_id(FileName) -> "im-"++spa_utils:hash({image_key,FileName}).
