-module(erlach_thread).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").

title(#st{thread=#post{}=T}) -> <<(topic(T))/binary," – Erlach"/utf8>>.
urn() -> ?UNDEF.

init(#route{board=Burn,thread=Turn}=Route) ->
    wf:info(?M,"init ~p ~p",[Burn,Turn]),
    case {kvs:index(board,urn,Burn),kvs:get(post,erlach_qs:urn_to_id(Turn))} of
        {[#board{id=Bid}=B],{ok,#post{type=thread,feed_id={thread,Bid},id=Tid}=T}} ->
            wf:reg({board,Bid}),
            wf:reg({thread,Tid}),
            {ok,#st{route=Route,action=view,board=B,thread=T}};
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
    #st{thread=#post{id=Tid}}=spa:st(),
    wf:unreg({thread,Tid}),
    wf:info(?M,"TERMINATE ~p OK",[self()]).

topic(#post{urn=Urn,name_escaped=Name}) ->
    case utils:strip(Name) of
        <<>> -> <<"thread#",Urn/binary>>;
        Text -> Text
    end.

scroll_to(Pid) ->
    case spa:st() of #st{thread=#post{id=Pid}} -> wf:wire(wf:f("scrollToTop();")); _ -> skip end, % if thread
    wf:wire(wf:f("scrollToPost('~s');",[spa:id({post,Pid})])).

render(content=Panel,#st{thread=#post{id=Tid}=T}=S) ->
    T1=erlang:system_time(),
    PostList=kvs:entries(kvs:get(feed,{post,Tid}),post,undefined),
    T2=erlang:system_time(),
    wf:info(?M,"TIMINGS DB ops in ~p ms",[(T2-T1)/1000000]),
    Container= <<"posts">>,
    Instant=erlach_utils:partial(#view{target=post,element=Container,queue=PostList,option=S,partial=true}),
    #panel{id=Panel,body=[
        #panel{class= <<"content-title">>,body=topic(T)},
        render(T,#hes{},S),
        #panel{id=Container,body=[ render(P,#hes{},S) || P <- Instant ]},
        #panel{id= <<"posts-new">>},
        render('posts-new-controls',S),
        case is_read_only(S) of true -> []; false -> #media_input{id= <<"input">>,image= <<"input-image">>,target=post,disabled=true} end
    ]};
render({imagePanel,ImagePanel,Image},#st{}) ->
    case Image of
        ?UNDEF -> [];
        Aid ->
            case kvs:get(attachment,Aid) of
                {ok,#attachment{path=RelPath,view=View,width=JsWidth,height=JsHeight}} ->
                    Class=["post-image",case proplists:get_value(image,View) of thumbnail -> "thumbnail"; _ -> [] end],
                    Path=path(RelPath),                    
                    CanvasPanel=case send_image(ImagePanel,Path) of
                        {ok,ImInfo} ->
                            {ok,W,H}=erlach_image:dim(ImInfo),
                            #canvas{class=[media,image],width=W,height=H};
                        _ ->
                            wf:info(?M,"Attachment file not found ~p",[Path]),
                            #canvas{class=[media,image],width=JsWidth,height=JsHeight}
                    end,
                    #panel{id=ImagePanel,class=Class,body=[
                        #panel{body=[
                            #panel{class= <<"image-process fl cent visibled">>,
                                body=wf:jse(element_media_input:progress(<<"#fe8675">>))},
                                CanvasPanel
                        ]}
                    ]};
                _ -> wf:warning(?M,"Attachment db record not found ~p",[Aid]), []
            end
    end;
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
render(message,#hes{post=#post{message_escaped=Message}},#st{}=S) ->
    lists:reverse(lists:foldl(fun([Text,Capture],Acc) ->
                       Link=case kvs:get(post,erlach_qs:urn_to_id(Capture)) of
                           {ok,#post{}=P} -> erlach_utils:link(#hookup{class=l},P,S);
                           _ -> [?RPL,Capture]
                       end,     
                       [[Text,Link] | Acc];
                   ([Text],Acc) ->
                       [Text | Acc]
        end,[],re:split(Message,re_compiled(links_replace),[group,trim,{return,binary}])));
render(reply,#hes{post=#post{urn=Urn}},#st{}) ->
    #link{class= <<"l lemon reply-action">>,body=?RPL_A,
        onclick=wf:jse(wf:f("append(qs('#input .post-message'),'>>~s ');",[Urn])) };
render(links,#hes{post=#post{id=Pid,links=Links}},#st{}=S) ->
    Panel=spa:id({{post,answers},Pid}),
    LinkElements=utils:map_join(fun(LinkId) ->
        case kvs:get(post,LinkId) of
            {ok,L} -> erlach_utils:link(#hookup{class=[l,lemon]},L,S);
            _ -> #span{body=[?RPL,erlach_qs:id_to_urn(LinkId)]}
        end
        end,#span{body= <<", ">>},lists:reverse(Links)),
    case LinkElements of
        [] -> #panel{id=Panel,body=[]}; % for updating
        _ -> #panel{id=Panel,class= <<"post-answers">>,body=[#span{body= <<"Answers: ">>},LinkElements]}
    end;
render(#post{type=thread,id=Tid,urn=Urn,links=Links,message_escaped=Message,image=Image}=T,#hes{}=Hes,#st{board=B}=S) ->
    Panel=spa:id(T),
    ImagePanel=spa:id({image,Tid}),
    #panel{id=Panel,class= <<"post head">>,
        body=#panel{class= <<"post-content">>,body=[
            render({imagePanel,ImagePanel,Image},S),
            #panel{class= <<"post-message">>,body=[
                render({'post-header',T},Hes,S),
                render(message,#hes{post=T},S),
                render(reply,#hes{post=T},S)
            ]},
            render(links,#hes{post=T},S)
    ]}};
render(#post{type=post,id=Pid,urn=Urn,image=Image}=P,#hes{}=Hes,#st{}=S) ->
    Panel=spa:id(P),
    ImagePanel=spa:id({image,Pid}),
    #panel{id=Panel,class= <<"post">>,
        body=#panel{class= <<"post-content">>,body=[
            render({imagePanel,ImagePanel,Image},S),
            #panel{class= <<"post-message">>,body=[
                render({'post-header',P},Hes,S),
                render(message,#hes{post=P},S),
                render(reply,#hes{post=P},S)
            ]},
            render(links,#hes{post=P},S)
    ]}}.

event(#add{target=Target,forms=[TopicOrSage,Input,Selector]}) when Target =:= thread orelse Target =:= post ->
    wf:info(?M,"Adding post ~p",[Target]),
    M=utils:strip(wf:q(Input)),
    {T,IsSage}=case {Target,wf:q(TopicOrSage)} of
        {thread,X}        -> {utils:strip(X),false};
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
    

    % #rst.image for overwriting image in post
    case {Target,size(T),size(M),erlach_utils:rst()} of
        {thread,SZT,_,_} when SZT > LIMT -> spa:warning(<<"Very long topic, bro">>);
        {_,_,SZM,_} when SZM > LIMM -> spa:warning(<<"Very long message, LOL">>);
        {_,_,0,#rst{image=?UNDEF}} -> spa:warning(<<"Nothing publish">>);
        {_,SZT,SZM,Rst} ->
            #st{board=#board{id=Bid},thread=StateThread}=S=spa:st(),
            Pid=kvs:next_id(post,1),
            Tid=case Target of post -> StateThread#post.id; thread -> Pid end,
            
            EscapedT=case Target of thread -> wf:to_binary(wf:html_encode(wf:to_list(T))); post -> <<>> end,
            EscapedM=wf:to_binary(wf:html_encode(wf:to_list(utils:squish(M)))),
            
            Image=case Rst#rst.image of
                #ftp{filename=RelPath,size=Size,meta={meta,_,Width,Height}}=Ftp ->
                    erlach_image:target(path(RelPath),{element,image,Pid}),
                    erlach_utils:rst(Rst#rst{image=?UNDEF}),
                    A=#attachment{id=kvs:next_id(attachment,1),
                        feed_id={attachment,Bid},
                        target=Pid,
                        created=erlang:system_time(),original_path=RelPath,path=erlach_image:path(RelPath),
                        original_mime= <<>>,
                        view=View,size=Size,width=Width,height=Height},
                    {ok,#attachment{id=Aid}}=erlach_feeds:call(append,A),
                    erlach_stat:incr(attachment,{Bid,Tid,A}),
                    Aid;
                _ -> ?UNDEF
            end,
            wf:info(?M,"Adding post ~p",[1]),
            
            case re:run(EscapedM,re_compiled(links_replace),[global,{capture,[1],binary}]) of
                nomatch -> [];
                {match,Match} -> % {match,[[<<"234">>],[<<"23434">>],[<<"23434343">>]]}
                    lists:foreach(fun([Idx]) ->
                        FunUpdate=fun(#post{links=Links}=P) -> {ok,P#post{links=lists:usort([Pid | Links])}} end,
                        {ok,P}=kvs:get(post,erlach_qs:urn_to_id(Idx)),
                        case erlach_feeds:call(update,P,FunUpdate) of
                            {ok,#post{type=thread,id=I,feed_id={thread,F}}=P2} ->
                                    wf:send({board,F},{server,#pubsub{target=content,action=put,data=P2,from=self()}}),
                                    wf:send({subscription,thread,I},{server,#pubsub{target=subscription,action=put,element=P2,data=(spa:st())#st{level=self()},from=self()}});
                            {ok,#post{type=post,feed_id={post,F}}=P2} ->
                                wf:send({thread,F},{server,#pubsub{target=content,action=put,data=P2,from=self()}}),
                                wf:send({subscription,thread,F},{server,#pubsub{target=subscription,action=put,element=P2,data=(spa:st())#st{level=self()},from=self()}});
                            _ -> skip
                        end
                    end,lists:usort(Match))
            end,
            Addition=case Target of % TODO: move to KVS FEED SERVER
                thread ->
                    #post{type=thread,id=Tid,feed_id={thread,Bid},
                        created=erlang:system_time(),urn=erlach_qs:id_to_urn(Pid),
                        name=T,name_escaped=EscapedT,message=M,message_escaped=EscapedM,image=Image};
                post ->
                    #post{type=post,id=Pid,feed_id={post,Tid},created=erlang:system_time(),
                        urn=erlach_qs:id_to_urn(Pid),message=M,message_escaped=EscapedM,sage=IsSage,image=Image}
            end,
            
            {ok,Post}=erlach_feeds:call(append,Addition),
            case Target of thread -> erlach_stat:incr(thread,{Bid,Post}); post -> erlach_stat:incr(post,{Bid,Tid,Post}) end,
            erlach_subscription:put(Post,S#st{level=self()}),

            wf:reg({subscription,thread,Tid}),
            {ok,XT}=kvs:get(post,Tid),
            wf:send({subscription,thread,Tid},{server,#pubsub{target=subscription,action=put,element=XT,data=S#st{level=self()},from=self()}}),
            
            wf:wire("textErase();"), % clean
            wf:wire(#jq{target=Input,method=["removeAttribute"],args=["'contentEditable'"]}),
            case Target of thread ->
                wf:wire(#jq{target=TopicOrSage,method=["removeAttribute"],args=["'contentEditable'"]}); post -> skip end,
            wf:wire("qs('#input .input-selector').remove();"),
            wf:wire("qs('#input label').removeAttribute('for');"),
            wf:wire("var x=qs('#input .image-manage'); x.parentNode.removeChild(x);"),
            wf:wire("qs('.input-controls').remove();"),
            wf:wire(#jq{target=input,property=className,right="post"}),
            case Target of thread -> skip;
                post ->
                    wf:wire(#jq{target="qs('#input .post-message')",property=outerHTML,args=simple,format="'~s'",
                        right=[#panel{class= <<"post-message">>,body=[
                                render({'post-header',Post},#hes{},S),
                                render(message,#hes{post=Post},S),
                                render(reply,#hes{post=Post},S) ]},
                            render(links,#hes{post=Post},S)
                        ]})
            end,
            wf:wire(#jq{target=input,property=id,right=spa:id(Post)}),
            ImageContainer=spa:id({image,Pid}),
            wf:wire(#jq{target= <<"input-image">>,property=id,right=ImageContainer}),
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
event(#pubsub{target=content,action=add,data=#post{type=post,feed_id={post,Tid}}=P,from=Proc}) ->
    wf:info(?M,"RECEIVE add #post{} ~p",[self()]),
    
    % bump limit check
    #st{view=View}=S=spa:st(),
    ReadOnly=is_read_only(S),
    
    AutoExpand=case lists:keyfind('auto-expand', 1, View) of {'auto-expand',true} -> true; _ -> false end,
    PostsNew=#panel{id= <<"posts-new">>,class=case AutoExpand of true -> <<"auto">>; false -> <<>> end},
    
    case self() of
        Proc ->
            wf:wire(#jq{target= <<"posts-new">>,method=["removeAttribute"],args=["'id'"]}),
            erlach_utils:hidden(), % clear
            wf:remove('posts-new-controls'),
            wf:insert_bottom(content,PostsNew),
            wf:insert_bottom(content,render('posts-new-controls',S)),
                        
            case ReadOnly of
                false -> wf:insert_bottom(content,#media_input{id= <<"input">>,image= <<"input-image">>,target=post});
                true -> skip end,
            wf:wire("window.scrollTo(0,document.body.scrollHeight);"),
            wf:wire("var x=qs('#input .post-message'); x && x.focus();");
        _ ->
            case ReadOnly of false -> skip; true -> wf:remove('input') end,
            wf:insert_bottom(<<"posts-new">>,render(P,#hes{},S)),
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
event(#view{target=post,queue=[],option=#st{route=#route{query=#query{q2=T,q3=P}}},partial=true}) ->
    wf:info(?M,"Finalize q2=~p, q3=~p (~p)",[T,P,self()]),
    case P of ?UNDEF -> skip; _ -> scroll_to(erlach_qs:urn_to_id(P)) end,
    wf:wire("var x=qs('#input .input-controls [disabled]'); x && x.removeAttribute('disabled');");
event(#view{target=post,element=Container,option=S,partial=true}=V) ->
    wf:info(?M,"Partially view ~p",[self()]),
    Instant=erlach_utils:partial(V),
    wf:insert_adjacent(beforeend,Container,[render(P,#hes{},S) || P <- Instant]);

    
event(#render_event{target=input,event={sage,Boolean,Sage}}) when is_boolean(Boolean) ->
    wf:update(Sage,#button{id=Sage,class=case Boolean of true -> <<"black checked">>; false -> <<"black">> end,
        postback=#render_event{target=input,event={sage,not Boolean,Sage}},
        body= <<"Sage">>,value=Boolean});

% Removing
event(#delete{target=thread,value=Tid}) -> % thread
    wf:info(?M,"Removing thread ~p",[Tid]),
    % delete(T),
    case kvs:get(post,Tid) of
        {ok,#post{type=thread,feed_id={thread,Bid}}=T} ->
            erlach_feeds:call(delete,T),
            wf:send({board,Bid},{server,#pubsub{target=content,action=delete,data=T,from=self()}});
        _ -> skip
    end;
event(#delete{target=post,value=Pid}) -> % posts
    wf:info(?M,"Removing post ~p",[Pid]),
    case kvs:get(post,Pid) of
        {ok,#post{type=post,feed_id={post,Tid},image=Im}=P} ->
            erlach_feeds:call(delete,P),
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
event(#ftp{sid=Sid,status={event,init},filename=RelPath,meta={meta,ImageContainer,_Width,_Height},size=Size}=Ftp) ->
    wf:info(?M,"IMAGE INIT ~p ~p ~p",[RelPath,Size,ImageContainer]),
    case (erlach_utils:rst())#rst.image of
        #ftp{filename=Preview} ->
            FileName=path(erlach_image:path(Preview)),
            wf:info(?M,"Remove temp ~p",[FileName]),
            file:delete(FileName);
        Skip -> wf:info(?M,"Skip temp ~p",[Skip])
    end,
    erlach_utils:rst((erlach_utils:rst())#rst{image=Ftp});
event(#ftp{sid=Sid,status={event,stop},filename=RelPath,meta={meta,ImageContainer,_Width,_Height},size=Size}) ->
    wf:info(?M,"IMAGE ~p ~p ~p",[RelPath,Size,ImageContainer]),
    Pid=self(),
    FilePath=path(RelPath),
    wf:info(?M,"IMAGE ~p",[1]),    
    Pid ! {server,{upload_state,ImageContainer,"Processing"}},
    wf:info(?M,"IMAGE ~p",[(spa:st())#st.route#route.render]),
    
    Group=case spa:st() of
        #st{route=#route{render=erlach_board},board=#board{id=Bid}} -> {board,Bid};
        #st{route=#route{render=erlach_thread},thread=#post{id=Tid}} -> {thread,Tid}
    end,
    wf:info(?M,"IMAGE ~p",[Group]),
    erlach_image:convert(FilePath,Group,{container,ImageContainer});
event(#pubsub{target=image,action=convert,element=Target,data=Data,from=From}) ->
    wf:info(?M,"RECEIVE converted image ~p ~p",[self(),Target]),
    Client=fun({ok,Path},C) ->
            {ok,ImInfo}=send_image(C,Path),
            {ok,W,H}=erlach_image:dim(ImInfo),
            wf:wire(wf:q("setDimension('~s',~b,~b)",[C,W,H]));
        ({error,E},C) ->
            wf:warning(?M,"Convert error: ~p",[E]),
            wf:wire(wf:f("uploadFileError('~s');",[C]))
    end,
    case Target of
        {container,Cc} when self() =:= From -> Client(Data,Cc); % self
        {container,_} -> skip; % page reloaded and post not stored
        {element,Key,Id} -> Client(Data,spa:id({Key,Id})) % all (stored)
    end;

event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).

count(Tid) -> case kvs:get(feed,{post,Tid}) of {ok,#feed{count=Pc}} -> Pc; _ -> 0 end.
limit(#st{board=#board{limit=L}}) -> case L of L when is_integer(L) -> L; _ -> wf:config(erlach,bump_limit,500) end.

is_read_only(#st{thread=#post{readonly=true}}) -> true;
is_read_only(#st{thread=#post{id=Tid}}=S) -> is_read_only(count(Tid),limit(S));
is_read_only(_) -> false.

is_read_only(Count,Limit) -> Count >= Limit.

root() -> wf:config(n2o,upload,code:priv_dir(erlach)).
path(RelPath) -> wf:to_binary(filename:join(root(),wf:to_list(RelPath))).
send_image(Container,Path) ->
    wf:info(?M,"SEND IMAGE 1/2 ~p ~p",[Container,Path]),
    case erlach_image:image_info_file(Path) of
        {ok,ImageInfo,Bin} ->
            {ok,W,H}=erlach_image:dim(ImageInfo),
            Meta=[wf:to_list(Container),erlach_image:mime(ImageInfo),W,H],
            wf:info(?M,"SEND IMAGE 2/2 ~p ~p",[self(),byte_size(Bin)]),
            self() ! #ftp{status= <<"relay">>,meta=Meta,data=Bin},
            {ok,ImageInfo};
        Error -> Error
    end.

% Regexp. For output use: rp(erlach_thread:re(links_replace)).
re(links_replace) -> {ok,Re}=re:compile(<<"&gt;&gt;([a-z0-9]{1,10})\\b">>),Re.
re_compiled(links_replace) ->
    {re_pattern,1,0,0,
            <<69,82,67,80,134,0,0,0,0,0,0,0,81,0,0,0,255,255,255,255,
              255,255,255,255,38,0,59,0,1,0,1,0,0,0,64,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,125,0,66,
              29,38,29,103,29,116,29,59,29,38,29,103,29,116,29,59,
              127,0,43,0,1,106,0,0,0,0,0,0,255,3,0,0,0,0,254,255,255,
              7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,104,0,1,0,10,114,0,
              43,5,114,0,66,0>>}.