-module(erlach_thread).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").
-include("erlach_image.hrl").

title(#st{thread=#post{}=T}) -> <<(topic(T,false))/binary," – Erlach"/utf8>>.
urn() -> ?URN_PAGE_DYNAMIC.

init(#route{board=Burn,thread=Turn,post=Purn}=Route) ->
    wf:info(?M,"init ~p ~p ~p",[Burn,Turn,Purn]),
    case {erlach_board:get_board(Burn),kvs:get(post,erlach_qs:urn_to_id(Turn))} of
        {#board{id=Bid}=B,{ok,#post{type=thread,feed_id={thread,Bid},id=Tid}=T}} ->
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
            {ok,#st{user=eauth_user:get(),route=Route,action=view,board=B,thread=T,post=P}};
        _ -> {redirect,erlach_qs:mp(main)}
    end.
finalize(#st{thread=#post{id=Tid},route=#route{option=highlight}}) ->
    wf:info(?M,"finalize with option (~p)",[self()]),
    [{scroll,Tid},{wait_finalize,true}];
finalize(#st{post=P}) ->
    wf:info(?M,"finalize (~p)",[self()]),
    case P of
        #post{id=Pid} ->
            [{scroll,Pid},{wait_finalize,true}];
        _ ->
            [{wait_finalize,true}]
    end.
terminate() ->
    wf:info(?M,"TERMINATE ~p",[self()]),
    convert_clear_pending(),
    #st{thread=#post{id=Tid},board=#board{id=Bid}}=spa:st(),
    wf:unreg({board,Bid}),
    wf:unreg({thread,Tid}),
    wf:info(?M,"TERMINATE ~p OK",[self()]).

topic(#post{}=P) -> topic(P,true).
topic(#post{urn=Urn,name=Name,name_escaped=Escaped},IsEscaped) ->
    Topic = case IsEscaped of true -> Escaped; _ -> Name end,
    case spa_utils:strip(Topic) of
        <<>> -> <<"thread#",Urn/binary>>;
        Text -> Text
    end.

up_state(Pid) ->
    wf:info(?M,"Up state ~p",[Pid]),
    case kvs:get(post,Pid) of {ok,P} -> spa:st((spa:st())#st{post=P}), erlach_qs:history_update(); _ -> skip end.
scroll_to(Pid) ->
    wf:info(?M,"Scroll to ~p",[Pid]),
    up_state(Pid),
    wf:wire(wf:f("scrollToPost('~s');",[erlach_utils:post_id(Pid)])).


load_actions(#view{}=View) ->
    E=wf_event:new(View,"lambda",?M,event,"[]",[]),
    [list_to_binary(["{ setTimeout(function() { ",E," },20) }"])];
load_actions(_) -> "var x=qs('#input .input-controls [disabled]');x&&x.removeAttribute('disabled');finalize();".


render(content=Panel,#st{thread=#post{id=Tid}=T,board=B}=S) ->
    Container= <<"posts">>,
    View=#view{target=post,
        element=Container,
        option={thread,Tid},
        partial=true,
        table=post,
        feed={post,Tid},
        count=wf:config(erlach,db_fold_count_first,3),
        direction=next },
    
    {Instant,View2}=erlach_utils:partial(View),
    
    FilterAction=case erlach_markup:image_filter(topic(T,false)) of
        ?UNDEF -> [];
        Filter -> #wire{actions=[
            "var x = function(c) { c && c.classList.add('filter','",wf:to_list(Filter),"') };",
            "x(qi('content')); x(qi('viewer'));"
        ]}
    end,
    
    View3=case View2 of #view{} -> View2#view{count=wf:config(erlach,db_fold_count,70)}; _ -> View2 end,

    #panel{id=Panel,
        actions=[ FilterAction, load_actions(View3) ],
        body=[
            #panel{class= <<"content-title">>,body=topic(T)},
            render(T,#hes{board=B},S),
            #panel{id=Container,body=lists:foldl(fun(P,A) -> [render(P,#hes{thread=T,board=B},S)|A] end,[],Instant)},
            #panel{id= <<"posts-new">>},
            render('posts-new-controls',S),
            case is_archived(S) of true -> []; false -> #media_input{id=input,target=post,disabled=true} end
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
    
    ImageControls=fun(ImPanelId,#bpg{animation=Anim}) -> #panel{class= <<"image-manage visibled">>,body=[
                #span{body= <<"BPG"/utf8>>},
                case Anim of
                    true -> [
                        #button{class= <<"control play slim">>, body="Play",actions=["render_image('",ImPanelId,"');"]},
                        #button{class= <<"control pause slim">>, body= <<"❚❚"/utf8>>},
                        #button{class= <<"control stop slim">>, body= <<"▲"/utf8>>}
                        ];
                    _ -> []
                end
            ]};
        (_ImPanelId,_Info) -> []
    end,
    case kvs:get(attachment,Aid) of
        {ok,#attachment{path=?UNDEF,name=UploadName,view=View,width=JsWidth,height=JsHeight}=A} -> % deferred
            ImPanelId=erlach_utils:image_panel_id(UploadName,S),
            element_media_input:image_panel(ImPanelId,Class(View),true,JsWidth,JsHeight,
                Fields(A),?UNDEF,?UNDEF,ImageControls(ImPanelId,?UNDEF));
        {ok,#attachment{path=Path,name=Name,view=View,width=Width,height=Height,info=Info}=A} ->
            ImPanelId=erlach_utils:image_panel_id(Name,S),
            element_media_input:image_panel(ImPanelId,Class(View),true,Width,Height,
                Fields(A),url(A),erlach_image:type(Info),ImageControls(ImPanelId,Info));
        _ -> wf:warning(?M,"Attachment db record not found ~p",[Aid]), []
    end;
    
render({'post-header',#post{type=thread,id=Tid}=T},#hes{}=Hes,#st{}=S) ->
    [];

render({'post-header',#post{type=post}=P},#hes{}=Hes,#st{}=S) ->
    [];

render({'post-manage',#post{id=Id,created=Ts,type=Type}=P},#hes{}=Hes,#st{access=A}=S) ->
    UTC=spa_utils:now_js(Ts),
    Panel=spa:id({manage,Id}),
    PostPanel=erlach_utils:post_id(P),
    #panel{id=Panel,class= <<"post-manage">>,body=[
        case A of full -> #a{class=[b,warn],body= <<"Удалить"/utf8>>,postback=#delete{target=Type,value=Id}}; _ -> [] end,
        case spa:render(S) of
            ?THREAD -> render(reply,Hes,S);
            ?SERVICES -> render(reply,Hes,S);
            _ -> []
        end,
        #a{class= <<"pearl collapse-action">>, %onclick=["collapse(\\'",PostPanel,"\\');"],
            body= <<"✄"/utf8>>,title= <<"Скрыть"/utf8>>},
        #a{class= <<"pearl expand-action">>, %onclick=["expand(\\'",PostPanel,"\\');"],
            body= <<"✧"/utf8>>,title= <<"Показать"/utf8>>},
        #panel{class=time,data_fields=[{<<"data-ts">>,UTC}] }, % ,actions=["render_time('",Panel,"');"] }
        #panel{class=pid}
    ]};
render(message,#hes{post=#post{message=Raw,message_escaped=Esc}=P}=Hes,#st{}=S) ->
    erlach_markup:html(P,Hes,S);

render(reply,#hes{post=P,board=B,thread=T},#st{}) ->    
    Meta=case P of #post{} -> {post,B,T,P}; _ -> {thread,B,T} end,
    #button{class= <<"l lemon reply-action">>,body=?RPL_A};

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
        _ -> #panel{id=Panel,class= <<"post-answers">>,body=[#span{body=[
                #span{class=ru,body= <<"Ответы:"/utf8>>},
                #span{class=en,body= <<"Replies:"/utf8>>} ]},LinkElements]}
    end;

render(#post{type=thread,id=Tid,urn=Urn,links=Links,message_escaped=Message,image=Image}=T,#hes{board=B}=Hes,#st{}=S) ->
    Panel=erlach_utils:post_id(T),
    #panel{id=Panel,class= <<"post head">>,
        actions=["render('",Panel,"');"],
        data_fields=[{<<"data-id">>,Urn}],
        body=[
            render({'post-manage',T},Hes#hes{thread=T},S),
            #panel{class= <<"post-content">>,body=[
                #panel{class= <<"post-flash">>,body=[
                    render({imagePanel,Image},Hes,S),
                    #panel{class= <<"post-message">>,body=[
                        render({'post-header',T},Hes,S),
                        render(message,#hes{post=T},S)
                    ]},
                    render(links,#hes{post=T},S),
                    #panel{class= <<"hidden-info">>,body=?TR(<<"Скрыто"/utf8>>,<<"Hidden"/utf8>>,<<"Приховано"/utf8>>)}
                ]}
            ]}
        ]};

render(#post{type=post,urn=Urn,image=Image,sage=Sage}=P,#hes{board=B,thread=T}=Hes,#st{}=S) ->
    Panel=erlach_utils:post_id(P),
    Hes2=Hes#hes{post=P},
    #panel{id=Panel,class=[post,case Sage of true -> sage; _ -> [] end],
        actions=["render('",Panel,"');"],
        data_fields=[{<<"data-id">>,Urn}],
        body=[
            render({'post-manage',P},Hes2,S),
            #panel{class= <<"post-content">>,body=[
                #panel{class= <<"post-flash">>,body=[
                    render({imagePanel,Image},Hes,S),
                    #panel{class= <<"post-message">>,body=[
                        render({'post-header',P},Hes2,S),
                        render(message,Hes2,S)
                    ]},
                    render(links,#hes{post=P},S),
                    #panel{class= <<"hidden-info">>,body=?TR(<<"Скрыто"/utf8>>,<<"Hidden"/utf8>>,<<"Приховано"/utf8>>)}
                ]}
            ]}
        ]}.


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
            #st{board=#board{id=Bid,feed_id={board,PartyId},view=Bview}=StateBoard,thread=StateThread}=S=spa:st(),
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
                            erlach_image:update(FileName,key_group(),UpdateFun),
                            {JsW,JsH,?UNDEF,?UNDEF,?UNDEF};
                        {converted,_Key,_Storage,Path,_FileNameExt,IiA,IiB} -> % already converted
                            {erlach_image:w(IiB),erlach_image:h(IiB),Path,IiA,IiB}
                    end,
                    A=#attachment{id=Aid,
                        feed_id={attachment,Bid},
                        party=PartyId,
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
            
            Addition=case Target of
                thread ->
                    #post{type=thread,id=Tid,feed_id={thread,Bid},
                        created=erlang:timestamp(),urn=erlach_qs:id_to_urn(Pid),
                        party=PartyId,board=Bid,thread=?UNDEF,
                        name=T,name_escaped=EscapedT,message=M,message_escaped=EscapedM,image=Image};
                post ->
                    #post{type=post,id=Pid,feed_id={post,Tid},created=erlang:timestamp(),
                        party=PartyId,board=Bid,thread=Tid,
                        urn=erlach_qs:id_to_urn(Pid),message=M,message_escaped=EscapedM,sage=IsSage,image=Image}
            end,
            
            %
            % APPEND POST/THREAD (SYNC)
            %
            IsBump=not is_bumplimit(S),
            IsUpdateRecent=not spa:option(tor_only,Bview,false),
            {ok,Post}=erlach_feeds:append(Addition,IsBump,IsUpdateRecent),
            erlach_search:index_post(Post),
            
            %
            % ANY MODIFY OPS (MESSAGING, EDITING)
            %
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
                            body=render(message,spa:setoption(limit,wf:config(erlach,board_char_limit,300),#hes{post=Post}),S)}]}),
                    wf:update({qs,"#input .post-manage"},render({'post-manage',Post},#hes{},S));
                post ->
                    wf:wire(#jq{target="qs('#input .post-message')",property=outerHTML,args=simple,format="'~s'",
                        right=[#panel{class= <<"post-message">>,body=[
                                render({'post-header',Post},#hes{},S),
                                render(message,#hes{post=Post},S)
                                ]},
                            render(links,#hes{post=Post},S)
                        ]}),
                    wf:update({qs,"#input .post-manage"},render({'post-manage',Post},#hes{board=StateBoard,thread=StateThread,post=Post},S)),
                    wf:wire(#insert{
                        target={qs,"#input .post-flash"},
                        elements=#panel{class= <<"hidden-info">>,body=
                            ?TR(<<"Скрыто"/utf8>>,<<"Hidden"/utf8>>,<<"Приховано"/utf8>>)},
                        position=beforeend})
            end,
            wf:wire(#jq{target=input,property=id,right=erlach_utils:post_id(Post)}),
            
            wf:wire("onAddPostSelf();"),
            case Target of
                thread ->
                    case {kvs:get(party,PartyId),StateBoard,Addition} of
                        {{ok,#party{hidden=false}},#board{hidden=false},#post{hidden=false}} ->
                            wf:wire(["cacachSender('",erlach_qs:ml({thread,StateBoard,Addition}),"','",topic(Addition),"');"]);
                        _ -> skip
                    end,
                    wf:send({board,Bid},{server,#pubsub{target=content,action=add,data=Post,from=self()}}),
                    wf:insert_top(threads,erlach_board:render(controls,spa:st())); % only for erlach_board
                post ->
                    {ok,XT2}=kvs:get(post,Tid),
                    wf:send({board,Bid},{server,#pubsub{target=content,action=update,data=XT2,from=self()}}), % update post count
                    wf:send({thread,Tid},{server,#pubsub{target=content,action=add,data=Post,from=self()}})
            end
    end, [];

event(#pubsub{target=content,action=add,data=#post{type=post,urn=Urn,feed_id={post,Tid},sage=Sage}=P,from=Proc}) ->
    wf:info(?M,"RECEIVE add #post{} ~p",[self()]),
    
    % bump limit check
    #st{view=View,board=B,thread=T}=S=spa:st(),
    IsArchived=is_archived(S),
    BumpLimit=is_bumplimit(S),
    
    AutoExpand=case lists:keyfind('auto-expand', 1, View) of {'auto-expand',true} -> true; _ -> false end,
    PostsNew=#panel{id= <<"posts-new">>,class=case AutoExpand of true -> <<"auto">>; false -> <<>> end},
    Panel=erlach_utils:post_id(P),
    
    case self() of
        Proc ->
            erlach_utils:hidden(), % clear

            wf:wire(#multi{actions=[
                case Sage of true -> #jq{target=Panel,method=["classList.add"],args=["'sage'"]}; _ -> [] end,
                #jq{target= <<"posts-new">>,method=["removeAttribute"],args=["'id'"]},
                #wire{actions="qi('posts-new-controls').remove();"},
                #insert{target=content,elements=PostsNew},
                #insert{target=content,elements=render('posts-new-controls',S)},
                case IsArchived of
                    false -> #insert{target=content,elements=#media_input{id=input,target=post}};
                    true -> [] end,
                #jq{target=Panel,property=["dataset.id"],right=Urn},

                #wire{actions=["render('",Panel,"');"]},
                #wire{actions="window.scrollToElement('input',true);"},
                #focus{target={qs,"#input .post-message"}}
            ]});
        _ ->
            case IsArchived of false -> skip; true -> wf:remove('input') end,
            wf:insert_bottom(<<"posts-new">>,render(P,#hes{board=B,thread=T},S)),
            case AutoExpand of
                true -> skip;
                false -> erlach_utils:hidden(Panel)
            end,
            wf:update('posts-new-controls',render('posts-new-controls',S))
    end,
    case BumpLimit of false -> skip; true -> spa:success(<<"Bump limit raised, bro">>) end;

event(#pubsub{target=content,action=put,data=#post{id=Id}=PT,from=_Proc}=E) ->
    wf:info(?M,"Pubsub: ~p (~p)",[E,self()]),
    S2=case spa:st() of #st{thread=#post{id=Id}}=S -> spa:st(S#st{thread=PT}); S -> S end, % update state (thread)
    wf:update(spa:id({{post,answers},Id}),render(links,#hes{post=PT},S2)),
    wf:wire(["render('",erlach_utils:post_id(PT),"');"]);

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

event(#view{target=post,option={thread,Tid},element=Container,partial=true,start=Start,count=Count}=View) ->
    wf:info(?M,"Partial view ~p",[View]),
    case spa:st() of
        #st{thread=#post{id=Tid}=T,board=B}=S ->
            {Instant,View2}=erlach_utils:partial(View),
            wf:insert_adjacent(beforeend,Container,lists:foldl(fun(P,A) -> [render(P,#hes{thread=T,board=B},S)|A] end,[],Instant)),
            wf:wire(load_actions(View2));
        _ -> skip
    end;

event(#render_event{target=input,event={sage,Boolean,Sage}}) when is_boolean(Boolean) ->
    wf:update(Sage,#button{id=Sage,class=case Boolean of true -> <<"black checked">>; false -> <<"black">> end,
        postback=#render_event{target=input,event={sage,not Boolean,Sage}},
        body=[ #span{class=ru,body= <<"Сажи"/utf8>>},
            #span{class=en,body= <<"Sage"/utf8>>},
            #span{class=ua,body= <<"Сажи"/utf8>>} ],value=Boolean});
    
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
    wf:remove(erlach_utils:post_id(P));
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
    
    convert_abort(),
    wf:wire(["var x=qi('",TempContainer,"'); x && ( x.id='",erlach_utils:image_panel_id(FileName),"');"]),
    erlach_utils:rst((erlach_utils:rst())#rst{image_ftp=Ftp#ftp{meta={process,FileName,Width,Height}}}),
    
    
    %
    % Init convert entry
    %
    Target=case spa:st() of % TODO:
        #st{route=#route{render=?BOARD},board=#board{id=Bid}} -> {board,Bid};
        #st{route=#route{render=?THREAD},thread=#post{id=Tid}} -> {thread,Tid};
        #st{route=#route{render=?SERVICES},thread=#post{id=Tid}} -> {thread,Tid}
    end,

    Key=key(Ftp),
    Group=key_group(),
    FilePath=upload_path(FileName),
    Meta=?UNDEF,
    Hash=fun(Term) -> spa_utils:hash(Term,16777216) end,
    {{_,Month,Day},_}=calendar:universal_time(),
    Destination=filename:join([storage(),Hash({m,Month}),Hash({d,Day})]),
    Path=filename:join([Hash({m,Month}),Hash({d,Day})]),
    AutoStart=false,
    
    wf:info(?M,"Convert [autostart=false] ~p",[Key]),
    erlach_image:convert(Key,Group,FilePath,storage(),Path,Meta,Target,fun convert_finally/1,fun convert_error/1,AutoStart);
    
    
event(#ftp{sid=Sid,status={event,stop},filename=FileName,meta={meta,ImageContainer,_Width,_Height},size=Size}=Ftp) ->
    wf:info(?M,"IMAGE DOWNLOADED ~p ~p ~p ~p",[self(),FileName,Size,ImageContainer]),
    self() ! {server,{upload_state,ImageContainer,"Processing"}},
    
    Key=key(Ftp),
    Group=key_group(),
    erlach_image:run(Key,Group);

event(#pubsub{target=image,action=convert,element=Key,meta=Meta,data={error,Reason},from=From}=Evt) ->
    wf:warning(?M,"~p Convert error: ~p",[self(),{Reason,Key}]),
    case self() of
        From ->
            Rst=erlach_utils:rst(),
            CurrentKey=case Rst#rst.image_ftp of #ftp{}=Ftp -> key(Ftp); _ -> ?UNDEF end,
            case CurrentKey of
                Key -> erlach_utils:rst(Rst#rst{image_ftp=?UNDEF});
                _ -> skip
            end,
            wf:wire(["uploadFileError('",erlach_utils:image_panel_id(Key),"');"]),
            ok;
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
    Wire=["imgLoad('",erlach_utils:image_panel_id(Key),"','",url(Path,FileNameExt),"');"],
    wf:info(?M,"RECEIVE converted image ~p ~p ",[self(),{wire,Wire}]),
    wf:wire(Wire),
    wf:info(?M,"RECEIVE converted image ~p ~p ",[self(),{wire,ok}]);

event(Unknown) ->
    wf:info(?M,"Unknown Event ~p ~n~n~n",[Unknown]), ?EVENT_ROUTER:unknown(?M,Unknown).


key(#ftp{filename=Key}) -> Key.
key_group() -> default.

convert_clear_pending() ->
    wf:warning(?M,"Clear convert pending items ~p",[self()]),
    erlach_image:remove_pending(self(),key_group()).
convert_abort() ->
    case erlach_utils:rst() of
        #rst{image_ftp=#ftp{filename=OldKey}} ->
            wf:warning(?M,"~p Aborting convert ~p",[self(),OldKey]),
            erlach_image:abort(OldKey,key_group(),_ByUser=true);
        Skip ->
            wf:warning(?M,"~p Aborting convert FAIL: #ftp{} not found in ~p",[self(),Skip])
    end,
    wf:warning(?M,"DEBUG ~p",[{4,convert_abort,self()}]),
    ok.
convert_finally(#entry{id=Key,meta=Aid,infoA=InfoA,infoB=InfoB,path=P}=Entry) when is_integer(Aid) ->
    {ok,Attachment}=kvs:get(attachment,Aid),
    [W,H,IType]=[erlach_image:w(InfoB),erlach_image:h(InfoB),erlach_image:type(InfoB)],
    {ok,_}=erlach_feeds:update(Attachment,fun(#attachment{}=A) ->
        {ok,A#attachment{name=Key,path=P,width=W,height=H,type=IType,original_info=InfoA,info=InfoB}}
    end),
    wf:warning(?M,"DEBUG ~p",[{5,convert_finally,self(),Entry}]),
    ok;
convert_finally(Entry) ->
    wf:warning(?M,"DEBUG ~p",[{6,convert_finally,self(),Entry}]),
    skip.
convert_error(#entry{meta=Aid}=E) when is_integer(Aid) ->
    wf:warning(?M,"DEBUG convert_error ~p",[{7,convert_error,self(),E}]),
    case kvs:get(attachment,Aid) of
        {ok,#attachment{thread=AT,post=AP}=A} ->
            wf:info(?M,"Clear attachment with convert error ~p",[Aid]),
            case kvs:get(post,wf:coalesce([AP,AT])) of
                {ok,#post{message= <<>>}=ToDelete} ->
                    erlach_feeds:delete(ToDelete);
                {ok,#post{}=ToUpdate} ->
                    erlach_feeds:update(ToUpdate,fun(#post{}=ToUpdate2) -> {ok,ToUpdate2#post{image=?UNDEF}} end);
                _ ->
                    skip
            end,
            erlach_feeds:delete(A);
        Err -> wf:warning(?M,"Error fun, attachment not found ~p ~p ~p",[self(),Err,E]), skip
    end,
    false;
convert_error(Entry) ->
    wf:warning(?M,"DEBUG convert_error ~p",[{8,convert_error,self(),post_not_created_yet_for_entry,Entry}]),
    wf:info(?M,"Error fun: post not created yet for entry~n  ~p",[Entry]),
    false.

count(Tid) -> case kvs:get(feed,{post,Tid}) of {ok,#feed{count=Pc}} -> Pc; _ -> 0 end.
limit(#board{limit=L}) -> case L of L when is_integer(L) -> L; _ -> wf:config(erlach,bump_limit,500) end.


is_archived(_) -> false.

is_bumplimit(#st{board=B,thread=#post{id=Tid}}=S) -> count(Tid) >= limit(B);
is_bumplimit(_) -> false.

storage() -> wf:to_binary(wf:config(erlach,storage,code:priv_dir(erlach))).
upload_path(FileName) -> filename:join(wf:config(n2o,upload),FileName).


url(Path,FileName) -> filename:join([wf:config(erlach,storage_urn),wf:to_binary(Path),wf:to_binary(FileName)]).
url(#attachment{name=N,path=P,info=I}) -> url(P,[N,erlach_image:ext(I)]).

path(#attachment{name=N,path=P,info=I}) -> filename:join([storage(),P,[N,erlach_image:ext(I)]]). % for erlach_feeds
