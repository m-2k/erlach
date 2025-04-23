-module(erlach_thread).
-author('andy').
-compile(export_all).

-include_lib("n2o/include/wf.hrl").
-include_lib("nitro/include/nitro.hrl").

-include_lib("kvs/include/config.hrl").

-include_lib("erlach_db/include/erlach_db.hrl").
-include("erlach.hrl").

title(#st{}) -> <<"Thread – Erlach"/utf8>>.
urn() -> ?UNDEF.

init(#route{board=Burn,thread=Turn}=Route) ->
    wf:info(?M,"init",[]),
    case {kvs:index(board,urn,Burn),kvs:index(thread,urn,Turn)} of
        {[#board{}=B],[#thread{id=Tid}=T]} ->
            wf:reg({thread,Tid}),
            {ok,#st{route=Route,action=view,board=B,thread=T}};
        _ -> {redirect,erlach_qs:mp(main)}
    end.
terminate() ->
    wf:info(?M,"TERMINATE ~p",[self()]),
    #st{thread=#thread{id=Tid}}=erlang:get(state),
    wf:unreg({thread,Tid}),
    wf:info(?M,"TERMINATE ~p OK",[self()]).

render(content=Panel,#st{thread=T}=S) ->
    PostList=kvs:entries(kvs:get(feed,{post,T#thread.id}),post,undefined),
    #panel{id=Panel,body=[
        #panel{class= <<"content-title">>,body=T#thread.name},
        render(T,#hes{},S),
        #panel{id= <<"posts">>,body=[ render(P,#hes{},S) || P <- PostList ]},
        #panel{id= <<"posts-new">>},
        render('posts-new-controls',S),
        #media_input{id= <<"input">>,image= <<"input-image">>,type=post}
    ]};
render({image_panel,Panel,Image},#st{}) ->
    case Image of
        ?UNDEF -> [];
        Aid ->
            case kvs:get(attachment,Aid) of
                {ok,#attachment{path=RelPath,view=View,height=JsHeight,width=JsWidth}} ->
                    Class=["post-image",case proplists:get_value(image,View) of thumbnail -> "thumbnail"; _ -> [] end],
                    Path=filename:join(root(),wf:to_list(RelPath)),
                    
                    CanvasPanel=case send_bpg(Panel,Path) of
                        #bpg_info{picture_width=BpgW,picture_height=BpgH} -> #canvas{height=BpgH,width=BpgW};
                        _ ->
                            wf:info(?M,"Attachment file not found ~p",[Path]),
                            #canvas{height=JsHeight,width=JsWidth}
                    end,
                    #panel{id=Panel,class=Class,body=#panel{body=[
                        #panel{class= <<"image-process fl cent visibled">>,
                            body=wf:jse(element_media_input:progress(<<"#fe8675">>))},
                            CanvasPanel
                        ]}};
                _ -> wf:warning(?M,"Attachment db record not found ~p",[Aid]), []
            end
    end;
render('posts-new-controls'=Panel,#st{}) ->
    Show=erlach_utils:temp_id(),
    Hidden=(erlach_utils:rst())#rst.hidden_elements,
    
    wf:info(?M,"posts-new-controls: ~p ~p",[Hidden,self()]),
    
    Body=case Hidden of
        [] -> [];
        _ -> #button{id=Show,postback=#view{target= <<"posts-new">>,element=Show},
            body=[<<"Show new (">>,wf:to_binary(length(Hidden)),<<")">>]}
    end,
    #panel{id=Panel,class= <<"center">>,body=Body}.
    
render(#thread{id=Tid,image=Image}=T,#hes{},#st{}=S) ->
    Panel=erlach_utils:id(T),
    ImagePanel=erlach_utils:id({thread,image},Tid),
    #panel{id=Panel,class= <<"post head">>,
        body=#panel{class= <<"post-content">>,body=[
            render({image_panel,ImagePanel,Image},S),
            #panel{class= <<"post-message">>,body=T#thread.message_escaped}
            ]}};
render(#post{id=Pid,image=Image}=P,#hes{},#st{}=S) ->
    Panel=erlach_utils:id(P),
    ImagePanel=erlach_utils:id({post,image},Pid),
    #panel{id=Panel,class= <<"post">>,
        body=#panel{class= <<"post-content">>,body=[
            render({image_panel,ImagePanel,Image},S),
            #panel{class= <<"post-message">>,body=P#post.message_escaped}
            ]}}.

event(#add{target=post,feed=Type,forms=[TopicOrSage,Input,Selector]}) when Type =:= thread orelse Type =:= post ->
    wf:info(?M,"Adding post",[]),
    M=wf:q(Input),
    {T,IsSage}=case {Type,wf:q(TopicOrSage)} of
        {thread,X} -> {X,false};
        {post,<<"true">>} -> {<<>>,true};
        {post,_}          -> {<<>>,false}
    end,
    wf:info(?M,"Sage: ~p",[IsSage]),
    View=case wf:q(Selector) of
        <<"on">> ->
            wf:wire("qs('#input .post-image').classList.add('thumbnail');"), [{image,thumbnail}];
        _ -> [{image,fullwidth}] end,
        

    CheckM=size(M) =< config:post_max_length(),
    CheckT=case Type of thread -> size(T) =< config:topic_max_length(); post -> true end,

    case {CheckT,CheckM} of
        {false,_} -> wf:wire("alert('too looooong topic');");
        {_,false} -> wf:wire("alert('too looooong message');");
        {true,true} ->
            #st{board=#board{id=Bid},thread=St}=erlang:get(state),
            
            Tid=case Type of thread -> kvs:next_id(thread,1); post -> St#thread.id end,
            Pid=kvs:next_id(post,1),
            
            EscapedT=case Type of thread -> wf:to_binary(wf:html_encode(wf:to_list(T))); post -> <<>> end,
            EscapedM=wf:to_binary(wf:html_encode(wf:to_list(M))),
            
            Rst=erlach_utils:rst(), % #rst.image for overwriting image in post
            Image=case Rst#rst.image of
                #ftp{filename=RelPath,size=Size,meta={meta,_,Width,Height}} ->
                    erlach_image:target(wf:to_binary(filename:join(root(),RelPath)),{element,{post,image},Pid}),
                    erlach_utils:rst(Rst#rst{image=?UNDEF}),
                    {ok,#attachment{id=Aid}}=kvs:add(#attachment{id=kvs:next_id(attachment,1),
                        feed_id={attachment,Type},
                        created=erlang:system_time(),original_path=RelPath,path=erlach_image:path(RelPath),
                        original_mime= <<>>,
                        view=View,size=Size,width=Width,height=Height}),
                    Aid;
                _ -> ?UNDEF
            end,
            Element=case Type of % TODO: move to KVS FEED SERVER
                thread ->
                    {ok,Thread}=kvs:add(#thread{id=Tid,feed_id={thread,Bid},
                        created=erlang:system_time(), urn=erlach_utils:id_to_urn(Tid),
                        name=T,name_escaped=EscapedT,message=M,message_escaped=EscapedM,image=Image}),
                    Thread;
                post ->
                    {ok,Post}=kvs:add(#post{id=Pid,feed_id={post,Tid},created=erlang:system_time(),
                        urn=erlach_utils:id_to_urn(Pid),message=M,message_escaped=EscapedM,image=Image}),
                    case IsSage of
                        true -> sage;
                        false ->
                            {ok,ThreadFeed}=kvs:get(feed,{thread,Bid}),
                            {ok,Thread}=kvs:get(thread,Tid),
                            kvs:relink(ThreadFeed,Thread,#kvs{mod=?DBA}), % unlink
                            kvs:link(Thread)
                    end, Post
            end,
            
            wf:wire("removeFromLocalStorage();"), % clean
            wf:wire(#jq{target=Input,method=["removeAttribute"],args=["'contentEditable'"]}),
            case Type of thread ->
                wf:wire(#jq{target=TopicOrSage,method=["removeAttribute"],args=["'contentEditable'"]}); post -> skip end,
            wf:wire("qs('#input .input-selector').remove();"),
            wf:wire("qs('#input label').removeAttribute('for');"),
            wf:wire("qs('.input-controls').remove();"),
            wf:wire(#jq{target=input,property=className,right=case Type of thread -> "thread"; post -> "post" end}),
            wf:wire(#jq{target=input,property=id,right=erlach_utils:id(Element)}),
            ImageContainer=erlach_utils:id({post,image},Pid),
            wf:wire(#jq{target= <<"input-image">>,property=id,right=ImageContainer}),
            
            case Type of
                thread -> wf:send({board,Bid},{server,#pubsub{action=add,data=Element,from=self()}});
                post -> wf:send({thread,Tid},{server,#pubsub{action=add,data=Element,from=self()}}) end
    end;
event(#pubsub{action=add,data=#post{feed_id={post,Tid}}=P,from=Proc}) ->
    wf:info(?M,"RECEIVE add #post{} ~p",[self()]),
    case self() of
        Proc ->
            % wf:insert_bottom(<<"posts-new">>,render(P,#hes{},erlang:get(state)));
            % wf:insert_bottom(<<"content">>,render(input,erlang:get(state)));
            wf:info(?M,"Cx: ~p",[(get(context))=:=undefined]),
            
            wf:wire(#jq{target= <<"posts-new">>,method=["removeAttribute"],args=["'id'"]}),
            erlach_utils:hidden(), % clear
            wf:remove('posts-new-controls'),
            wf:insert_bottom(content,#panel{id= <<"posts-new">>}),
            wf:insert_bottom(content,render('posts-new-controls',erlang:get(state))),
            wf:insert_bottom(content,#media_input{id= <<"input">>,image= <<"input-image">>,type=post});
        _ ->
            % wf:insert_bottom(<<"posts-new">>,render(P,#hes{option=hidden},erlang:get(state))),
            S=erlang:get(state),
            wf:insert_bottom(<<"posts-new">>,render(P,#hes{},S)),
            erlach_utils:hidden(erlach_utils:id(P)),
            wf:update('posts-new-controls',render('posts-new-controls',S))
            % erlach_utils:wire()
    end;
    
event(#view{target= <<"posts-new">>,element=Show}) -> % show new posts
    wf:wire(#jq{target= <<"posts-new">>,method=["removeAttribute"],args=["'id'"]}),
    erlach_utils:hidden(), % clear
    wf:update('posts-new-controls',[#panel{id= <<"posts-new">>},render('posts-new-controls',erlang:get(state))]);
    % [wf:show(H) || H <- erlach_utils:hidden() ];
event(#render_event{target=input,event={sage,Boolean,Sage}}) when is_boolean(Boolean) ->
    wf:update(Sage,#button{id=Sage,class=case Boolean of true -> <<"checked">>; false -> <<>> end,
        postback=#render_event{target=input,event={sage,not Boolean,Sage}},
        body= <<"Sage">>,value=Boolean});
event({server,{upload_state,ImageContainer,Text}}) ->
    wf:wire("uploadFileShowState('"++wf:to_list(ImageContainer)++"','"++wf:to_list(Text)++"');");
event(#ftp{sid=Sid,status={event,init},filename=RelPath,meta={meta,ImageContainer,_Width,_Height},size=Size}=Ftp) ->
    wf:info(?M,"IMAGE INIT ~p ~p ~p",[RelPath,Size,ImageContainer]),
    case (erlach_utils:rst())#rst.image of
        #ftp{filename=Preview} ->
            FileName=filename:join(root(),wf:to_list(erlach_image:path(Preview))),
            wf:info(?M,"Remove temp ~p",[FileName]),
            file:delete(FileName);
        Skip -> wf:info(?M,"Skip temp ~p",[Skip])
    end,
    erlach_utils:rst((erlach_utils:rst())#rst{image=Ftp});
event(#ftp{sid=Sid,status={event,stop},filename=RelPath,meta={meta,ImageContainer,_Width,_Height},size=Size}) ->
    wf:info(?M,"IMAGE ~p ~p ~p",[RelPath,Size,ImageContainer]),
    Pid=self(),
    FilePath=filename:join(root(),RelPath),
    wf:info(?M,"IMAGE ~p",[1]),    
    Pid ! {server,{upload_state,ImageContainer,"Processing"}},
    wf:info(?M,"IMAGE ~p",[(erlang:get(state))#st.route#route.render]),
    
    Group=case erlang:get(state) of
        #st{route=#route{render=erlach_board},board=#board{id=Bid}} -> {board,Bid};
        #st{route=#route{render=erlach_thread},thread=#thread{id=Tid}} -> {thread,Tid}
    end,
    wf:info(?M,"IMAGE ~p",[Group]),
    
    erlach_image:convert(FilePath,Group,{container,ImageContainer});
event(#pubsub{action=bpg,data=BpgPath,from=From,target=Target}=E) ->
    wf:info(?M,"RECEIVE converted image ~p ~p",[self(),E]),
    case Target of
        % self
        {container,ImageContainer} when self() =:= From ->
            #bpg_info{picture_width=BpgW,picture_height=BpgH}=send_bpg(ImageContainer,BpgPath),
            wf:wire(wf:q("setCanvasSize('~s',~b,~b)",[ImageContainer,BpgW,BpgH]));
        % page reloaded and post not stored
        {container,ImageContainer} -> skip;
        % all (stored)
        {element,Key,Id} ->
            ImageContainer=erlach_utils:id(Key,Id),
            #bpg_info{picture_width=BpgW,picture_height=BpgH}=send_bpg(ImageContainer,BpgPath),
            wf:wire(wf:q("setCanvasSize('~s',~b,~b)",[ImageContainer,BpgW,BpgH]))
    end;
event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).

root() -> wf:config(n2o,upload,code:priv_dir(n2o)).
send_bpg(ImageContainer,BpgPath) ->
    case file:read_file(BpgPath) of
        {ok,Bin} ->
            wf:info(?M,"SEND BPG ~p ~p",[self(),byte_size(Bin)]),
            {ok,BpgInfo}=erlach_image:bpg_info({data,Bin}),
            self() ! #ftp{status= <<"relay">>,meta=wf:to_list(ImageContainer),data=Bin},
            BpgInfo;
        Error -> Error
    end.