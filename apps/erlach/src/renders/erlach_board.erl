-module(erlach_board).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").

title(#st{board=#board{name=Name}}) -> <<Name/binary," – Erlach"/utf8>>.
urn() -> ?UNDEF.

init(#route{board=Burn}=Route) ->
    wf:info(?M,"init",[]),
    case kvs:index(board,urn,Burn) of
        [#board{id=Bid}=B] ->
            wf:reg({board,Bid}),
            {ok,#st{route=Route,action=view,board=B}};
        _ -> {redirect,erlach_qs:mp(main)}
    end.
finalize(#st{}) -> ok.
terminate() ->
    wf:info(?M,"TERMINATE ~p",[self()]),
    #st{board=#board{id=Bid}}=spa:st(),
    wf:unreg({board,Bid}),
    wf:info(?M,"TERMINATE ~p OK",[self()]).

render(content=Panel,#st{board=#board{id=Bid,name=Name,desc=Desc}=B}=S) ->
    wf:info(?M,"Board: ~p",[Bid]),
    
    Elements=case kvs:get(feed,{thread,Bid}) of
        {ok, #feed{top=Top,count=Count}} ->
            kvs:fold(fun(T,Acc) ->
                [[render(T,#hes{board=B},S),render(last,#hes{board=B,thread=T},S)] | Acc]
            end,[],post,Top,Count,#iterator.prev,#kvs{mod=?DBA});
        _ -> []
    end,    
    #panel{id=Panel,body=[
        #panel{class= <<"content-title">>,body=Name},
        #span{class= <<"remark">>,body=Desc},
        #panel{id= <<"threads">>, body=[render(controls,S),lists:reverse(Elements)]}
    ]};
render(controls=Panel,#st{board=B}) ->
    #panel{id=Panel,class= <<"center">>,body=[
        #button{class=black,body= <<"New thread">>,postback=#create{target=thread}},
        #a{class=[b,sea],body= <<"Image stream">>,postback=erlach_qs:mp({stream,B}),href=erlach_qs:ml({stream,B})}
        ]}.

render(last,#hes{board=#board{}=B,thread=#post{id=Tid}=T}=Hes,#st{}=S) ->
    LastPostsCount=wf:config(erlach,last_posts_count,3),
    Elements=case kvs:get(feed,{post,Tid}) of
        {ok, #feed{count=C}=F} ->
            LastPosts=kvs:entries(F,post,LastPostsCount),
            Info=case C > LastPostsCount of
                true -> #panel{class=omitted,body=#panel{class= <<"post-content">>,body=[
                        #panel{class= <<"post-message">>,body=[wf:to_list(C-LastPostsCount),<<" messages omitted">>]}]}};
                false -> []
            end,
            LastElements=[ render(P,#hes{thread=T,board=B},S) || P <- LastPosts ],
            [Info,LastElements];
        _ -> []
    end,
    #panel{id=spa:id({last,Tid}),class=last,body=Elements};
render({'post-header',#post{type=thread,id=Tid}=T},#hes{board=#board{}=B}=Hes,#st{}=S) ->
    #panel{id=spa:id({'post-header',Tid}),body=#panel{class= <<"post-header">>,body=[
        #a{class= <<"post-topic">>,href=erlach_qs:ml({thread,B,T}),postback=erlach_qs:mp({thread,B,T}),body=erlach_thread:topic(T)},
        render({'post-info',T},Hes,S),
        render({'post-manage',T},Hes,S)
        ]}};
render({'post-header',#post{type=post}=P},#hes{}=Hes,#st{}=S) ->
    render({'post-manage',P},Hes,S);
render({'post-manage',#post{type=Type,id=Id}=P},#hes{},#st{access=full}) ->
    #a{class=[b,warn],body= <<"Remove">>,postback=#delete{target=Type,value=Id}};
render({'post-manage',#post{}},#hes{},#st{}) -> [];
render({'post-info',#post{id=Tid,type=thread}=T},#hes{board=#board{}=B}=Hes,#st{}=S) ->
    wf:info(?M,"post-info: ~p",[Tid]),
    Link=#a{class=[b,sea],href=erlach_qs:ml({thread,B,T}),postback=erlach_qs:mp({thread,B,T}),body= <<"Reply">>,title= <<"Reply or view thread">>},
    PostCount=erlach_thread:count(Tid),
    Limit=erlach_thread:limit(wf:coalesce([B,S#st.board])),
    [
        case erlach_thread:is_read_only(PostCount,Limit) of true -> Link#a{body= <<"View">>,title= <<"View thread">>}; false -> Link end,
        case PostCount of 0 -> []; Pc -> #span{class= <<"b info info-posts-count">>,body=[wf:to_binary(Pc),<<" posts">>]} end,
        case erlach_stat:count({attachment,thread,Tid}) of
            0 -> [];
            ImCount -> #span{class= <<"b info info-images-count">>,body=[wf:to_binary(ImCount),<<" images">>]}
        end
    ];
render(#post{type=thread,id=Pid,image=Image}=T,#hes{board=#board{}}=Hes,#st{}=S) ->
    Panel=spa:id(T),
    Hes2=Hes#hes{thread=T,post=T},
    Limit=wf:config(erlach,board_char_limit,300),
    #panel{id=Panel,class= <<"post">>,
        body=#panel{class= <<"post-content">>,body=[
            erlach_thread:render({imagePanel,Image},Hes2,S),
            render({'post-header',T},Hes2,S),
            #panel{class= <<"post-message">>,body=[
            erlach_thread:render(message,spa:setoption(limit,Limit,Hes2),S)
            ]}
            ]}};
render(#post{type=post,image=Image,sage=Sage}=P,#hes{board=B,thread=T}=Hes,#st{}=S) ->
    Panel=spa:id(P),
    Hes2=Hes#hes{post=P},
    Limit=wf:config(erlach,board_char_limit,300),
    #panel{id=Panel,class=[post,case Sage of true -> sage; _ -> [] end],
        body=#panel{class= <<"post-content latest">>,body=[
            erlach_thread:render({imagePanel,Image},Hes,S),
            #panel{class= <<"post-message">>,body=[
                erlach_thread:render({'post-header',P},Hes2,S),
                erlach_thread:render(message,spa:setoption(limit,Limit,Hes2),S)
            ]},
            erlach_thread:render(links,#hes{post=P},S)
    ]}}.

event(#delete{target=post}=E) -> erlach_thread:event(E);
event(#delete{target=thread}=E) -> erlach_thread:event(E);
event(#pubsub{target=content,action=delete,data=#post{type=thread}=T,from=Proc}) -> % threads
    wf:info(?M,"RECEIVE delete #thread{} ~p",[self()]),
    wf:remove(spa:id(T));

event(#pubsub{target=content,action=add,data=#post{type=thread}=T,from=Proc}) ->
    wf:info(?M,"RECEIVE add #post{type=thread} ~p",[self()]),
    
    #st{board=#board{}=B}=S=spa:st(),
    Panel=spa:id(T),
    case self() of
        Proc ->
            Target="qs('[id=\""++Panel++"\"] .post-topic')",
            Content=[ render({'post-header',T},#hes{board=B},S) ],
            wf:wire(#jq{target=Target,property=outerHTML,args=simple,right=Content,format="'~s'"});
        _ ->
            wf:insert_adjacent(afterend,controls,render(T,#hes{board=B},S))
    end,
    wf:insert_adjacent(afterend,Panel,render(last,#hes{board=B,thread=T},S));
event(#pubsub{target=content,action=update,data=#post{id=Tid,type=thread}=T,from=_Proc}=E) ->
    Panel=spa:id({'post-header',Tid}),
    #st{board=B}=S=spa:st(),
    wf:update(Panel,render({'post-header',T},#hes{board=B},S));
    
event(#create{target=thread}) ->
    wf:update(controls,#media_input{id=input,target=thread});
event(#cancel{target=input,panel=Panel}) -> wf:update(Panel,render(controls,spa:st()));
event(#add{target=thread}=E) -> erlach_thread:event(E);
event(#ftp{}=Ftp) ->
    wf:info(?M,"FTP: ~p",[Ftp]),
    erlach_thread:event(Ftp);
event(#pubsub{target=image,action=convert}=E) -> erlach_thread:event(E);
event({server,{upload_state,ImageContainer,Text}}) ->
    wf:wire("uploadFileShowState('"++wf:to_list(ImageContainer)++"','"++wf:to_list(Text)++"');");
event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).
