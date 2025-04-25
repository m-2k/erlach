-module(erlach_board).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").

title(#st{board=#board{name=Name}}) -> <<Name/binary," – Erlach"/utf8>>.
urn() -> ?URN_PAGE_DYNAMIC.

access(#board{view=V}) ->
    {Origin,_Req}=cowboy_req:header(<<"origin">>,?REQ),
    ViewTor=spa:option(tor_only,V),
    wf:warning(?M,"Board Tor: ~p, (origin: ~p)",[ViewTor,Origin]),
    Access=case {ViewTor,Origin} of
        {true,<<"http://",_:128,".onion">>} -> allow;
        {true,<<"https://",_:128,".onion">>} -> allow;
        {true,_} -> deny;
        _ -> allow
    end.
    
get_board(Urn) ->
    case kvs:index(board,urn,Urn) of
        [#board{id=Bid}=B] ->    
            case erlach_board:access(B) of
                allow -> B;
                Deny -> Deny
            end;
        _ -> not_found
    end.

init(#route{board=Burn}=Route) ->
    wf:info(?M,"init",[]),
    case get_board(Burn) of
        #board{id=Bid}=B ->
            wf:reg({board,Bid}),
            {ok,#st{user=eauth_user:get(),route=Route,action=view,board=B}};
        _ -> {redirect,erlach_qs:mp(main)}
    end.
finalize(#st{}) -> ok.
terminate() ->
    wf:info(?M,"TERMINATE ~p",[self()]),
    erlach_thread:convert_clear_pending(),
    #st{board=#board{id=Bid}}=spa:st(),
    wf:unreg({board,Bid}),
    wf:info(?M,"TERMINATE ~p OK",[self()]).
    

guard_addition(#st{access=full}) -> true;
guard_addition(#st{board=#board{feed_id={board,PartyId}}}) ->
    case kvs:get(party,PartyId) of {ok,#party{type=services}} -> false; _ -> true end;
guard_addition(_) -> true.

render(content=Panel,#st{board=#board{id=Bid,name=Name,desc=Desc}=B}=S) ->
    wf:info(?M,"Board: ~p ~p",[Bid,guard_addition(S)]),
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

render(controls=Panel,#st{board=#board{feed_id={board,PartyId}}=B}=S) ->
    #panel{id=Panel,class= <<"center">>,
        actions="ls(['opt','catalog']) && qi('content').classList.add('catalog');",
        body=[
            case guard_addition(S) of
                true ->
                    #button{class=black,body=[
                        #span{class=ru,body= <<"Создать тред"/utf8>>},
                        #span{class=en,body= <<"Create thread"/utf8>>},
                        #span{class=ua,body= <<"Створити тред"/utf8>>} ],
                        postback=#create{target=thread}};
                false -> []
            end,
            #button{class=[black,<<"action-catalog">>],
                body=[#span{class=ru,body= <<"Каталог"/utf8>>},
                    #span{class=en,body= <<"Catalog"/utf8>>},
                    #span{class=ua,body= <<"Каталог"/utf8>>}],
                title= <<"Thread-view mode selector"/utf8>>,
                actions=#bind{target={qs,["[id='",Panel,"'] button.action-catalog"]},
                    type=click,postback="ls(['opt','catalog'],true);qi('content').classList.add('catalog');"}
                },
            #button{class=[black,<<"action-catalog">>,checked],
                body=[#span{class=ru,body= <<"Каталог"/utf8>>},
                    #span{class=en,body= <<"Catalog"/utf8>>},
                    #span{class=ua,body= <<"Каталог"/utf8>>}],
                title= <<"Thread-view mode selector"/utf8>>,
                actions=#bind{target={qs,["[id='",Panel,"'] button.action-catalog.checked"]},
                    type=click,postback="lsrem(['opt','catalog']);qi('content').classList.remove('catalog');"}
                },
            #a{class=[b,sea],body=[
                    #span{class=ru,body= <<"Стрим картинок"/utf8>>},
                    #span{class=en,body= <<"Image stream"/utf8>>},
                    #span{class=ua,body= <<"Стрім зображень"/utf8>>}  ],
                postback=erlach_qs:mp({stream,B}),href=erlach_qs:ml({stream,B})}
            ]}.

render(last,#hes{board=#board{}=B,thread=#post{id=Tid}=T}=Hes,#st{}=S) ->
    LastPostsCount=wf:config(erlach,last_posts_count,3),
    Elements=case kvs:get(feed,{post,Tid}) of
        {ok, #feed{count=C}=F} ->
            LastPosts=kvs:entries(F,post,LastPostsCount),
            Info=case C > LastPostsCount of
                true -> #panel{class=omitted,body=#panel{class= <<"post-content">>,body=[
                        % #panel{class= <<"post-message">>,body=[wf:to_list(C-LastPostsCount),<<" messages omitted">>]}]}};
                        #panel{class= <<"post-message">>,body=[
                            wf:to_list(C-LastPostsCount),
                            ?TR(<<" ответов пропущено"/utf8>>,
                                <<" replies omitted"/utf8>>,
                                <<" відповідей пропущено"/utf8>>)]}]}};
                false -> []
            end,
            Hes2=Hes#hes{thread=T,board=B},
            % spa:info(wf:jse(wf:to_list(spa:option(limit,Hes2,"undef")))),
            LastElements=[ render(P,Hes2,S) || P <- LastPosts ],
            [Info,LastElements];
        _ -> []
    end,
    #panel{id=[erlach_utils:post_id(T),"-last"],class=last,body=Elements};

render({'post-header',#post{type=thread,id=Tid}=T},#hes{board=#board{}=B}=Hes,#st{}=S) ->
    #panel{id=spa:id({'post-header',Tid}),body=#panel{class= <<"post-header">>,body=[
        #a{class= <<"post-topic">>,href=erlach_qs:ml({thread,B,T}),postback=erlach_qs:mp({thread,B,T}),body=erlach_thread:topic(T)},
        render({'post-info',T},Hes,S)
        ]}};

render({'post-header',#post{type=post}=P},#hes{}=Hes,#st{}=S) ->
    [];

render({'post-info',#post{id=Tid,type=thread}=T},#hes{board=#board{}=B}=Hes,#st{}=S) ->
    wf:info(?M,"post-info: ~p",[Tid]),
    PostCount=erlach_thread:count(Tid),
    Limit=erlach_thread:limit(wf:coalesce([B,S#st.board])),
    [
        case PostCount of 0 -> []; Pc -> #span{class= <<"b info info-posts-count">>,
            body=[?TR(<<"П: "/utf8>>,<<"P: "/utf8>>,<<"П: "/utf8>>),wf:to_binary(Pc)]} end,
        case erlach_stat:count({attachment,thread,Tid}) of
            0 -> [];
            ImCount -> #span{class= <<"b info info-images-count">>,body=[
                ?TR(<<"К: "/utf8>>,<<"I: "/utf8>>,<<"З: "/utf8>>),wf:to_binary(ImCount)]}
        end
    ];

render(#post{type=thread,id=Pid,urn=Urn,image=Image}=T,#hes{board=#board{}}=Hes,#st{}=S) ->
    Panel=erlach_utils:post_id(T,S),
    Hes2=Hes#hes{thread=T,post=T},
    Limit=spa:option(limit,Hes,wf:config(erlach,board_thread_char_limit,200)),
    
    #panel{id=Panel,class=[post],
        actions=["render('",Panel,"');"],
        data_fields=[{<<"data-id">>,Urn}],
        body=[
            erlach_thread:render({'post-manage',T},Hes2,S),
            #panel{class= <<"post-content">>,body=[
                #panel{class= <<"post-flash">>,body=[
                    erlach_thread:render({imagePanel,Image},Hes2,S),
                    render({'post-header',T},Hes2,S),
                    #panel{class= <<"post-message">>,body=[
                        erlach_thread:render(message,spa:setoption(limit,Limit,Hes2),S)
                    ]}
                ]}
            ]}
    ]};

render(#post{type=post,image=Image,urn=Urn,sage=Sage}=P,#hes{board=B,thread=T,option=O}=Hes,#st{}=S) ->
    Panel=erlach_utils:post_id(P,S),
    Hes2=Hes#hes{post=P},
    Limit=spa:option(limit,Hes,wf:config(erlach,board_post_char_limit,100)),
    #panel{id=Panel,class=[post,case Sage of true -> sage; _ -> [] end],
        actions=["render('",Panel,"');"],
        data_fields=[{<<"data-id">>,Urn}],
        body=[
            erlach_thread:render({'post-manage',P},Hes2,S),
            #panel{class= <<"post-content latest">>,body=[
                #panel{class= <<"post-flash">>,body=[
                    erlach_thread:render({imagePanel,Image},Hes,S),
                    #panel{class= <<"post-message">>,body=[
                        erlach_thread:render({'post-header',P},Hes2,S),
                        erlach_thread:render(message,spa:setoption(limit,Limit,Hes2),S),
                        case spa:option('to-thread-link',Hes) of
                            true ->
                                #a{class=goto,body= <<"⎆"/utf8>>,title= <<"To thread"/utf8>>,
                                    href=erlach_qs:ml({post,B,T,P}),postback=erlach_qs:mp({post,B,T,P})};
                            _ -> []
                        end
                    ]},
                    erlach_thread:render(links,#hes{post=P},S),
                    #panel{class= <<"hidden-info">>,body=?TR(<<"Скрыто"/utf8>>,<<"Hidden"/utf8>>,<<"Приховано"/utf8>>)}
                ]}
            ]}
    ]}.

event(#delete{target=post}=E) -> erlach_thread:event(E);
event(#delete{target=thread}=E) -> erlach_thread:event(E);
event(#pubsub{target=content,action=delete,data=#post{type=thread}=T,from=Proc}) -> % threads
    wf:info(?M,"RECEIVE delete #thread{} ~p",[self()]),
    wf:remove(erlach_utils:post_id(T));

event(#pubsub{target=content,action=add,data=#post{type=thread}=T,from=Proc}) ->
    wf:info(?M,"RECEIVE add #post{type=thread} ~p",[self()]),
    
    #st{board=#board{}=B}=S=spa:st(),
    Panel=erlach_utils:post_id(T),
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
    
event(#create{target=thread}=E) ->
    case guard_addition(spa:st()) of
        true -> wf:update(controls,#media_input{id=input,target=thread});
        false -> wf:warning(?M,"Access denied (add board, input form) ~p ~p",[self(),E])
    end;
event(#cancel{target=input,panel=Panel}) -> wf:update(Panel,render(controls,spa:st()));
event(#add{target=thread}=E) ->
    case guard_addition(spa:st()) of
        true -> erlach_thread:event(E);
        false -> wf:warning(?M,"Access denied (add board) ~p ~p",[self(),E])
    end;
event(#ftp{}=Ftp) ->
    wf:info(?M,"FTP: ~p",[Ftp]),
    erlach_thread:event(Ftp);
event(#pubsub{target=image,action=convert}=E) -> erlach_thread:event(E);
event({server,{upload_state,ImageContainer,Text}}) ->
    wf:wire("uploadFileShowState('"++wf:to_list(ImageContainer)++"','"++wf:to_list(Text)++"');");
event(#view{target=catalog,option=show}) ->
    wf:wire(spa:add_class(threads,[catalog]));
event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).
