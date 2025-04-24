-module(erlach_stream).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").

title(#st{board=#board{name=Name}}) -> <<"Stream: ",Name/binary," – Erlach"/utf8>>.
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

process_part(Start,#st{board=#board{}=B}=S) ->
    Count=70,
    LoaderPosition=25,
    
    Part=kvs:fold(fun(E,A) -> [E|A] end,[],attachment,Start,Count,#iterator.prev),

    Hes=#hes{board=B},
    {PartElements1,Loader,PartElements2,_}=lists:foldl(fun
        (E,{A1,Ldr,A2,C}) when C < LoaderPosition   -> {[render(E,Hes,S)|A1],Ldr,A2,C+1};
        (E,{A1,Ldr,A2,C}) when C =:= LoaderPosition -> {A1,E,A2,C+1};
        (E,{A1,Ldr,A2,C}) when C > LoaderPosition   -> {A1,Ldr,[render(E,Hes,S)|A2],C+1}
    end,{[],[],[],1},Part),
        
    LoaderElements=case length(Part) of
        Count ->
            LoaderId=erlach_utils:post_id(Loader),
            Actions=[
                #wire{actions=["lazyLoader=qi('",LoaderId,"');"]},
                #event{type=lazy,target=LoaderId,validation="this.removeEventListener('lazy',arguments.callee);",
                    postback=#view{target=attachment,element=element(#iterator.prev,hd(Part)),option=lazy}}
            ],
            render(Loader,spa:setoption(actions,Actions,Hes),S);
        _ -> []
    end,
    [PartElements1,LoaderElements,PartElements2].

render(content=Panel,#st{board=#board{id=Bid,name=Name,desc=Desc}=B}=S) ->
    wf:info(?M,"Board: ~p",[Bid]),
    
    Elements=case kvs:get(feed,{attachment,Bid}) of
        {ok,#feed{top=Top}} -> process_part(Top,S);
        _ -> []
    end,
        
    #panel{id=Panel,body=[
        #panel{class= <<"content-title">>,body=[#span{class=section,body= <<"ImagƎ StяƎam"/utf8>>},Name]},
        #span{class= <<"remark">>,body=Desc},
        #panel{id= <<"stream">>, body=[
            render(controls,S),
            #panel{id= <<"images">>, body=Elements}
        ]}
    ]};
render(controls=Panel,#st{board=B}) ->
    #panel{id=Panel,class= <<"center">>,body=[
        #a{class=[b,black],body= <<"Вернуться к доске"/utf8>>,href=erlach_qs:ml({board,B}),postback=erlach_qs:mp({board,B})}
        ]}.

render(#attachment{id=Aid}=A,#hes{board=B}=Hes,#st{}=S) ->
    Panel=erlach_utils:post_id(A),
    #panel{
        actions=[ "render('",Panel,"');",spa:option(actions,Hes,[]) ],
        id=Panel,
        class= <<"stream-image">>,
        body=erlach_thread:render({imagePanel,Aid},#hes{board=B},S) }.

event(#view{target=attachment,option=lazy,element=Prev}) ->
    wf:info(?M,"Lazy loading ~p",[Prev]),
    wf:insert_adjacent(beforeEnd,images,process_part(Prev,spa:st()));

event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).
