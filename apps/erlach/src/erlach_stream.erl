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

render(content=Panel,#st{board=#board{id=Bid,name=Name,desc=Desc}=B}=S) ->
    wf:info(?M,"Board: ~p",[Bid]),
    Elements=case kvs:get(feed,{attachment,Bid}) of
        {ok, #feed{top=Top,count=Count}} ->
            kvs:fold(fun(A,Acc) -> [render(A,#hes{board=B},S) | Acc] end,[],attachment,Top,Count,#iterator.prev,#kvs{mod=?DBA});
        _ -> []
    end,    
    #panel{id=Panel,body=[
        #panel{class= <<"content-title">>,body=[#span{class=section,body= <<"ImagƎ StяƎam"/utf8>>},Name]},
        #span{class= <<"remark">>,body=Desc},
        render(controls,S),
        #panel{id= <<"images">>, body=lists:reverse(Elements)}
    ]};
render(controls=Panel,#st{board=B}) ->
    #panel{id=Panel,class= <<"center">>,body=[
        #button{class=black,body= <<"Back to board">>,postback=erlach_qs:mp({board,B})}
        ]}.

render(#attachment{id=Aid}=A,#hes{board=B},#st{}=S) ->
    #panel{id=spa:id(A),body=[
        erlach_thread:render({imagePanel,spa:id({image,Aid}),Aid},S)
    ]}.

event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).
