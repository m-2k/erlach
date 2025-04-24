-module(erlach_main).
-compile(export_all).
-author('Andy').

-include("erlach.hrl").

title(#st{}) -> wf:to_binary(wf:config(erlach,title,"Erlach")).
urn() -> ?UNDEF.

init(#route{}=Route) ->
    wf:info(?M,"init",[]),
    {ok,#st{route=Route,action=view}}.
finalize(#st{}) -> ok.
terminate() -> wf:info(?M,"TERMINATE ~p",[self()]).

render(content=Panel,#st{}) ->
    #panel{id=Panel,body=[board_list(),last_updates()]}.

board_list() ->
    #panel{class= <<"board-list">>,
        body=lists:foldl(fun(#party{hidden=false,id=Gid,name=Gname,desc=Gdesc},Acc) ->
                BoardList=kvs:entries(kvs:get(feed,{board,Gid}),board,undefined),
                GroupButton=#panel{class= <<"inline b black inactive">>,title=Gdesc,body=Gname},
                BoardButtonList=lists:foldl(fun(#board{hidden=false}=B,Acc2) ->
                        GetCount=fun(Type) -> case kvs:get(id_seq,{anal,{Type,board,B#board.id}}) of
                            {ok,#id_seq{id=Count}} -> Count;
                            _ -> 0
                        end end,
                        PostCount=GetCount(thread)+GetCount(post),
                        BoardPanel=#panel{class= <<"inline">>,body=[
                            #a{class= <<"b blue">>,title=B#board.name,
                                href=erlach_qs:ml({board,B}),
                                body=[B#board.urn," ",#span{class=count,body=wf:to_list(PostCount)}],%[$/,B#board.urn,$/],
                                postback=erlach_qs:mp({board,B})
                                }]},
                        [BoardPanel|Acc2];
                    (#board{},Acc2) -> Acc2
                end,[],BoardList),
                [#panel{class= <<"group">>,body=[GroupButton|BoardButtonList]}|Acc];
            (#party{},Acc) -> Acc
        end,[],kvs:entries(kvs:get(feed,party),party,undefined))}.

last_updates() ->
    S=spa:st(),
    Elements=[ begin
        {ok,#post{board=Bid}=T}=kvs:get(post,Tid),
        {ok,#board{feed_id={board,PartyId}}=B}=kvs:get(board,Bid),
        {ok,B}=kvs:get(board,Bid),
        case kvs:get(party,PartyId) of
            {ok,#party{hidden=false}} -> [erlach_board:render(T,#hes{board=B},S),erlach_board:render(last,#hes{board=B,thread=T},S)];
            _ -> []
        end
    end || Tid <- last_threads() ],

    #panel{class= <<"last-updates">>,body=[
        #panel{class=remark,body= <<"Last updates">>},
        #panel{id=threads,body=Elements}
    ]}.

last_threads() ->
    Last=case kvs:get(statistic,{threads,recent_activity}) of
        {ok,#statistic{value=List}} -> List;
        _ ->
            List=calculate_last_threads(wf:config(erlach,threads_recent_activity_count,100)),
            kvs:put(#statistic{id={threads,recent_activity},value=List}),
            List
    end,
    {First,_}=spa_utils:ensure_split(wf:config(erlach,main_last_count,7),Last),
    First.
calculate_last_threads(Count) ->
    All=[ case Type of thread -> {Id,C}; post -> {Parent,C} end || #post{type=Type,id=Id,feed_id={_,Parent},created=C} <- kvs:all(post) ],
    Map=lists:foldl(fun({Id,C},A) -> maps:update_with(Id, fun(C0) when C0 > C -> C0; (_) -> C end, C, A) end, #{}, All),
    Sorted=lists:keysort(2,maps:to_list(Map)),
    {LastList,_}=spa_utils:ensure_split(Count,lists:reverse(Sorted)),
    [ Id || {Id,_} <- LastList ].

event(#view{target=sidebar,option=Visibled,element=Btn}=E) ->
    wf:update(Btn,#a{id=Btn,class=case Visibled of true -> [selector,checked]; false -> selector end,
        body= <<"Notify">>,postback=E#view{option=not Visibled}}),
    wf:wire(wf:f("qi('sidebar').dataset.visibled=~s;",[Visibled])); % data-* for safari supports

event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).