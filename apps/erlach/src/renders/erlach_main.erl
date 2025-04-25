-module(erlach_main).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").

-define(HES_PCL(Hes),spa:setoption(limit,99999,Hes)).

title(#st{}) -> wf:to_binary(wf:config(erlach,title,"Erlach")).
urn() -> ?URN_PAGE_MAIN.

init(#route{}=Route) ->
    wf:info(?M,"init",[]),
    {ok,#st{user=eauth_user:get(),route=Route,action=view}}.
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
    Count=wf:config(erlach,main_last_count,7),
    Elements=lists:foldl(fun(Tid,Acc) when length(Acc) < Count ->
        case kvs:get(post,Tid) of
            {ok,#post{board=Bid}=T} ->
                {ok,#board{feed_id={board,PartyId}}=B}=kvs:get(board,Bid),
                {ok,B}=kvs:get(board,Bid),
                case kvs:get(party,PartyId) of
                    {ok,#party{hidden=false}} ->
                        [[
                            erlach_board:render(T,?HES_PCL(#hes{board=B}),S),
                            erlach_board:render(last,?HES_PCL(#hes{board=B,thread=T}),S)
                        ]|Acc];
                    _ -> Acc
                end;
            _ -> Acc
        end;
        (_,Acc) -> Acc
    end,[],last_threads()),
    
    #panel{class= <<"last-updates">>,body=[
        #panel{class=remark,body=?TR(<<"Самое свежее, ням"/utf8>>,<<"Latest news, yum"/utf8>>,<<"Найсвіжіше"/utf8>>) },
        #panel{id=threads,body=lists:reverse(Elements)}
    ]}.

last_threads() ->
    case kvs:get(statistic,{threads,recent_activity}) of
        {ok,#statistic{value=List}} -> List;
        _ ->
            List=calculate_last_threads(wf:config(erlach,threads_recent_activity_count,100)),
            kvs:put(#statistic{id={threads,recent_activity},value=List}),
            List
    end.
calculate_last_threads(Count) ->
    All=[ case Type of thread -> {Id,C}; post -> {Parent,C} end || #post{type=Type,id=Id,feed_id={_,Parent},created=C} <- kvs:all(post) ],
    Map=lists:foldl(fun({Id,C},A) -> maps:update_with(Id, fun(C0) when C0 > C -> C0; (_) -> C end, C, A) end, #{}, All),
    Sorted=lists:keysort(2,maps:to_list(Map)),
    {LastList,_}=spa_utils:ensure_split(Count,lists:reverse(Sorted)),
    [ Id || {Id,_} <- LastList ].
    

event(#view{target=sidebar,option=Visibled,element=Btn}=E) ->
    wf:update(Btn,#a{id=Btn,class=case Visibled of true -> [selector,checked]; false -> selector end,
        body=[ #span{class=ru,body= <<"Ответы"/utf8>>},
            #span{class=en,body= <<"Replies"/utf8>>},
            #span{class=ua,body= <<"Повідомлення"/utf8>>} ],
        postback=E#view{option=not Visibled}}),
    wf:wire(wf:f("qi('sidebar').dataset.visibled=~s;",[Visibled])); % data-* for safari supports

event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).