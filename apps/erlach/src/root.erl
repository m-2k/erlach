-module(root). %% MAIN PAGE / BOARDS LISTING
-author('andy').
-vsn('0.0.0').

-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/feed.hrl").
-include_lib("db/include/post.hrl").
-include_lib("db/include/attachment.hrl").

-include_lib("db/include/group.hrl").
-include_lib("db/include/board.hrl").
-include_lib("db/include/user.hrl").
-include("erlach.hrl").

main() -> #dtl{file="erlach",app=erlach,bindings=[{body,body()}, {theme,<<"glassy">>}, {title,<<"Erlach">>}]}.
    
body() ->        
    GroupsHtml=case kvs:get(feed,group) of
        {ok,Gf} ->
            Groups=kvs:traversal(group, Gf#feed.top, Gf#feed.entries_count, #iterator.prev),
            lists:foldl(fun(#group{id=Gid,name=Gname,description=Gdesc},Acc) ->
                BoardsOfGroup=case kvs:get(feed,{board,Gid}) of
                    {ok,Bf} ->
                        Group=#panel{class= <<"inline">>,body=#link{class= <<"button light alpha nohover">>,title=Gdesc,body=Gname}},
                        Boards=lists:map(fun(#board{id=_Id,uri=Uri,name=Bname}) ->
                            #panel{class= <<"inline">>,body=[
                                #link{class= <<"button dark alpha">>,title=Bname,href=qs:ml({board,Uri}),body=
                                    <<"/",(Uri)/binary,"/">> }]}
                            end,kvs:traversal(board, Bf#feed.top, Bf#feed.entries_count, #iterator.prev)),
                        [lists:reverse(Boards),Group];
                    _ -> [] end,
                [#panel{class= <<"group">>,body=BoardsOfGroup}|Acc]
                end,[],Groups);
        _ -> [] end,
    html:body(#panel{ class= <<"board-list">>, body=GroupsHtml}).

event(init) ->
    wf:info(?MODULE, "User: ~p WS Pid: ~p, State ~p", [wf:user(), self(), get(state)]);
event({server, {temp_user_created,User}}) ->
    wf:info(?MODULE, "SERVER EVENT User: ~p WS Pid: ~p, State ~p", [u:id(User), self(), get(state)]),
    html:info("Clear browser cache if U have problem"),
    html:warning("Development version");
event(terminate) -> skip;
event(Event) -> guard:shared_event(?MODULE, Event).
