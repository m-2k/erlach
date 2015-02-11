-module(root). %% MAIN PAGE / BOARDS LISTING
-vsn('0.0.0').

-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/feed.hrl").
-include_lib("db/include/post.hrl").
-include_lib("db/include/attachment.hrl").

-include_lib("db/include/board.hrl").
-include_lib("db/include/user.hrl").
-include("erlach.hrl").

main() -> #dtl{file="erlach",app=erlach,bindings=[{body,body()}, {theme,<<"glassy">>}, {title,<<"Erlach">>}]}.
body() ->
    Boards = case kvs:get(feed, {board,1}) of
        {ok,Bf} ->
            kvs:traversal(board, Bf#feed.top, Bf#feed.entries_count, #iterator.prev);
        _ -> []
    end,
    RightList = #panel{ class= <<"board-list-right">>, body=[
        #panel{ body= #link{class= <<"button info">>,body= <<"Hack & Develop">>,href= <<"#">>} },
        #panel{ body= #link{class= <<"button info">>,body= <<"About Erlach">>,href= <<"#">>} },
        #panel{ body= #link{class= <<"button info">>,body= <<"Donate">>,href= <<"/donate">>} }
        ]},
    Html = lists:map(fun(#board{ id=Id, uri=Uri, name=Name }) ->
        #panel{ class = <<"board">>, body = [
            #link{ class= <<"button girl alpha">>, href = qs:ml({board,Id}), body = [
                #span{class= <<>>,body=wf:f("~s", [Uri])},
                wf:html_encode(wf:f(" - ~s", [Name]))] }
        ]} end, lists:reverse(Boards)),
    html:body([#panel{ class= <<"board-list">>, body=Html},RightList]).

event(init) ->
    % self() ! {server, "ololo"},
    % self() ! {custom, "ololo"},
    % self() ! {server, "ololo", 3},
    % self() ! {server, {temp_user_created,undefined}},
    wf:info(?MODULE, "User: ~p WS Pid: ~p, State ~p", [wf:user(), self(), get(state)]);
event({server, {temp_user_created,User}}) ->
    wf:info(?MODULE, "SERVER EVENT User: ~p WS Pid: ~p, State ~p", [u:id(User), self(), get(state)]),
    html:info("Clear browser cache if U have problem"),
    html:warning("Development version");
% event({server, Message}) -> % OK
%     wf:info(?MODULE, "Server message: ~p", [Message]);
% event({server, Message, M}) ->
%     wf:info(?MODULE, "Server3 message: ~p ~p", [Message,M]); %% NOT WORK, USE {server, Val}
% event({custom, Message}) ->
%     wf:info(?MODULE, "Custom message: ~p", [Message]);  %% NOT WORK, USE {server, Val}
event(terminate) -> skip;
event(Event) -> guard:shared_event(?MODULE, Event).
