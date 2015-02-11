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

% -ifndef(SESSION).
% -define(SESSION, (wf:config(n2o,session,erlach_session))).
% -endif.

% peer()    -> io_lib:format("~p",[wf:peer(?REQ)]).
main()    -> #dtl{file="erlach",app=erlach,bindings=[{body,body()}, {theme,<<"glassy">>}, {title,<<"Erlach">>}]}. %html:page(<<"Erlach">>,body(),glassy).
body() ->
    % wf:info(?MODULE, "body ~p", [User]),
    Boards = case kvs:get(feed, {board,1}) of
        {ok,Bf} ->
            kvs:traversal(board, Bf#feed.top, Bf#feed.entries_count, #iterator.prev);
        _ -> []
    end,
    Html = lists:map(fun(#board{ id=Id, uri=Uri, name=Name }) ->
        #panel{ class = <<"board">>, body = [
            #link{ class= <<"button girl alpha">>, href = qs:ml({board,Id}), body = [
                #span{class= <<>>,body=wf:f("~s", [Uri])},
                wf:html_encode(wf:f(" - ~s", [Name]))] }
        ]}
        end, lists:reverse(Boards)), %kvs:all(board)),
    html:body(#panel{ class= <<"board-list">>, body=
            % #span{ body=wf:f("Session: ~p<br/>User: ~p<br/>User 2: ~p", [Session,User,User2])},
            % [Boards,#span{ class= <<"helo">>, body = <<"Coming soon!">>}]}).
            Html}).
            %"<svg xmlns='http://www.w3.org/2000/svg' version='1.1'><defs><filter id='blur'><feGaussianBlur stdDeviation='5'/></filter></defs></svg>"]},

event(init) ->
    % self() ! {server, "ololo"},
    % self() ! {custom, "ololo"},
    % self() ! {server, "ololo", 3},
    % self() ! {server, {temp_user_created,undefined}},
    wf:info(?MODULE, "User: ~p WS Pid: ~p, State ~p", [wf:user(), self(), get(state)]);
% event(join) ->
%   u:join(),
%   wf:info(?MODULE, "User: ~p", [wf:user()]);
event({server, {temp_user_created,User}}) ->
    wf:info(?MODULE, "SERVER EVENT User: ~p WS Pid: ~p, State ~p", [u:id(User), self(), get(state)]),
    html:info("Clear browser cache if U have problem"),
    html:warning("Development version");
event({server, Message}) -> % OK
    wf:info(?MODULE, "Server message: ~p", [Message]);
event({server, Message, M}) ->
    wf:info(?MODULE, "Server3 message: ~p ~p", [Message,M]); %% NOT WORK, USE {server, Val}
event({custom, Message}) ->
    wf:info(?MODULE, "Custom message: ~p", [Message]);  %% NOT WORK, USE {server, Val}
event(terminate) -> skip;
event(Event) -> guard:shared_event(?MODULE, Event).

% event(#ev{msg={chat,Pid}},#cx{req=_Req}=Cx) -> Pid ! {peer(), wf:qp(message,Cx)};
% event(_Event,_) -> skip.
