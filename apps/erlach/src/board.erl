-module(board). %% BOARD LISTENING
-author('andy').
-vsn('0.0.0').

-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/feed.hrl").
-include_lib("db/include/post.hrl").
-include_lib("db/include/attachment.hrl").

-include_lib("db/include/board.hrl").
-include_lib("db/include/thread.hrl").
-include("erlach.hrl").

-ifndef(SESSION).
-define(SESSION, (wf:config(n2o,session,erlach_session))).
-endif.

main() ->
    Init = try init_state() catch Type:Error -> {error, {Type,Error}} end,
    case Init of
        {ok, Data} -> body(Data);
        {error, E} ->
            wf:error(?MODULE, "Init error: ~p",[E]),
            wf:redirect({http, "/"}) end.

body({Access, _Board, _Thread, _Action}=Data) ->
    
    {Content,Theme} = case Access of
        [] -> {html:board_body_private(Data),<<"glassy restrict">>};
        _ -> {html:board_body(Data),<<>>} end,
    #dtl{file="erlach",app=erlach,bindings=[
        {body,Content},
        {theme,Theme},
        {title,html:title(?MODULE,Data)}]}.

init_state() ->
    Route=?CTX#cx.path,
    
    case {qs:board_uri_to_id(Route#route.board),?SESSION:get_param(?MODULE)} of
        {Bid, _} when Bid =/= undefined -> % view thread
            thread:check_access_to_board(Bid,{board,view,{Route#route.type,Bid}});
        _ -> {error, bad_request}
    end.


event({thread, create, Type}=S) ->
    wf:info(?MODULE,"EVT: ~p",[S]),
    #board{uri=Uri}=erlang:get(board),
    ?SESSION:set_param(thread, {thread, create, Type}), wf:redirect(qs:ml({thread,create,html:gettype(),Uri}));
event({apply_board, BID}) ->
    wf:info(?MODULE, "Apply event ~p", [wf:q(checkbox_anonymous)]),
    u:restricted_call(fun() ->
        {ok, B} = kvs:get(board, BID),
        kvs:put(B#board{anonymous=case wf:q(checkbox_anonymous) of "on" -> true; _ -> undefined end})
    end, {feature, admin}),
    ok;
event(init) -> ok;
event(terminate) -> skip;
event(Event) -> guard:shared_event(?MODULE, Event).
