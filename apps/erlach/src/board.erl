-module(board). %% BOARD LISTENING
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

% peer()    -> io_lib:format("~p",[wf:peer(?REQ)]).
% main()    -> #dtl{file="erlach",app=erlach,bindings=[{body,body()}, {theme,<<"">>}, {title,<<"Threads">>}]}.
%
% check_access_to_board(Board) ->
%     case u:is_admin() of
%         true -> allow;
%         _ ->
%             case Board#board.access of
%                 private -> u:check_access({board,Board#board.id});
%                 _ -> allow
%             end
%     end.

main() ->
    Init = try init_state() catch Type:Error -> {error, {Type,Error}} end,
    case Init of
        {ok, Data} -> body(Data);
        {error, E} ->
            wf:error(?MODULE, "Init error: ~p",[E]),
            wf:redirect({http, "/"}) end.

body({Access, _Board, _Thread, _Action}=Data) ->
    
    % {Value, Req} = cowboy_req:binding(module, ?REQ),
    % wf:info(?MODULE,"Binding body: ~p",[Value]),
    
    {Content,Theme} = case Access of [] -> {html:board_body_private(Data),<<"glassy restrict">>}; _ -> {html:board_body(Data),<<>>} end,
    #dtl{file="erlach",app=erlach,bindings=[
        {body,Content},
        {theme,Theme},
        {title,html:title(?MODULE,Data)}]}.

init_state() ->
    % Id = guard:to_integer(wf:qs(<<"id">>)),
    % Id = guard:to_integer(erlang:get(sasay)),
    % wf:info(?MODULE,"Binding INIT: ~p",[erlang:get(matched_qs)]),
    % #{board := Id} = erlang:get(matched_qs),
    Route=?CTX#cx.path,
    
    case {qs:board_uri_to_id(Route#route.board),?SESSION:get_param(?MODULE)} of
        {Bid, _} when Bid =/= undefined -> % view thread
            % Type=case Route#route.type of <<"blog">> -> blog; _ -> thread end,
            thread:check_access_to_board(Bid,{board,view,{Route#route.type,Bid}});
        % {_, {thread, create, {request, {board, Bid}}}=Action} -> % new thread (request)
        %     check_access_to_board(Bid,Action);
        % {_, {thread, create, {post, Bid}}=Action} -> % new thread
        %     check_access_to_board(Bid,Action);
        _ -> {error, bad_request}
    end.


% event({thread, create, {request, {board, Bid}}=Type}) ->
event({thread, create, Type}=S) ->
    wf:info(?MODULE,"EVT: ~p",[S]),
    % u:restricted_call(fun() ->
        % case guard:to_integer(wf:qs(<<"id">>)) of
            % undefined -> skip;
            % Bid ->
            #board{uri=Uri}=erlang:get(board),
            ?SESSION:set_param(thread, {thread, create, Type}), wf:redirect(qs:ml({thread,create,Uri}));
        % end;
        % end end, {feature, admin});
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

% c(BoardID) ->
%     ThreadID = kvs:next_id(thread, 1),
%     {ok, P} = thread:write_post(wf:f("head post ~p", [ThreadID]), ThreadID, final),
%     erase(),
%     add_new_thread(BoardID, ThreadID, wf:f("topic ~p", [ThreadID]), P#post.id, false, false),
%     lists:map(fun(E) ->
%         thread:write_post(wf:f("some post ~p", [E]), ThreadID, final),
%         erase()
%         end, [22,33,44,55,66,77,88,99]).
%
% add_new_thread(BoardID, ThreadID, Topic, PostID, InTop, Hidden) ->
%     add_new_thread(BoardID, ThreadID, undefined,undefined, Topic, PostID, InTop, Hidden).
% add_new_thread(BoardID, ThreadID, ThreadType, RequestTo, Topic, PostID, _InTop, Hidden) ->
%     kvs:add( #thread {
%         id=ThreadID,
%         type=ThreadType,
%         request_to=RequestTo,
%         created=erlang:now(),
%         name=wf:to_binary(Topic),
%         head_post=PostID,
%         % order=,
%         hidden=Hidden,
%         feed_id={thread, BoardID}
%         }).