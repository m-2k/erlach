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

peer()    -> io_lib:format("~p",[wf:peer(?REQ)]).
main()    -> #dtl{file="erlach",app=erlach,bindings=[{body,body()}, {theme,<<"">>}, {title,<<"Threads">>}]}.

check_access_to_board(Board) ->
    case u:is_admin() of
        true -> allow;
        _ ->
            case Board#board.access of
                private -> u:check_access({board,Board#board.id});
                _ -> allow
            end
    end.

body() ->
    wf:info(?MODULE, "body ~p / blog: ~p", [wf:qs(<<"blog">>), wf:qs(<<"blog">>)]),
    case guard:to_integer(wf:qs(<<"board">>)) of
        undefined -> wf:redirect("/"), [];
        BoardID ->
            wf:info(?MODULE, "body board ~p", [BoardID]),
            case kvs:get(board, BoardID) of
                {ok, Board} ->
                    put(?BOARD_ID, BoardID),
                    case check_access_to_board(Board) of
                        allow ->
                            IsAdmin = u:is_admin(),
                            Content = case kvs:get(feed, {board, BoardID}) of
                                {ok, TF} ->
                                    wf:info(?MODULE, "body feed ~p", [TF]),
                                    Threads = kvs:traversal(thread, TF#feed.top, TF#feed.entries_count, #iterator.prev),
                                    % lists:map(fun(T) ->
                                    %     wf:info(?MODULE, "Head post: ~p", [T#thread.head_post]),
                                    %     {ok, #post{ user=User, message=Message } = P} = kvs:get(post, T#thread.head_post),
                                    %     % #panel { body = wf:f("~s: ~s~n~s", [User,T#thread.topic,Message]) }
                                    %
                                    %     html_thread(T, P)
                                    %     end, Threads);
                                    {HeadPostList3, HeadBlogList2,RequestHeadPostList2} = lists:foldl(fun(T, {PL, BL, RL}) ->
                                        % wf:info(?MODULE, "Head post: ~p", [T#thread.head_post]),
                                        {ok, #post{type=Type}=P} = kvs:get(post, T#thread.head_post),
                
                                        % TODO: Last post must overwrite #thread.last_post_date
                                        {ok, #feed{top=LastPostId}} = kvs:get(feed, {thread, T#thread.id}),
                                        {ok, #post{created=LastTimestamp}} = kvs:get(post, LastPostId),
                
                                        case T#thread.type of
                                            undefined -> {[{LastTimestamp,T,P}|PL], BL, RL};
                                            post -> {[{LastTimestamp,T,P}|PL], BL, RL};
                                            message -> {[{LastTimestamp,T,P}|PL], BL, RL};
                                            blog -> {PL, [{T,P}|BL], RL};
                                            request -> {PL, BL, [{T,P}|RL]}
                                        end
                                    end,{[],[],[]},Threads),
                                    % wf:info(?MODULE, "Head post list: ~p", [HeadPostList]),
            
                                    case wf:qs(<<"blog">>) of
                                        true -> []; % TODO: not implemented
                                        _ ->
                                            % Bump sorting TODO: inject thread-element to top in feed when post written
                                            SortedPostList = lists:sort(fun({TS1,_T1,_P1},{TS2,_T2,_P2}) -> TS1 >= TS2 end, HeadPostList3),
            
                                            #panel{ body = [
                                                % #span{ body = wf:f("Blog count: ~p, Thread count: ~p", [length(HeadBlogList2),length(HeadPostList3)])},
                                                html_blog(HeadBlogList2),
                                                lists:map(fun({_LastTimestamp,T,P}) -> html_thread(T,P) end, SortedPostList)
                                            ]}
                                    end;
                                _Empty -> []
                            end,
                            html:board_body(Board, Content);
                        _Disallow ->
                            case u:is_temp() of
                                true ->
                                    html:body(#panel{class= <<"center">>, body=[
                                        #span{class= <<"content-title">>, body= <<"The board only for the elect">>},
                                        #span{class= <<"remark">>, body= <<"To use this board authorization is needed and submit a request.">>},
                                        #link{class= <<"button primary">>, body= <<"Auth">>, postback=signin}]});
                                _ ->
                                    html:body(#panel{class= <<"center">>, body=[
                                        #span{class= <<"content-title">>, body= <<"The board only for the elect">>},
                                        #link{class= <<"button success">>, body= <<"Make request for access to this board">>,
                                            url=wf:f("/thread?id=~b",[Board#board.request_thread])}]})
                            end
                    end;
                NotFound ->
                    wf:info(?MODULE, "Board ~p not found: ~p", [BoardID,NotFound]),
                    wf:redirect("/"), []
            end
    end.
    
html_thread(#thread{id=Id,topic=Topic}=_Thread,#post{message=Message,created=_Timestamp,user=User}=Post) ->
    #panel {id=wf:f("thread-~.36b",[Id]),class= <<"board-thread">>,body=[
            #panel{class= <<"thread-topic">>,body=[
                #link{class= <<"link">>,
                    body=case guard:html_escape(Topic) of <<>> -> wf:f("#~w",[Id]); T -> T end,
                    href=wf:f("/thread?id=~p",[Id])}
            ]},
            #span{class= <<"username">>,body= <<"anonymous">>},
            #span{class= <<"message">>,body=guard:html_escape(Message)},
            #panel{class= <<"post-attachment">>,body=html:post_attachment(Post)}
        ]}.

html_blog([]) -> [];
html_blog(HeadBlogList2) ->
    {#thread{id=Id, topic=Topic}, #post{}} = lists:last(HeadBlogList2),
    #panel { class = <<"board-thread">>, body = [
            %         #link{
            % body = case guard:html_escape(Topic) of <<>> -> wf:f("#~w", [Id]); T -> T end,
            % href = wf:f("/thread?id=~p", [Id]) },
        #link{
            body = guard:html_escape(<<"Вся лента блога ("/utf8, (wf:to_binary(length(HeadBlogList2)))/binary,") >>>>"/utf8>>),
            href = wf:f("/board?board=~p&blog", [get(?BOARD_ID)]) }
    ]}.



% event({thread, create, {request, {board, Bid}}=Type}) ->
event({thread, create, Type}) ->
    % u:restricted_call(fun() ->
        case guard:to_integer(wf:qs(<<"board">>)) of
            undefined -> skip;
            Bid -> ?SESSION:set_param(thread, {thread, create, Type}), wf:redirect("/thread")
        end;
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

c(BoardID) ->
    ThreadID = kvs:next_id(thread, 1),
    {ok, P} = thread:write_post(wf:f("head post ~p", [ThreadID]), ThreadID, final),
    erase(),
    add_new_thread(BoardID, ThreadID, wf:f("topic ~p", [ThreadID]), P#post.id, false, false),
    lists:map(fun(E) ->
        thread:write_post(wf:f("some post ~p", [E]), ThreadID, final),
        erase()
        end, [22,33,44,55,66,77,88,99]).

add_new_thread(BoardID, ThreadID, Topic, PostID, InTop, Hidden) ->
    add_new_thread(BoardID, ThreadID, undefined,undefined, Topic, PostID, InTop, Hidden).
add_new_thread(BoardID, ThreadID, ThreadType, RequestTo, Topic, PostID, _InTop, Hidden) ->
    kvs:add( #thread {
        id=ThreadID,
        type=ThreadType,
        request_to=RequestTo,
        created=erlang:now(),
        topic=wf:to_binary(Topic),
        head_post=PostID,
        % order=,
        hidden=Hidden,
        feed_id={board, BoardID}
        }).