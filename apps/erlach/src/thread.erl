-module(thread). % THREAD LISTENING / ADDING THREAD HEAD POST
-vsn('0.2.0').

-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/feed.hrl").
-include_lib("db/include/post.hrl").
-include_lib("db/include/thread.hrl").
-include_lib("db/include/board.hrl").
-include_lib("db/include/user.hrl").
-include_lib("db/include/attachment.hrl").
-include("erlach.hrl").

-define(JS_IMAGE_LIST_CHANGED_LESTENER, wf:wire("dom_image_list_changed();")).

-ifndef(SESSION).
-define(SESSION, (wf:config(n2o,session,erlach_session))).
-endif.

main()    ->
    Init = try init_state() catch Type:Error -> {error, {Type,Error}} end,
    case Init of
        {ok, Data} -> body(Data);
        {error, E} ->
            wf:error(?MODULE, "Init error: ~p",[E]),
            wf:redirect({http, "/"})
    end.

body(Data) ->
    #dtl{file="erlach",app=erlach,bindings=[
        {body,html:body(html:thread_body(Data))},
        {theme,<<>>},
        {title,html:title(?MODULE,Data)}]}.

init_state() ->
     wf:info(?MODULE,"Test ~p",[init_state]),
    case {guard:to_integer(wf:qs(<<"id">>)),?SESSION:get_param(?MODULE)} of
        {Tid, _} when Tid =/= undefined -> % view thread
            check_access_to_thread(Tid,{thread, view, Tid});
        {_, {thread, create, {request, {board, Bid}}}=Action} -> % new thread (request)
            check_access_to_board(Bid,Action);
        {_, {thread, create, {post, Bid}}=Action} -> % new thread
            check_access_to_board(Bid,Action);
        _ -> {error, bad_request}
    end.

check_access_to_thread(Tid, Action) ->
    User = u:get(),
    {ok,#thread{feed_id={board,Bid},topic=Topic}=Thread}=kvs:get(thread,Tid),
    {ok,Board}=kvs:get(board,Bid),
    AccessMetaList = [access:meta(Board),access:meta(Thread)],
    case access:discavering(User,AccessMetaList) of
        {ok, Access} ->
            wf:info(?MODULE, "Access OK: ~p",[Access]),
            wf:wire(#transfer{state=[{board,Board},{thread,Thread},{action,Action}]}),
            {ok, {Access, Board, Thread, Action}};
        E -> E end.

check_access_to_board(Bid, Action) ->
    User = u:get(),
    {ok,Board}=kvs:get(board,Bid),
    AccessMetaList = [access:meta(Board)],
    case access:discavering(User,AccessMetaList) of
        {ok, Access} ->
            wf:info(?MODULE, "Access OK: ~p",[Access]),
            wf:wire(#transfer{state=[{board,Board},{action,Action}]}),
            {ok, {Access, Board, undefined, Action}};
        E -> E end.

event(init) ->
    % wf:info(?MODULE, "----> BOARD: ~p, THREAD: ~p", [get(board), get(thread)]),
    erlang:put(?NEWEST_POSTS,[]),
    erlang:put(attachments,[]),
    % wf:info(?MODULE, "Bridge info: ~p", [proplists:get_value(<<"user-agent">>, element(17,?REQ),undefined)]),
    case erlang:get(action) of
        {thread, view, Tid} ->
            put(?THREAD_ID, guard:to_integer(Tid)),
            wf:reg({thread, guard:to_integer(Tid)});
        {thread, create, {post, Bid}} -> put(?BOARD_ID, wf:to_integer(Bid));
        {thread, create, {request, {board=Element, ElementId}}} -> 
            u:restricted_call(fun() ->
                put(?STORED_REQUEST_TO, {Element,ElementId})
                end,{feature,admin})
    end,
    % wf:info(?MODULE, " BOARD: ~p, THREAD: ~p, USER_ID: ~p", [get(?BOARD_ID), get(?THREAD_ID), u:id(u:get())]),
    ok;

    
event({client, auto_store}) ->
    wf:info(?MODULE,"Auto store",[]),
    message(store);
event({store, finalize}) ->
    wf:info(?MODULE,"Final storing",[]),
    case guard:is_empty(wf:q(message)) of
        true -> html:warning("Nothing to publish, bro.");
        false ->
            {ok, {T,P}} = message(finalize),
            erlang:put(attachments,[]),
            B = erlang:get(board),
            wf:send({thread,T#thread.id},{server, {add, post, P, self()}}),
            case erlang:get(action) of
                {thread,create,_} -> wf:redirect(wf:f("/board?board=~p", [B#board.id]));
                _ ->
                    wf:wire("publish_finished();"),
                    add_post(P) end end;

event({request, accepted, ReqTo, User, PostId}) ->
    u:restricted_call(fun() ->
            wf:info(?MODULE, "Define access: ~p", [u:define_access(User,ReqTo,allow)]),
            update_post(PostId)
        end, {feature,admin}),
    ok;
    
event({server,{add, post, #post{id=Pid}=Post, Self}}) ->
    case self() of
        Self -> ok;
        _ ->
            Newest=erlang:get(?NEWEST_POSTS),
            case Newest of
                [] -> wf:insert_bottom(posts,#panel{id=more,class= <<"center">>,body=[
                    #link{class= <<"button success">>,body= <<"Load More">>,postback=load_newest_posts}]});
                _ -> ok end,
            erlang:put(?NEWEST_POSTS,[Pid|Newest]),
            wf:info(?MODULE, "NEWEST_POSTS: ~p",[erlang:get(?NEWEST_POSTS)])
            % add_post(Id)
    end;

event(load_newest_posts) ->
    wf:remove(more),
    [ add_post(Id) || Id <- lists:reverse(erlang:get(?NEWEST_POSTS))],
    erlang:put(?NEWEST_POSTS,[]);



event({hide_post, Id}) ->
    wf:info(?MODULE, "Hiding post: ~p", [Id]),
    u:restricted_call(fun() ->
        case kvs:get(post, Id) of
            {ok, Post} ->
                kvs:put(Post#post{deleted=true});
            Err -> wf:error(?MODULE, "Hiding post failed ~p", [Err])
        end,
        update_post(Id)
    end, {feature, admin});
event({show_post, Id}) ->
    wf:info(?MODULE, "Showing post: ~p", [Id]),
    u:restricted_call(fun() ->
        case kvs:get(post, Id) of
            {ok, Post} ->
                kvs:put(Post#post{deleted=undefined});
            Err -> wf:error(?MODULE, "Showing post failed ~p", [Err])
        end,
        update_post(Id)
    end, {feature, admin});
event(enable_markdown) ->
    u:restricted_call(fun() ->
        wf:update(markdown,#button { id = markdown, class= <<"warning">>, body = <<"Disable Markdown">>, postback = disable_markdown }),
        put(?MARKDOWN_ENABLED, ok)
        end, {feature, admin}), ok;
event(disable_markdown) ->
    u:restricted_call(fun() ->
        wf:update(markdown,#button { id = markdown, class= <<"info">>, body = <<"Enable Markdown">>, postback = enable_markdown }),
        put(?MARKDOWN_ENABLED, undefined)
        end, {feature, admin}), ok;

event({binary,{attachment,upload,Position,Name,_Type,_Size,Date,Data}}) ->
    wf:info(?MODULE, "Position: ~p",[Position]),
    P = message(create),
    if is_integer(Position)
        andalso is_binary(Data)
        andalso (size(Data) >= ?UPLOAD_MIN_SIZE)
        andalso (size(Data) =< ?UPLOAD_MAX_SIZE) ->
            case image:mime_type(Data) of
                undefined -> self() ! {server, {attachment,upload,Position,wrong_mime_type}};
                {Mime,Ext} ->
                    Quality=photo,
                    Unique = utils:node_utc_random(),
                    Filaname = wf:to_list([Unique,".",Ext]),
                    Path=filename:join(?ATTACHMENT_LOCATION,Filaname),
                    Self=self(),
                    Id=kvs:next_id(attachment,1),
                    
                    A = #attachment {
                        created = erlang:now(),
                        id=Id,
                        uuid=Unique,
                        feed_id={post,P#post.id},
                        position=Position,
                        path=Path,
                        name=wf:to_list(Name),
                        date=wf:to_list(Date),
                        user=u:id(),
                        temporary=true
                        },
                    
                    erlang:put(attachments,[{Position,Id}|erlang:get(attachments)]),
                    
                    SuccFun = fun({ok, Mime2, Size2}) ->
                            % wf:info(?MODULE, "SuccFun ~p ~p ~p, ~p",[Mime2,Size2,Self,self()]),
                            Self ! {server, {attachment,upload,Position,{ok,A,Size2,Mime2}}};
                        (Error) ->
                            Self ! {server, {attachment,upload,Position,Error}}
                        end,
                
                    image:convert_async({Mime, Ext}, Data, filename:join(?SITE_ROOT_LOCATION,Path), Quality, SuccFun)
            end;
        true -> self() ! {server, {attachment,upload,Position,wrong_file}}
    end;
event({server,{attachment,upload,Position,{ok,#attachment{}=A, Size, Mime}}}) ->
    case lists:keymember(Position,1,erlang:get(attachments)) of
        true -> case kvs:add(A#attachment{temporary=false,size=Size,mime=Mime}) of
            {ok, _} -> wf:wire(wf:f("fileLoadFinished(~b);", [Position]));
            E -> self() ! {server, {attachment,upload,Position,kvs_store_error}} end;
        false -> file:delete(filename:join(?SITE_ROOT_LOCATION,A#attachment.path))
    end;
event({server,{attachment,upload,Position,Error}}) ->
    wf:warning("Upload failed: ~p", [Error]),
    wf:wire(wf:f("fileLoadFailed(~b);", [Position]));
event({binary,{attachment,remove,Position}}) ->
    Ats = erlang:get(attachments),
    case lists:keytake(Position,1,Ats) of
        false -> ok;
        {value,{Position,Id},Ats2} ->
            erlang:put(attachments,Ats2),
            kvs:remove(attachment,Id) % delete if already stored
    end, ok;
    
event(terminate) ->
    % ?SESSION:erase_param(?MODULE),
    case erlang:get(post) of
        #post{id=Pid,temporary=true}=P ->
            case kvs:get(feed,{post,Pid}) of
                {ok, F} ->
                    Attachments = kvs:traversal(attachment, F#feed.top, F#feed.entries_count, #iterator.prev),
                    lists:map(fun(A) ->
                        % kvs:remove(A),
                        kvs:delete(attachment,A#attachment.id), % raw ops without feed recalc
                        file:delete(filename:join(?SITE_ROOT_LOCATION, A#attachment.path))
                    end,Attachments),
                    % kvs:remove(F); % +kvs:get operation to performans
                    kvs:delete(feed,{post,Pid}); % raw ops without feed recalc
                _ -> skip
            end,
            kvs:remove(post, Pid);
        _ -> skip
    end,
    case erlang:get(thread) of
        #thread{id=Tid,temporary=true} ->
            kvs:remove(thread, Tid),
            kvs:delete(feed, {thread, Tid}); % raw ops (feed not contain container) already returning ok atom
        _ -> skip
    end;

event(Event) -> guard:shared_event(?MODULE, Event).

update_post(Id) ->
    HexId=wf:f("post-~.36b", [Id]),
    case kvs:get(post, Id) of
        {ok, Post} ->
            case html:post(Post) of
                {ok, Html} -> wf:update(HexId, Html);
                _ -> wf:remove(HexId)
            end;
        Err ->
            wf:remove(HexId),
            wf:warning(?MODULE, "Updating post failed ~p", [Err])
    end, ?JS_IMAGE_LIST_CHANGED_LESTENER, ok.
add_post(#post{}=Post) ->
    case html:post(Post) of
        {ok, Html} -> wf:insert_bottom(posts, Html);
        _ -> skip end, ?JS_IMAGE_LIST_CHANGED_LESTENER, ok;
add_post(Id) ->
    case kvs:get(post, Id) of
        {ok, Post} -> add_post(Post);
        Err -> wf:warning(?MODULE, "Adding post failed ~p", [Err]) end.

thread(create) ->
    #board{id=Bid} = erlang:get(board),
    case erlang:get(thread) of
        undefined ->
            T = #thread {
                id=kvs:next_id(thread,1),
                created=erlang:now(),
                topic= <<>>,
                feed_id={board,Bid},
                temporary=true },
            {ok, T2} = kvs:add(T),
            erlang:put(thread, T2), T2;
        Exist -> Exist
    end;
thread(store) ->
    T = thread(create),
    T3 = case erlang:get(action) of
        {thread,create,_} ->
            % wf:info(?MODULE, "THREAD ~p",[T]),
            T2 = T#thread{topic=wf:to_binary(guard:prevent_undefined(wf:q(topic),<<>>))},
            kvs:put(T2),
            erlang:put(thread, T2), T2;
        _ -> T end,
    T3;
thread(finalize) ->
    T = thread(store),
    case erlang:get(action) of
        {thread,create,_} ->
            T2 = T#thread{ temporary=false },
            kvs:put(T2),
            erlang:erase(thread),
            {ok, T2};
        _ -> {ok, T} end.

message(create) ->
    T = thread(create),
    
    case erlang:get(post) of
        undefined ->
            P = #post{
                id=kvs:next_id(post,1),
                type=message,
                feed_id={thread,T#thread.id},
                created=erlang:now(),
                user=u:id(),
                netw_info={wf:peer(?REQ), proplists:get_value(<<"user-agent">>,element(17,?REQ),undefined)},
                temporary=true },
            P2 = case erlang:get(action) of
                {thread, view, Tid} -> P#post{ head=false };
                {thread,create,{Type,Extra}} ->
                    T2 = T#thread{
                        type=Type,
                        request_to=case Type of request -> Extra; _ -> undefined end,
                        head_post=P#post.id },
                    erlang:put(thread, T2),
                    P#post{ head=true }
            end,
            {ok, P3} = kvs:add(P2),
            erlang:put(post, P3), P3;
        Exist -> Exist
    end;
    % message(store);

message(store) ->
    P = message(create),
    T = thread(store),
    P2 = P#post{message=wf:to_binary(guard:prevent_undefined(wf:q(message),<<>>))},
    kvs:put(P2),
    erlang:put(post, P2), P2;
            
message(finalize) ->
    P = message(store),
    {ok, T} = thread(finalize),
    P2 = P#post{ temporary=fase },
    kvs:put(P2),
    erlang:erase(post),
    {ok, {T, P2}}.
    