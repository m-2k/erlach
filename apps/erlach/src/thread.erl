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

main() ->
    Init = try init_state() catch Type:Error -> {error, {Type,Error}} end,
    case Init of
        {ok, Data} -> body(Data);
        {error, E} ->
            wf:error(?MODULE, "Init error: ~p",[E]),
            wf:redirect({http, "/"}) end.

body({Access, _Board, _Thread, _Action}=Data) ->
    {Content,Theme} = case Access of [] -> {html:thread_body_private(Data),<<>>}; _ -> {html:thread_body(Data),<<>>} end,
    #dtl{file="erlach",app=erlach,bindings=[
        {body,html:body(Content)},
        {theme,Theme},
        {title,html:title(?MODULE,Data)}]}.

init_state() ->
    Route=?CTX#cx.path,
    
    case {guard:to_integer(Route#route.thread,?IDS_BASE),?SESSION:get_param(?MODULE)} of
        {Tid, _} when Tid =/= undefined -> % view thread
            check_access_to_thread(Tid,{thread, view, Tid});
        {_, {thread, create, {request, {board, Bid}}}=Action} -> % new thread (request)
            EntranceBid = ?REQUESTS_BOARD,
            check_access_to_board(EntranceBid,Action);
        {_, {thread, create, {Type, Bid}}=Action} when Type =:= blog orelse Type =:= thread -> % new thread
            check_access_to_board(Bid,Action);
        _ -> {error, bad_request}
    end.

check_access_to_thread(Tid, Action) ->
    User = u:get(),
    {ok,#thread{feed_id={thread,Bid},name=Topic}=Thread}=kvs:get(thread,Tid),
    {ok,Board}=kvs:get(board,Bid),
    AccessMetaList = [access:meta(Board),access:meta(Thread)],
    Access=access:discavering(User,AccessMetaList),
    wf:wire(#transfer{state=[{board,Board},{thread,Thread},{action,Action},{access,Access}]}),
    {ok, {Access, Board, Thread, Action}}.

check_access_to_board(Bid, Action) ->
    User = u:get(),
    {ok,Board}=kvs:get(board,Bid),
    AccessMetaList = [access:meta(Board)],
    Access=access:discavering(User,AccessMetaList),
    wf:wire(#transfer{state=[{board,Board},{action,Action},{access,Access}]}),
    {ok, {Access, Board, undefined, Action}}.

event(init) ->
    erlang:put(?NEWEST_POSTS,[]),
    erlang:put(attachments,[]),
    case erlang:get(action) of
        {thread, view, Tid} ->
            wf:reg({thread, guard:to_integer(Tid)});
        _ -> ok end, ok;

    
event({client, auto_store}) ->
    wf:info(?MODULE,"Auto store",[]),
    message(store);
event({Store, finalize}) when Store =:= store orelse Store =:= edit ->
    wf:info(?MODULE,"Final storing",[]),
    case guard:is_empty(wf:q(message)) of
        true -> html:warning("Nothing to publish, bro.");
        false ->
            {ok, {T,P}} = message(finalize),
            erlang:put(attachments,[]),
            B=erlang:get(board),
            wf:info(?MODULE,"Final storing -sending",[]),
            wf:send({thread,T#thread.id},{server, {add, post, P, self()}}),
            case erlang:get(action) of
                {thread,create,{request, _}} -> wf:redirect(qs:ml({thread,thread,B#board.uri,T#thread.id}));
                {thread,create,{blog,_}} -> wf:redirect(qs:ml({board,blog,B#board.uri}));
                {thread,create,_} -> wf:redirect(qs:ml({board,B#board.uri}));
                _ ->
                    wf:info(?MODULE,"Final storing -wiring",[]),
                    wf:wire("publish_finished();"),
                    wf:info(?MODULE,"Final storing -wiring2",[]),
                    if Store =:= edit ->
                        {ok,HtmlPost}=html:post(P),
                        wf:info(?MODULE,"Final storing -htmlpost",[]),
                        wf:update(html:input_form_id(),HtmlPost),
                        ?JS_IMAGE_LIST_CHANGED_LESTENER,
                        wf:info(?MODULE,"Final storing -img update",[]),
                        S={erlang:get(access),erlang:get(board),erlang:get(thread),erlang:get(action)},
                        wf:insert_after(posts,html:input_form(S));
                    true -> add_post(P) end end end;

event({request, accepted, {Level,Lid}=ReqTo, User, PostId}) ->
    wf:info(?MODULE, "Granting access to ~p for ~p", [ReqTo,User]),
    % u:restricted_call(fun() ->
            % wf:info(?MODULE, "Define access: ~p", [u:define_access(User,ReqTo,allow)]),
            wf:info(?MODULE, "Define access: ~p", [access:define(User,private,Level,Lid,infinity,infinity)]),
            update_post(PostId),
        % end, {feature,admin}),
    ok;
    
event({server,{add, post, #post{id=Pid}=Post, Self}}) ->
    case self() of
        Self -> ok;
        _ ->
            Newest=erlang:get(?NEWEST_POSTS),
            case Newest of
                [] -> wf:insert_bottom(posts,#panel{id=more,class= <<"center">>,body=[
                    #link{class= <<"button success">>,body= <<"Load More ⟳"/utf8>>,postback=load_newest_posts}]});
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
    % u:restricted_call(fun() ->
        case kvs:get(post, Id) of
            {ok, Post} ->
                kvs:put(Post#post{deleted=true});
            Err -> wf:error(?MODULE, "Hiding post failed ~p", [Err])
        end,
        update_post(Id);
    % end, {feature, admin});
event({show_post, Id}) ->
    wf:info(?MODULE, "Showing post: ~p", [Id]),
    % u:restricted_call(fun() ->
        case kvs:get(post, Id) of
            {ok, Post} ->
                kvs:put(Post#post{deleted=undefined});
            Err -> wf:error(?MODULE, "Showing post failed ~p", [Err])
        end,
        update_post(Id);
    % end, {feature, admin});
event({edit_post,Id}) ->
    wf:info(?MODULE, "Edit post: ~p", [Id]),
    Post=message({restore,Id}),
    wf:info(?MODULE, "Edit post: ~p", [0]),
    wf:remove(html:input_form_id()),
    wf:info(?MODULE, "Edit post: ~p", [1]),
    S={erlang:get(access),erlang:get(board),erlang:get(thread),{post,edit,Id}},
    wf:info(?MODULE, "Edit post: ~p", ['1a']),
    wf:update(utils:hex_id({post,Id}),html:input_form(S)),
    wf:info(?MODULE, "Edit post: ~p", [2]),
    wf:wire("textarea_init(qi(\"message\"));drag_input_init();"),
    wf:info(?MODULE, "Edit post: ~p", [3]),
    wf:info(?MODULE, "Edit post: ~p", [4]);
event(enable_markdown) ->
    % u:restricted_call(fun() ->
        wf:update(markdown,#link{id=markdown,class= <<"link warning compact">>,body= <<"Disable Markdown">>,postback=disable_markdown}),
        P = message(create),
        erlang:put(post,P#post{markup=markdown}),
        % end, {feature, admin}),
        ok;
event(disable_markdown) ->
    % u:restricted_call(fun() ->
        wf:update(markdown,#link{id=markdown,class= <<"link info compact">>,body= <<"Enable Markdown">>,postback=enable_markdown}),
        P = message(create),
        erlang:put(post,P#post{markup=undefined}),
        % end, {feature, admin}),
        ok;

event({binary,{attachment,upload,Position,Name,_Type,_Size,Date,Data}}) ->
    wf:info(?MODULE, "Position: ~p",[Position]),
    P = message(create),
    wf:info(?MODULE, "New messg: ~p",[P#post.id]),
    if is_integer(Position) andalso is_binary(Data)
        andalso (size(Data) >= ?UPLOAD_MIN_SIZE) andalso (size(Data) =< ?UPLOAD_MAX_SIZE) ->
            case image:mime_type(Data) of
                undefined -> self() ! {server, {attachment,upload,Position,wrong_mime_type}};
                {Mime,Ext} ->
                    wf:info(?MODULE, "Mime OK: ~p",[Mime]),
                    Quality=photo,
                    Unique = utils:node_utc_random(),
                    Filaname = wf:to_list([Unique,".",Ext]),
                    Path=filename:join(?ATTACHMENT_LOCATION,Filaname),
                    Self=self(),
                    Id=kvs:next_id(attachment,1),
                    A = #attachment {
                        created = erlang:now(), id=Id, uuid=Unique,
                        feed_id={attachment,P#post.id},
                        position=Position, path=Path,
                        name=wf:to_list(Name),
                        date=wf:to_list(Date),
                        user=u:id(),
                        temporary=true },
                    erlang:put(attachments,[{Position,Id}|erlang:get(attachments)]),
                    wf:info(?MODULE, "Attachments in Proc State: ~p",[erlang:get(attachments)]),
                    SuccFun = fun({ok, Mime2, Size2}) -> Self ! {server, {attachment,upload,Position,{ok,A,Size2,Mime2}}};
                        (Error) -> Self ! {server, {attachment,upload,Position,Error}} end,
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
            case kvs:get(feed,{attachment,Pid}) of
                {ok, F} ->
                    Attachments = kvs:traversal(attachment, F#feed.top, F#feed.entries_count, #iterator.prev),
                    lists:map(fun(A) ->
                        kvs:delete(attachment,A#attachment.id), % raw ops without feed recalc
                        file:delete(filename:join(?SITE_ROOT_LOCATION, A#attachment.path))
                    end,Attachments),
                    kvs:delete(feed,{attachment,Pid});
                _ -> skip
            end,
            kvs:remove(post, Pid);
        _ -> skip
    end,
    case erlang:get(thread) of
        #thread{id=Tid,temporary=true} ->
            kvs:remove(thread, Tid),
            kvs:delete(feed, {post, Tid}); % raw ops (feed not contain container) already returning ok atom
        _ -> skip
    end;

event(Event) -> guard:shared_event(?MODULE, Event).

update_post(Id) ->
    HexId=utils:hex_id({post,Id}),
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


board({restore,Id}) ->
    erlang:erase(board),
    {ok, #board{}=B} = kvs:get(board, Id),
    erlang:put(board, B), B.

thread(create) ->
    #board{id=Bid} = erlang:get(board),
    case erlang:get(thread) of
        undefined ->
            T = #thread {
                id=kvs:next_id(thread,1),
                created=erlang:now(),
                name= <<>>,
                user=u:id(),
                feed_id={thread,Bid},
                temporary=true },
            {ok, T2} = kvs:add(T),
            erlang:put(thread, T2), T2;
        Exist -> Exist
    end;
thread(store) ->
    T = thread(create),
    T3 = case erlang:get(action) of
        {thread,create,_} ->
            T2 = T#thread{name=wf:to_binary(guard:prevent_undefined(wf:q(topic),<<>>))},
            kvs:put(T2),
            erlang:put(thread, T2), T2;
        _ -> T end,
    T3;

thread({restore,Id}) ->
    erlang:erase(thread),
    {ok, #thread{feed_id={thread,Bid}}=T} = kvs:get(thread, Id),
    board({restore,Bid}),
    erlang:put(thread, T), T;

thread(finalize) ->
    T = thread(store),
    Uname=html:name_selector_extract(wf:q(name_selector)), % only for create
    case erlang:get(action) of
        {thread,create,{Type,To}} ->
            T2=case {Type,To} of
                {request, {board=Level, Lid}} ->
                    {ok,E}=kvs:get(Level,Lid),
                    E2 = setelement(#db_element.request_thread,E,T#thread.id),
                    kvs:put(E2),
                    T#thread{request_to={Level,Lid}};
                _ -> T end,
            T3 = T2#thread{ type=Type, temporary=false, user_name=Uname, category=html:categories_extract(erlang:get(board)) },
            kvs:put(T3),
            erlang:erase(thread),
            {ok, T3};
        _ -> {ok, T} end.

message(create) ->
    T = thread(create),
    
    case erlang:get(post) of
        undefined ->
            P = #post{
                id=kvs:next_id(post,1),
                type=message,
                feed_id={post,T#thread.id},
                created=erlang:now(),
                user=u:id(),
                netw_info={wf:peer(?REQ), proplists:get_value(<<"user-agent">>,element(17,?REQ),undefined)},
                temporary=true },
            P2 = case erlang:get(action) of
                {thread, view, Tid} -> P#post{ head=false };
                {thread,create,{Type,Extra}} ->
                    T2 = T#thread{
                        % type=Type, % in thread(finalize)
                        % request_to=case Type of request -> Extra; _ -> undefined end,
                        head_post=P#post.id },
                    erlang:put(thread, T2),
                    P#post{ head=true }
            end,
            {ok, P3} = kvs:add(P2),
            erlang:put(post, P3), P3;
        Exist -> Exist
    end;

message(store) ->
    P = message(create),
    T = thread(store),
    P2 = P#post{message=wf:to_binary(guard:prevent_undefined(wf:q(message),<<>>))},
    kvs:put(P2),
    erlang:put(post, P2), P2;

message({restore,Id}) ->
    erlang:erase(post),
    {ok, #post{feed_id={post,Tid}}=P} = kvs:get(post, Id),
    thread({restore,Tid}),
    erlang:put(post, P), P;

message(finalize) ->
    P = message(store),
    {ok, T} = thread(finalize),
    P2 = P#post{ temporary=fase, user_name=html:name_selector_extract(wf:q(name_selector)) },
    kvs:put(P2),
    erlang:erase(post),
    {ok, {T, P2}}.
    