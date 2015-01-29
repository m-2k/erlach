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

peer()    -> io_lib:format("~p",[wf:peer(?REQ)]).
main()    -> #dtl{file="erlach",app=review,bindings=[{body,body()}, {theme,<<"">>}, {title,<<"Thread">>}]}.

% check_access_to_thread(#board{}=Board,#thread{id=Tid,access=Access,type=Type,request_to=ReqTo}) ->
% 	case u:is_admin() of
% 		true -> allow;
% 		_ ->
% 			case {Access,Type} of
% 				{private,_} ->
% 					case {board:check_access_to_board(Board),u:check_access({thread,Tid})} of
% 						{allow,allow} -> allow;
% 						_ -> none
% 					end;
% 				{_,request} when ReqTo =/= undefined -> case u:is_temp() of true -> none; _ -> allow end;
% 				_ -> board:check_access_to_board(Board)
% 			end
% 	end.

check_access_to_post(_Thread, _Post) -> allow.

body() ->
	% u:ensure_user(),
	%     wf:info(?MODULE, "body ~p", [0]),
	% wf:info(?MODULE, "user ~p ~p ~p", [u:get(), u:id(), u:is_admin()]),
    case wf:qs(<<"thread">>) of
        undefined -> % new thread creating request
            case wf:qs(<<"board">>) of
                undefined ->
                    wf:redirect("/"), [];
                B ->
					IsAdmin = u:is_admin(),
					case {IsAdmin,?SESSION:get_param(?MODULE)} of
						{true, {new_thread_request,E,Eid}} -> % request creation
							html:body(html:thread_body({E,Eid}));
							
						{_,_} ->	% normal creation
		                    wf:info(?MODULE, "body NEW-T, Bid=~p", [B]),
		                    Bid = wf:to_integer(B),
		                    case kvs:get(board, Bid) of
		                        {ok, Board} ->
									case board:check_access_to_board(Board) of
										allow -> html:body(html:thread_body());
										_ -> wf:redirect("/"), []
									end;
		                        _NotExist -> wf:redirect("/"), []
		                    end
						% ErrorGet ->
						% 	wf:warning(?MODULE, "Unknown request for creating thread: ~p (request,request_id,IsAdmin)",[ErrorGet]),
						% 	wf:redirect("/"), []
					end
            end;
        T -> % existing thread request
            Tid = wf:to_integer(T),
            case kvs:get(thread, Tid) of
				{ok, #thread{feed_id = {board, Bid}, request_to=ReqTo}=Thread} ->
					case kvs:get(board, Bid) of
						{ok, Board} ->
				            % wf:info(?MODULE, "body Bid=~p", [Bid]),
							case check_access_to_thread(Board,Thread) of
								allow -> html:body(html:thread_body(Tid,Bid));
								_ -> wf:redirect("/"), [] end;
						_ -> wf:redirect("/"), []
					end;
                _NotExist -> wf:redirect("/"), []
			end
    end,
	
	
	Init = try init_state() catch Type:Error -> {error, {Type,Error}} end,
	case Init of
		{ok, Data} -> html:body(html:thread_body(Data));
		{error, _E} -> wf:redirect("/"), []
	end.

init_state() ->
    case {guard:to_integer(wf:qs(<<"id">>)),?SESSION:get_param(?MODULE)} of
		{Id, _} when Id =/= undefined -> % view thread
			{ok,#thread{feed_id={board,Bid}}=Thread}=kvs:get(thread,Id),
			{ok,Board}=kvs:get(board,Bid),
			{ok, {view, {default, Thread}, Board}};
		{undefined, {thread, create, {request, {board, Bid}}=Type}} -> % new thread (request)
			{ok,Board}=kvs:get(board,Bid),
			{ok, {create, Type, Board}};
		{undefined, {thread, create, {default, Bid}=Type}} -> % new thread
			{ok,Board}=kvs:get(board,Bid),
			{ok, {create, Type, Board}};
		_ -> {error, bad_request}
	end.

check_access(Init) ->
	case Init of
		{ok, view, Board, Thread} ->
			case check_access_to_thread(Board,Thread) of allow -> Init; _ -> 400 end;
		{ok, create, Type, Board}
		

	% TODO: придумать администрирование пользователями
	% INPUT: User, Item, Action,
	% ACLs: {feature,admin}
		{moderate, {board, 3, Type}}, Type :: #element.type
		{read, {board,3, Type}} -> {allow,infinity,infinity}
		{write, {board,3, Type}}
		{write, {thread, 10, Type}}
		{read, {global, board, Type}} -> {allow, Start, Expire} % Start for examply: ban on 1 day
		{moderate, {global, thread, Type}} -> {allow, Start, Expire}
		% {feature, moderator} % as global
		% {feature, banned}
	
	% по умолчанию все запрещено кроме начальных установок
	% накладываем на это то что разрешено, начиная с самого верхнего уровня
	% ???
	% PROFIT!

% can restrict the rights for temporary users or others
get_initial_access(Uid, {Action, {_Purpose, _Section, Object, Type}}=Feature) ->
	% Read -> {allow, infinity},	% as {read, global}
	% Write -> {allow, infinity},	% as {write, global}
	% {undefined,Read,Write,none}.
	Allow={allow,infinity,infinity},
	Deny = none,
	Access=u:check_access(Uid, Feature),
	case Access of
		Deny ->
			case {Action,Object,Type} of
				{read,thread,_} -> Allow;
				{read,board,_} -> Allow;
				{write,thread,default} -> Allow;
				{write,thread,request} -> Allow;
				{write,board,default} -> Allow;
				_ -> Deny
			end;
		_ -> Access
	end.

% check_access_to_thread({Mode, Type, Board}) ->
	{thread, read,   {default, Thread}, Board}
	{thread, write,  {default, Bid}, Board}
	{thread, write,  {request, {board, Bid}}, Board}
	{thread, write,  {request, {thread, Tid}}, Board}
	{thread, write,  {request, {group, Gid}}, Board}
	{thread, write,  {blog, Bid}, Board}
	{thread, read,   {blog, Thread}, Board}
	{board,  write,  {default, Gid}, Group}
	{post,   write,  {default, Tid}, Thread}
	{post,   write,  {question, Tid}, Thread}
	{board,  read,   {default, Board}, Group}
	{post,   read,   {default, Post}, Thread}
	{post,   delete, {default, Pid}, Thread}
	
	
	
check_access({Object, OAction, {OType, _Data}, Parent) ->
	% User = u:get(),
	% IsAdmin = u:is_admin(User),
	% IsTemp = u:is_temp(User),
	% Uid = u:id(User),
	
	Purpose = element(1,Parent),
	Action = case OAction of delete -> moderate; _ -> OAction end,
	
	Section = element(2,Parent),
	Type = OType,
	
	Feature = {Action,{Purpose,Section,Object,Type}},
	% {Value,Start,Expire}=
	get_initial_access(1,Feature).
	
		{ kvs_acl:check_access(Uid, {feature, admin}) == allow,
		kvs_acl:check_access(Uid, {feature, reviewer}) == allow,
		kvs_acl:check_access(Uid, {feature, developer}) == allow} end,
		
	case IsAdmin of
		true -> allow;
		_ ->
			case {Mode, Type, Board} of
				{view, {default, #thread{id=Tid}}, #board{access=private}} ->
					case {board:check_access_to_board(Board),u:check_access({thread,Tid})} of
						{allow,allow} -> allow;
						_ -> none
					end;
				{create, }
			case {Access,Type} of
				{private,_} ->
					case {board:check_access_to_board(Board),u:check_access({thread,Tid})} of
						{allow,allow} -> allow;
						_ -> none
					end;
				{_,request} when ReqTo =/= undefined -> case u:is_temp() of true -> none; _ -> allow end;
				_ -> board:check_access_to_board(Board)
			end
	end.
	

posts_list(#thread{id=Tid,type=Type,request_to=ReqTo}=Thread) ->
    wf:info(?MODULE, "body ~p", [2]),
	Uid = u:id(),
    case kvs:get(feed, {thread, Tid}) of
        {ok, F} ->
            wf:info(?MODULE, "body ~p", [3]),
			IsAdmin = u:is_admin(),
			Uid = u:id(),
            AllPosts = kvs:traversal(post, F#feed.top, F#feed.entries_count, #iterator.prev),
			% {Head, Posts} = lists:foldl(fun(P, {H,Acc}) ->
			% 	case {H,P#post.head,P#post.temporary,Type} of
			% 		{_,undefined,false,request} when ReqTo =/= undefined andalso IsAdmin =/= true ->
			% 			case P#post.user of
			% 				Uid -> {H,[html_post(Thread,P,IsAdmin)|Acc]};
			% 				_ -> {H,Acc}
			% 			end;
			% 		{_,undefined,false,_} -> {H,[html_post(Thread,P, IsAdmin)|Acc]};
			% 		{undefined,_,false,_} -> {html_post(Thread,P,IsAdmin),Acc};
			% 		_ -> {H,Acc} end end, {undefined,[]}, AllPosts),
			% {ok, Head, Posts};
			{Head, Posts} = lists:foldl(fun(P, {H,Acc}) ->
				case {H,P#post.head,html_post(Thread,P,IsAdmin,Uid)} of
					{undefined,true,{ok, Html}} -> {Html,Acc};	% first head post
					{_,undefined,{ok, Html}} -> {H,[Html|Acc]};	% message posts
					% {_,{skip, access}} -> {H,Acc};
					_ -> {H,Acc}
				end end, {undefined,[]}, AllPosts),
			{ok, Head, Posts};
        Err ->
            wf:info(?MODULE, "body ~p", [4]),
            wf:warning(?MODULE, "Thread feed ~p not found: ~p", [{thread, Tid}, Err]),
            {error, feed_not_found}
    end.

html_post_check_visibility(Uid,IsAdmin,#thread{type=ThType,request_to=ReqTo},#post{deleted=Deleted,head=IsHead,temporary=IsTemp,user=User}) ->
	case {IsAdmin,IsHead,IsTemp,Deleted,ThType,User} of
		{_,_,true,_,_,_} -> skip;
		{true,_,_,_,_,_} -> ok;
		{_,_,_,true,_,_} -> skip;
		{_,true,_,_,_,_} -> ok;
		{_,_,_,_,request,Uid} when ReqTo =/= undefined -> ok;
		{_,_,_,_,request,_} -> skip;
		_ -> ok
	end.

html_post(#post{feed_id={thread,Tid}}=P) ->
	{ok, Thread} = kvs:get(thread,Tid),
	html_post(Thread,P, u:is_admin(),u:id()).
html_post(#thread{id=Tid,type=ThreadType,request_to=ReqTo}=Thread,
	#post{id=Id,type=PostType,feed_id ={thread,Tid},temporary=IsTemp,message=Message,
	created=Timestamp,user=User,deleted=Deleted,head=IsHead}=Post,
	IsAdmin,Uid) ->

	wf:info(?MODULE, " >>> html_post Message: ~p", [Message]),
    % {{_Y,_M,_D},{Hour,Minute,Second}} = calendar:now_to_local_time(Timestamp),
	Text = case PostType of
		blog -> markdown:conv_utf8(Message);
		message -> guard:html_escape(Message);
		_ -> guard:html_escape(Message)
	end,
	wf:info(?MODULE, " >>> html_post Converted message: ~p", [Text]),
	
	case {html_post_check_visibility(Uid,IsAdmin,Thread,Post),Deleted} of
		{ok,undefined} ->
		    Html = #panel { id = wf:f("post-~.36b", [Id]), class = <<"thread-post">>, body = [
		        % #panel{ class = <<"timestamp">>, body = guard:html_escape(wf:f("~2w:~2..0w:~2..0w", [Hour, Minute,Second])) },
		        % #panel{ class = <<"right-side">>, body = [
					case {IsAdmin, IsHead} of
						{true, true} -> #button{ class = <<"warning">>, body = <<"Hide thread">>, postback = {hide_thread, Tid} };
						{true, _} -> #button{ class = <<"warning">>, body = <<"Hide">>, postback = {hide_post, Id} };
						_ -> [] end,
					case ThreadType of
						request when ReqTo =/= undefined andalso IsAdmin =:= true ->
							case u:id() of
								User -> [];
								_ -> 
									case u:check_access(User,ReqTo) of
										allow -> [];
										_ -> #button{ class = <<"success">>,
											body = <<"Join user ",(wf:to_binary(wf:to_list(Uid)))/binary," to ", (wf:to_binary(wf:to_list(ReqTo)))/binary>>,
											postback = {request, accepted, ReqTo, User, Id} } end end;
						_ -> []
					end,
		            #span{ class = <<"username">>, body = <<"anonymous">> },
		            #span{ class = <<"message">>, body = Text },
		            #panel{ class = <<"post-attachment">>, body = html_post_attachment(Post) }
		        %    ]}
		        ]},
			{ok, Html};
		{ok,_} ->
			case IsAdmin of
				true ->
				    Html = #panel { id = wf:f("post-~.36b", [Id]), class = <<"thread-post">>, body = [
							case IsHead of
								true -> #button{ class = <<"success">>, body = <<"Show thread">>, postback = {show_thread, Tid} };
								_ -> #button{ class = <<"success">>, body = <<"Show">>, postback = {show_post, Id} }
							end,
				            #span{ class = <<"username">>, body = <<"anonymous">> },
				            #span{ class = <<"message">>, body = Text },
				            #panel{ class = <<"post-attachment">>, body = html_post_attachment(Post) }
				        %    ]}
				        ]},
					{ok, Html};
				_ -> skip end;
		{Skip,_} -> Skip
	end.
        
html_post_attachment(#post{ id=Id } = _P) ->
    case kvs:get(feed, {post, Id}) of
        {ok, F} ->
            Attachments = kvs:traversal(attachment, F#feed.top, F#feed.entries_count, #iterator.prev),
            lists:map(fun(A) ->
                #image{ image = A#attachment.path, class = <<"image">> }
                end, Attachments);
        NotFound ->
            wf:info(?MODULE, "Post ~p doesn't has attachments (feed: ~p reason: ~p)", [Id, {post, Id}, NotFound]),
            []
    end.

event(init) ->
	wf:info(?MODULE, "Pid: ~p", [self()]),
	erlang:put(?NEWEST_POSTS,[]),
	% wf:info(?MODULE, "Bridge info: ~p", [proplists:get_value(<<"user-agent">>, element(17,?REQ),undefined)]),
    %% also if get(?board_id) is initialized -> new_thread; else -> new post 
	u:ensure_user(),
    case wf:qs(<<"thread">>) of
        undefined -> % new thread creating request
            case wf:qs(<<"board">>) of
                undefined -> wf:error(?MODULE, "Logical error: body/0 preventing this clause", []);
                B -> put(?BOARD_ID, wf:to_integer(B))
            end;
        T -> % existing thread request
            put(?THREAD_ID, wf:to_integer(T)),
			wf:reg({thread, wf:to_integer(T)})
    end,
    wf:info(?MODULE, " BOARD: ~p, THREAD: ~p, USER_ID: ~p", [get(?BOARD_ID), get(?THREAD_ID), u:id(u:get())]),
	
	u:restricted_call(fun() ->
		case ?SESSION:get_param(?MODULE) of
			{new_thread_request,E,Eid} ->
				put(?STORED_REQUEST_TO, {E,Eid}),
				wf:info(?MODULE,"Setup STORED_REQUEST_TO: ~p",[{new_thread_request,E,Eid}]);
			_ -> ok
		end end,{feature,admin});
event(send) -> event({send, post_message});


event({request, accepted, ReqTo, User, PostId}) ->
	u:restricted_call(fun() ->
			wf:info(?MODULE, "Define access: ~p", [u:define_access(User,ReqTo,allow)]),
			update_post(PostId)
		end, {feature,admin}),
	ok;
	
event({server,{add, post, Id, Self}}) ->
	case self() of
		Self -> ok;
		_ ->
			Newest=erlang:get(?NEWEST_POSTS),
			case Newest of
				[] -> wf:insert_bottom(posts,#panel{id=more,class= <<"center">>,body=[
					#link{class= <<"button success">>,body= <<"Load More">>,postback=load_newest_posts}]});
				_ -> ok end,
			erlang:put(?NEWEST_POSTS,[Id|Newest]),
			wf:info(?MODULE, "NEWEST_POSTS: ~p",[erlang:get(?NEWEST_POSTS)])
			% add_post(Id)
	end;

event(load_newest_posts) ->
	wf:remove(more),
	[ add_post(Id) || Id <- lists:reverse(erlang:get(?NEWEST_POSTS))],
	erlang:put(?NEWEST_POSTS,[]);

% event({new, thread, request}) -> ok;
% event({new, thread, blog}) -> ok;
% event({new, thread, undefined}) -> ok;
event({new, message}) -> event({new, thread, undefined});
event({new, thread}) -> event({new, thread, undefined});
event({new, thread, Type}) ->
	wf:info(?MODULE, "New: ~p ~p", [wf:q(textarea_topic), wf:q(textarea)]),
    case {get(?BOARD_ID), wf:q(textarea_topic), wf:q(textarea)} of
        {undefined, _, undefined} -> ok;
        {undefined, _, []} -> ok;
        {undefined, _, Message} -> % new post creation
            case write_final_post(undefined, Message, Type) of
                {ok, #post{id=Pid,feed_id={thread,Tid}}=Post} ->
                    % erase(?STORED_POST_ID),
                    wf:wire("sending_finished();"),
                    wf:info(?MODULE, "Storing post: ~p", [get(?STORED_POST_ID)]),
                    add_post(Post),
					wf:send({thread,Tid},{server, {add, post, Pid, self()}}),
					?JS_IMAGE_LIST_CHANGED_LESTENER,
					wf:info(?MODULE, "Storing post OK", []);
                Err ->
                    wf:error(?MODULE, "Error storing post: ~p", [Err])
            end;
        {_Bid, undefined, _} -> ok;
        {_Bid, [], _} -> ok;
        {_Bid, _, []} -> ok;
        {Bid, Topic, Message} -> % new thread creation
            % case {write_final_post(Topic, Message), get(?THREAD_ID)} of
                % {{ok, _Post}, Tid} when Tid =/= undefined ->
			case write_final_post(Topic, Message, Type) of
				{ok, Post} ->
                    % erase(?STORED_POST_ID),
                    wf:redirect(wf:f("/board?board=~p", [Bid]));
                Err -> wf:error(?MODULE, "Error storing thread: ~p", [Err])
            end
        % Unknown -> wf:error(?MODULE, "Unknown context for creating item: ~p", [Unknown])
    end;
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
event({remove_thread, Tid}) ->
	wf:info(?MODULE, "Removing thread: ~p", [Tid]),
	u:restricted_call(fun() ->
		remove_thread(Tid)
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
%% tuple('attachment', p, n, t, s, d, bin(r.result)))));
% event({binary, {sasay, A,B,C,D,E, Bin}} = _T) ->
%     wf:info(?MODULE, "TEST: ~p ~p ~p ~p ~p ~p", [size(Bin),A,B,C,D,E]);
event({binary, {attachment, Position, Name, _Type, _Size, Date, Data}}) ->
    wf:info(?MODULE, "OLOLO-BINARY: ~p", [size(Data)]),
    PostID = case get(?STORED_POST_ID) of
        undefined ->
            case write_temp_post(undefined, undefined) of
                {ok, Post} ->
                    Post#post.id;
                {error, _R} ->
                    % TODO: ОТправлять сообщение на клиент о неудачной загрузке
                    undefined
            end;
        Id ->
            Id
    end,
    case write_attachment(PostID, Data, Position, Name, Date, "description") of
        {ok, Unique, Path} ->
            wf:info(?MODULE, "BINARY-ID: ~p | ~p", [Unique, Path]);
        {error, Rw} ->
            % TODO: ОТправлять сообщение на клиент о неудачной загрузке
            wf:info(?MODULE, "Error uploading file: ~p", [Rw])
    end;
event({client, {store_thread, Topic, Message}}) ->
    write_temp_post(Topic, Message);
event({client, {store_post, Message}}) ->
    write_temp_post(undefined, Message);
% event({client, {attachment, Position, Name, Type, Size, Date, Data}}) ->
%     wf:info(?MODULE, "test: ~p ~p ~p ~p ~p ~p", [Position, Name, Type, Size, Date, Data]);
event({binary, {remove_attachment, Position, FileName}}) ->
    wf:info(?MODULE, "Removing attachment: ~p ~p", [Position, FileName]),
    R = case get(?STORED_POST_ID) of
        undefined ->
            {error, "Post ID is undefined"};
        PostID ->
            case kvs:get(feed, {post, PostID}) of
                {ok, F} ->
                    Attachments = kvs:traversal(attachment, F#feed.top, F#feed.entries_count, #iterator.prev),
                    NotRemoved = lists:takewhile(fun(A) ->
                        case A#attachment.position =:= Position of
                            true ->
                                case kvs:remove(A) of
                                    ok ->
                                        wf:info(?MODULE, "Temporary attachment ~p removed.", [A#attachment.id]),
                                        case file:delete(wf:f("~s/~s", [?SITE_ROOT_LOCATION, A#attachment.path])) of
                                            ok ->
                                                wf:info(?MODULE, "Temporary attachment FILE ~p removed.", [A#attachment.path]);
                                            {error, Errd} ->
                                                wf:error(?MODULE, "Temporary attachment FILE ~p NOT removed: ~p", [A#attachment.path, Errd])
                                        end;
                                    ErrA -> wf:error(?MODULE, "Temporary attachment ~p NOT removed: ~p", [A, ErrA])
                                end,
                                false;
                            false ->
                                true
                        end
                    end,Attachments),
                    case {Attachments, NotRemoved} of
                        {A,N} when length(A) =:= length(N) ->
                            {error, "Attachment not removed from DB or Disk!"};
                        _ ->
                            ok
                    end;
                ErrK ->
                    {error, wf:f("Attachment with position ~p of feed ~p NOT removed:", [Position, {post, PostID}, ErrK])}
            end
    end,
    case R of
        ok ->
            % {reply, {remove_attachment, Position, ok}};
            undefined;
        Err ->
            wf:error(?MODULE, "Removing attachment error: ~p", [Err]),
            % {reply, {remove_attachment, Position, error}}
            some_error
    end;           
event({binary,_Message}) ->
        wf:info(?MODULE, "This API will return Raw Binary", []),
        <<84,0,0,0,108>>;
event({server,{converted, Data, {meta, PostID, Position, Name, Date, Description}}}) ->
	% wf:info(?MODULE, "Recieved SERVER message: ~p", [Data]),
	case Data of
		{ok, {Mime2,Ext2}, Converted} ->
            Unique = unique:node_utc_random(),
            % {ok, Location} = file:get_cwd(),
            wf:info(?MODULE, "Current directory: ~p", [file:get_cwd()]),
            Path = wf:f("~s/~s.~s", [?ATTACHMENT_LOCATION, wf:to_list(Unique), wf:to_list(Ext2)]),
            case file:write_file(wf:f("~s/~s", [?SITE_ROOT_LOCATION, Path]), Converted) of
                ok ->
                    kvs:add( #attachment {
                        created = erlang:now(),
                        id=kvs:next_id(attachment, 1),
                        uuid=unique:node_utc_random(),
                        feed_id = {post, PostID},
                        position = Position,
                        path = Path,
                        description = wf:to_list(Description),
                        name = wf:to_list(Name),
                        size = size(Converted),
                        date = wf:to_list(Date),
						user = u:id(),
                        mime = Mime2
                        }),
					wf:wire(wf:f("fileLoadFinished(~b);", [Position])),
                    {ok, Unique, Path};
                {error, R} ->
                    wf:error(?MODULE, "Error write file: ~p", [Path]),
                    {error, R}
            end;
		E -> E
	end,
	ok;
	
event(terminate) ->
    wf:info(?MODULE, "terminate: STORED_POST_ID: ~p", [get(?STORED_POST_ID)]),
	?SESSION:erase_param(?MODULE),
    case get(?STORED_POST_ID) of
        undefined ->
            wf:info(?MODULE, "terminate: ok", []),
            ok;
        PostID ->
            % wf:info(?MODULE, "terminate: PostID: ~p", [PostID]),
            case kvs:get(feed, {post, PostID}) of
                {ok, F} ->
                    % wf:info(?MODULE, "terminate: {ok,F} clause: ~p", [PostID]),
                    Attachments = kvs:traversal(attachment, F#feed.top, F#feed.entries_count, #iterator.prev),
                    lists:map(fun(A) ->
                        case kvs:remove(A) of
                            ok ->
                                wf:info(?MODULE, "Temporary attachment ~p removed.", [A#attachment.id]),
                                case file:delete(wf:f("~s/~s", [?SITE_ROOT_LOCATION, A#attachment.path])) of
                                    ok ->
                                        wf:info(?MODULE, "Temporary attachment FILE ~p removed.", [A#attachment.path]);
                                    {error, Errd} ->
                                        wf:error(?MODULE, "Temporary attachment FILE ~p NOT removed: ~p", [A#attachment.path, Errd])
                                end;
                            ErrA -> wf:error(?MODULE, "Temporary attachment ~p NOT removed: ~p", [A, ErrA])
                        end
                    end,Attachments),
                    case kvs:remove(F) of
                        ok -> wf:info(?MODULE, "Temporary feed ~p removed.", [F#feed.top]);
                        ErrF -> wf:error(?MODULE, "Temporary attachment ~p NOT removed: ~p", [F, ErrF])
                    end;
                Err ->
                    wf:info(?MODULE, "Skip removing attachment of feed ~p: ~p", [{post, PostID}, Err])
            end,
            wf:info(?MODULE, "terminate: removing tmp post: ~p", [PostID]),
            case kvs:remove(post, PostID) of
                ok ->
                    wf:info(?MODULE, "Temporary post ~p removed.", [{thread, PostID}]);
                ErrP ->
                    wf:error(?MODULE, "Temporary post ~p NOT removed: ~p", [{thread, PostID}, ErrP])
            end,
            case {get(?BOARD_ID), get(?THREAD_ID)} of % removing temp thread
                {undefined, _Tid} -> ok;
                {_Bid, Tid} -> remove_thread(Tid)
            end     
    end;
event(Event) -> guard:shared_event(?MODULE, Event).

event(#ev{msg={chat,Pid}},#cx{req=_Req}=Cx) -> Pid ! {peer(), wf:qp(message,Cx)};
event(_Event,_) -> skip.

chat_loop() ->
    receive {Peer, Message} -> 
       wf:insert_bottom(history,#panel{id=history,body=[Peer,": ",Message,#br{}]}),
       wf:flush(room) end, chat_loop().

write_temp_post(Topic,Message) -> write_post(Topic,Message,undefined,true).
write_final_post(Topic,Message,ThreadType) ->
	case write_post(Topic,Message,ThreadType,false) of
		{ok, #post{feed_id={thread,Tid}}=Post} ->
			case ThreadType of
				request ->
					case {?SESSION:erase_param(?MODULE), kvs:get(thread,Tid)} of
						{{new_thread_request,E,Eid},{ok,#thread{feed_id={board,Bid}}=Thread}} ->
							kvs:put(Thread#thread{request_to={E,Eid}, type=ThreadType}),
							% set_board_request_thread(Bid,Thread);
							case kvs:get(board, Bid) of
								{ok, Board} -> kvs:put(Board#board{request_thread=Tid, access=private});
								_ -> wf:error(?MODULE,"Update request elements failed (board,~p,thread,~p)",[Bid,Tid])
							end;
						_ -> wf:error(?MODULE,"Update request elements failed (thread,~p)",[Tid])
					end;
				_ -> ok
			end,

			erase(?STORED_POST_ID), {ok, Post};
		E -> E
	end.

%private fun
write_post(T,M,ThreadType,IsTemp) ->
    
	wf:info(?MODULE, "Write post T: ~p M: ~p T:~p", [T, M, IsTemp]),
	
	Message = wf:to_binary(guard:prevent_undefined(M,<<>>)),
	Topic = wf:to_binary(guard:prevent_undefined(T,<<>>)),
	
	%% TODO: case get(?BOARD_ID) of undefined -> new_post; _ -> new_thread end
    %% kvs:next_id/2 -> mnesia:dirty_update_counter({id_seq, RecordName}, Incr).

	PostType = case get(?MARKDOWN_ENABLED) of undefined -> undefined; _ -> markdown end,
		
    case get(?STORED_POST_ID) of
		_ when IsTemp =:= false andalso Message =:= <<>> ->
			wf:info(?MODULE, "Ignore viod message: ~p", [M]), {error, viod_message};
        undefined ->
            Tid = case get(?BOARD_ID) of
                undefined ->
                    get(?THREAD_ID);
                _Bid ->
                    ThreadID = kvs:next_id(thread, 1),
                    put(?THREAD_ID, ThreadID),
                    ThreadID
            end,
            Post = #post {
                id=kvs:next_id(post, 1),
                created=erlang:now(),
				type=PostType,
				head=case get(?BOARD_ID) of undefined -> undefined; _ -> true end,
                user=u:id(),
                feed_id={thread, Tid},
                message=Message,
				netw_info={wf:peer(?REQ), proplists:get_value(<<"user-agent">>, element(17,?REQ),undefined)},
                temporary=IsTemp
                },
            % wf:info(?MODULE, "Post: ~p", [Post]),
            case kvs:add(Post) of
                {ok, New} ->
                    put(?STORED_POST_ID, New#post.id),
                    wf:info(?MODULE, "New: ~p", [New]),
                    case get(?BOARD_ID) of
                        undefined -> ok;
                        Bid -> % new thread creation
							% RequestTo = case ThreadType of
								% request -> {new_thread_request,E,Eid} = ?SESSION:erase_param(wf:qs(<<"x">>)), {E,Eid};
								% _ -> undefined
							% end,
							% wf:info(?MODULE, "{ThreadType, RequestTo} = ~p", [{ThreadType, RequestTo}]),
							{ok, Thread}=board:add_new_thread(Bid, Tid, Topic, New#post.id, false, false)
							% set_board_request_thread(Bid,Thread,IsTemp)
                    end,
                    {ok, New};
                {error, Reason} ->
                    wf:error(?MODULE, "Error when adding a #post to the database: ~p~n~p", [Reason, Post]),
                    {error, Reason}
            end;
        PostID ->
            case kvs:get(post, PostID) of
                {ok, Stored} ->
					case get(?BOARD_ID) of
						undefined -> ok;
						Bid -> % new thread creation
							wf:info(?MODULE,"Updating topic of thread ~p.", [get(?THREAD_ID)]),
							{ok, Thread} = kvs:get(thread, get(?THREAD_ID)),
							Thread2=Thread#thread{topic=Topic},
							ok = kvs:put(Thread2)
							% set_board_request_thread(Bid,Thread2,IsTemp)
					end,
					wf:info(?MODULE, "Updating post ~p ok", [PostID]),
					Stored2 = Stored#post{message=Message,temporary=IsTemp,type=PostType},
                    case kvs:put(Stored2) of
						ok -> {ok, Stored2};
						E -> {error, E}
                    end;
                {error, Reason} ->
                    wf:error(?MODULE, "Error when updating a #post to the database: ~p", [Reason]),
                    {error, Reason}
            end
    end.
		  
write_attachment(PostID, Data, Position, Name, Date, Description) when (PostID =/= undefined) and is_binary(Data) and (size(Data) >= 2) ->
    case image:mime_type(Data) of
        undefined ->
            {error, wf:f("Not allowed file type: ~p", [<<Data:16/bitstring>>])};
        {Mime, Ext} ->
			image:convert_async(self(), {Mime, Ext}, Data, photo, {meta, PostID, Position, Name, Date, Description})
    end;
write_attachment(PostID, _Data, _Position, Name, _Date, _Description) ->
    {error, wf:f("Wrong file: post_id=~p name=~p", [PostID, Name])}.

remove_thread(Tid) ->
    case kvs:get(feed, {thread, Tid}) of %% removing posts & attachments
        {ok, TF} ->
            wf:info(?MODULE, "Removing posts of thread ~p", [Tid]),
            Posts = kvs:traversal(post, TF#feed.top, TF#feed.entries_count, #iterator.prev),
            lists:map(fun(P) ->
                case kvs:get(feed, {post, P#post.id}) of
                    {ok, PF} ->
                        Attachments = kvs:traversal(attachment, PF#feed.top, PF#feed.entries_count, #iterator.prev),
                        lists:map(fun(A) -> kvs:remove(A) end, Attachments);
                    _ -> ok
                end,
                kvs:remove(P)
                end, Posts);
        Err ->
            wf:warning(?MODULE, "Thread feed ~p not found: ~p", [{thread, Tid}, Err])
    end,
    case kvs:remove(thread, Tid) of
        ok -> wf:info(?MODULE, "Thread ~p removed", [Tid]);
        Error -> wf:error(?MODULE, "Thread ~p not removed: ~p", [Tid, Error])
    end,
    kvs:delete(feed, {thread, Tid}), % raw ops (feed not contain container) already returning ok atom
    ok.

%image_viewer() ->
%    #panel{ id= <<"image-viewer-overlay">>, body = [
%        #panel{ id = <<"image-viewer">>, body = [
%            #panel { id = <<"image-viewer-container">>, body = [
%                #image{ id = <<"image-viewer-picture">> },
%                #panel{ id = <<"image-viewer-info">>, body = "sasay lalka" }
%            ]}
%        ]}
%    ]}.
    
update_post(Id) ->
	HexId=wf:f("post-~.36b", [Id]),
	case kvs:get(post, Id) of
		{ok, Post} ->
			case html_post(Post) of
				{ok, Html} -> wf:update(HexId, Html);
				_ -> wf:remove(HexId)
			end;
		Err ->
			wf:remove(HexId),
			wf:warning(?MODULE, "Updating post failed ~p", [Err])
	end, ?JS_IMAGE_LIST_CHANGED_LESTENER, ok.
add_post(#post{}=Post) ->
	case html_post(Post) of
		{ok, Html} -> wf:insert_bottom(posts, Html);
		_ -> skip end, ?JS_IMAGE_LIST_CHANGED_LESTENER, ok;
add_post(Id) ->
	case kvs:get(post, Id) of
		{ok, Post} -> add_post(Post);
		Err -> wf:warning(?MODULE, "Adding post failed ~p", [Err]) end.
	