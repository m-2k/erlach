-module(html). %% HTML PARTS
-vsn('0.0.0').

-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/feed.hrl").
-include_lib("db/include/post.hrl").
-include_lib("db/include/attachment.hrl").

-include_lib("db/include/board.hrl").
-include_lib("db/include/thread.hrl").
-include_lib("db/include/user.hrl").
-include("erlach.hrl").
-include("style.hrl").

info(Message) -> message(Message, <<"info-message">>).
success(Message) -> message(Message, <<"success-message">>).
warning(Message) -> message(Message, <<"warning-message">>).
error(Message) -> message(Message, <<"error-message">>).
message(Message, Class) ->
    Id=wf:temp_id(),
    wf:insert_top(<<"popup-messages">>, #link{id=Id, class=Class,
            onclick=wf:f("qi(\\'~s\\').parentNode.removeChild(qi(\\'~s\\'));",[Id,Id]),
            body=guard:html_escape(Message)}),
    wf:wire(wf:f("window.setTimeout(function(){var a=qi('~s'); if(a){a.onclick();}},~b);",[Id,size(wf:to_binary(Message))*100+4000])),
    ok.
    % wf:wire(wf:f("var m=qi('~s');m.style.webkitAnimationName=m.style.animationName='popup-slide-left';", [Id])).

title(_Module, ?ACTION_API) ->
    Text = case {Level,Action} of
        {thread,view} -> Thread#thread.name;
        {thread, _} -> [<<"New thread in ">>, Board#board.name];
        {board,view} -> Board#board.name;
        _ -> "Erlach"
    end,
    guard:to_binary([Text, ?TITLE_DELIMETER, ?SITE_NAME]).

header() -> header(u:get()).
header(User) -> [#panel{id= <<"header">>,body=[#link{href="/",class= <<"erlach-logo">>,body=logo()},html:menu(User)]}].

menu() -> menu(u:get()).
menu(User) ->
    Class=?HEADER_LINKS,
    [ #panel{class= <<"links right">>,body=[
            #link {class=Class, href= <<"/donate">>, body= <<"Donate">>},
            #link {class=Class, href= <<"/profile">>, body= <<"Profile">>},
            case u:is_temp(User) of
                true -> #link {class=Class, body= <<"Sign in">>, postback=signin};
                false -> #link {class=Class, body= <<"Logout">>, postback=logout}
            end ]},
        #panel{class= <<"links left">>,body=[
            #link {class=Class, href= <<"#">>, body= <<"News">>},
            #link {class=Class, href=qs:ml({board,blog,<<"e">>}), body= <<"Blog">>} ]} ].


gettype() -> ?CTX#cx.path#route.type.

breadcrumbs2(?ACTION_API) ->
    Body = case {Level,Action} of
        {thread, view} -> #li{body=[#link{body= <<"thread">>},#link{body= <<"view">>}]};
        {thread, new} -> <<"thread new">>;
        {board, view} -> [#li{body=#link{body= <<"board">>}},#li{body=#link{body= <<"view">>}},#li{body=#link{body= <<"current">>}}]
    end,
    #panel{class= <<"center">>,body=#ul{class= <<"breadcrumb">>,body=Body}}.
    
    
breadcrumbs(?ACTION_API) ->
    Type = gettype(),
    Class = <<"link slim">>,
    Main = #link{class=Class,body= <<"Эрлач"/utf8>>,href=qs:ml(root) },
    
    wf:info(?MODULE, "BreadCrumbs: ~p",[Type]),
    
    Body = case {Level,Action,Type} of
        {thread,_,blog} -> [Main,
            #link{class=Class,body=Board#board.name,href=qs:ml({board,thread,Board#board.uri})},
            #link{class=Class,body= <<"Блог"/utf8>>,href=qs:ml({board,Type,Board#board.uri})}];
        {thread,_,_} -> [Main,#link{class=Class,body=Board#board.name,href=qs:ml({board,thread,Board#board.uri})}];
        {board,view,blog} -> [Main,#link{class= <<Class/binary," nolink">>,body= <<"Блог"/utf8>>}];
        {board,view,_} -> [Main]
    end,
    #panel{class= <<"breadcrumbs center">>,body=Body}.

footer() ->
    Class=?FOOTER_LINKS,
    #panel{id= <<"footer">>,body = [
        #panel{class= <<"related-links">>,body=[
            #link{class=Class, target="_blank", body= <<"Erlang Powered">>, href= <<"http://erlang.org">>},
            #link{class=Class, target="_blank", body= <<"SynRC">>, href= <<"https://synrc.com">>},
            #link{class=Class, target="_blank", body= <<"EoX">>, href= <<"http://erlangonxen.org">>},
            #link{class=Class, target="_blank", body= <<"Tails">>, href= <<"https://tails.boum.org">>},
            #link{class=Class, target="_blank", body= <<"Tor">>, href= <<"https://www.torproject.org">>},
            #link{class=Class, target="_blank", body= <<"Digital Ocean">>, href= <<"https://digitalocean.com">>},
            #link{class=Class, target="_blank", body= <<"N2O">>, href= <<"https://github.com/5HT/n2o">>},
            #link{class=Class, target="_blank", body= <<"Cowboy">>, href= <<"https://github.com/ninenines/cowboy">>},
            #link{class=Class, target="_blank", body= <<"NoSQL">>, href= <<"http://www.erlang.org/doc/apps/mnesia/">>},
            #link{class=Class, target="_blank", body= <<"BPG Ready">>, href= <<"http://bellard.org/bpg/">>},
            #link{class=Class, target="_blank", body= <<"Markdown">>, href= <<"https://help.github.com/articles/markdown-basics/">>},
            % #link{class=Class, body= <<"Flex CSS">>, href= <<"http://pepelsbey.net/pres/flexbox-gotcha/?full#Cover">>},
            #link{class=Class, body= <<"Erlach IBS, 2015"/utf8>>, href= <<"/privacy">>}
        ]}
    ]}.

body(Content) -> [ html:header(u:get()), #panel{ id= <<"content">>, body=Content}, html:footer(),#panel{id= <<"popup-messages">>}].


thread_body_private({Access, Board, Thread, {thread, Action, Data}}=S) ->
    Head = [ #panel{class= <<"content-title">>,body="Access denied"}, #span{class= <<"remark">>,body="Remark"} ],
    Manage = #panel{class= <<"center">>,body=[
        #link{class = <<"button primary slim">>, body = <<"Request access">>, postback=null } ]},
    [Head,Manage].

thread_body({Access, Board, Thread, {thread, Action, Data}}=S) ->
    
    case config:debug() of true -> html:info("ACCESS: " ++ wf:to_list(Access)); _ -> ok end,

    AllowWrite = access:is_allow(message,write,Access),
    wf:info(?MODULE, "State: ~p",[AllowWrite]),
    BreadCrumbs=breadcrumbs(S),
    
    Content = case Action of
        view ->
            {Topic, ThType, ReqTo} = {Thread#thread.name,Thread#thread.type,Thread#thread.request_to},
            {ok,Head,Posts}=posts_list(Thread,Access),
            [ #panel{class= <<"content-title">>,body=Topic},
                case ThType of
                    request when ReqTo =/= undefined ->
                        #span{class= <<"remark">>,body= <<"This thread is request for ", (wf:to_binary(wf:to_list(ReqTo)))/binary>>};
                    _ -> []
                end,    
                #panel{id=posts,body=[Head,Posts]} ];
        create -> [ ]
    end,
    #panel{id=imageboard,body=[BreadCrumbs,Content,input_form(S),image_viewer()]}.

input_form_id() -> <<"drag-input-form">>.
input_form({Access, Board, Thread, {Type, Action, Data}}=S) ->
    case access:is_allow(message,write,Access) of % allow write
        true ->
            AllowMarkdown = access:is_allow(blog,write,Access),
            {Topic,BClass,BBody,BSource,BPostback,Text,CatSelector} = case Action of
                view -> {[],<<"button primary">>,<<"Send message">>,[message,name_selector],{store,finalize},<<>>,[]};
                create -> {#textbox{id= <<"topic">>, class= <<"textarea topic">>, placeholder= <<"Topic">> },
                    <<"button success">>,<<"New thread">>,[topic,message,name_selector]++categories_list(Board),{store,finalize},<<>>,categories_selector(Board)};
                edit ->
                    #post{markup=Markup,message=Message}=erlang:get(post),
                    M2=guard:plaintext_escape(Message),
                    {[],<<"button info">>,<<"Update message">>,[message,name_selector],{edit,finalize},M2,[]}
            end,
            #panel{id=input_form_id(),class= <<"drag-accepted">>,body=[
                #panel{id= <<"text-input-form">>,body=[
                    Topic,
                    #textarea{id= <<"message">>,class= <<"textarea autostore">>,placeholder= <<"Message">>,body=Text,rows= <<"1">>},
                    CatSelector,
                    #panel{class= <<"right-container">>, body=[
                        name_selector(u:get()),
                        if AllowMarkdown -> #link{id=markdown,class= <<"link info compact">>,
                            body= <<"Enable Markdown">>,postback=enable_markdown};
                            true -> [] end,
                        #link{id= <<"store">>,class=BClass,body=BBody,postback=BPostback,source=BSource} ]},
                    #panel{id= <<"thumbnail-list">>}]}]};
        _ -> [] end.

categories_selector(#board{category=Categories}) ->
    #panel{class= <<"category-selector">>,body=[
        lists:map(fun({Cid,Ctext}) ->
            % #link{class= <<"button slim lime">>, body=C}
            #checkbox{id= <<"category-", (wf:to_binary(Cid))/binary>>,body=wf:to_list(Ctext)}
            end,Categories) ]}.

posts_list(#thread{id=Tid,type=Type,request_to=ReqTo}=Thread,Access) ->
    Uid = u:id(),
    IsAdmin = u:is_admin(),
    
    case kvs:get(feed, {post, Tid}) of
        {ok, F} ->
            AllPosts = kvs:traversal(post, F#feed.top, F#feed.entries_count, #iterator.prev),
            % wf:wire(#transfer{state=[{items,AllPosts}]}),
            {Head, Posts} = lists:foldl(fun(P, {H,Acc}) ->
                case {H,P#post.head,post(Thread,P,IsAdmin,Uid,Access)} of
                    {undefined,true, {ok, Html}} -> {Html,Acc};  % first head post
                    {_,        false,{ok, Html}} -> {H,[Html|Acc]}; % message posts
                    % {_,{skip, access}} -> {H,Acc};
                    _ -> {H,Acc}
                end end, {undefined,[]}, AllPosts),
            {ok, Head, Posts};
        Err ->
            wf:warning(?MODULE, "Thread feed ~p not found: ~p", [{thread, Tid}, Err]),
            {error, feed_not_found}
    end.

post_check_visibility(Uid,IsModerate,#thread{type=ThType,request_to=ReqTo},#post{deleted=Deleted,head=IsHead,temporary=IsTemp,user=User}) ->
    case {IsModerate,IsHead,IsTemp,Deleted,ThType,User} of
        {_,_,true,_,_,_} -> skip;
        {true,_,_,_,_,_} -> ok;
        {_,_,_,true,_,_} -> skip;
        {_,true,_,_,_,_} -> ok;
        {_,_,_,_,request,Uid} when ReqTo =/= undefined -> ok;
        {_,_,_,_,request,_} -> skip;
        _ -> ok
    end.

post(#post{feed_id={post,Tid}}=P) ->
    {ok, Thread} = kvs:get(thread,Tid),
    post(Thread,P, u:is_admin(),u:id(),erlang:get(access)).
post(#thread{id=Tid,type=ThreadType,request_to=ReqTo,user=Tu}=Thread, #post{id=Id,type=PostType,feed_id ={post,Tid},temporary=IsTemp,message=Message, created=Timestamp,user=User,deleted=Deleted,head=IsHead,markup=Markup}=Post, IsAdmin,Uid,Access) ->

    % wf:info(?MODULE, " >>> html_post Message: ~p", [Message]),
    
    IsBlog = case {IsHead,ThreadType} of {true,blog} -> true; _ -> false end,
    % {{_Y,_M,_D},{Hour,Minute,Second}} = calendar:now_to_local_time(Timestamp),
    Text = utils:html_message(Post),
    % wf:info(?MODULE, " >>> html_post Converted message: ~p", [Text]),
    
    IsSelfPost = User =:= Uid,
    CanDelete = IsSelfPost orelse access:is_allow(message,moderate,Access),
    CanRestore = access:is_allow(message,moderate,Access),
    CanEdit = IsSelfPost andalso not utils:expired(Timestamp,config:expire_time_to_edit_messages()),

    Uid = u:id(),
    % wf:warning(?MODULE,"~n~n uids: ~p ~p",[Tu,User]),
    IsModerate = case {Tu,User} of {Uid,_} -> true; {_,Uid} -> true; _ -> false end,
    % Access = get(access),
    V = post_check_visibility(Uid,IsModerate,Thread,Post),
    wf:info(?MODULE, " >>> html_post Visible: ~p", [V]),
    case {post_check_visibility(Uid,IsModerate,Thread,Post),Deleted} of
        {ok,undefined} ->
            Class = if IsBlog -> <<"thread-blog">>; true -> <<"thread-post">> end,
            ReplyId = utils:hex_id({reply,Id}),
            Html = #panel { id = utils:hex_id({post,Id}), class = Class, body = [
                % #panel{ class = <<"timestamp">>, body = guard:html_escape(wf:f("~2w:~2..0w:~2..0w", [Hour, Minute,Second])) },
                % #panel{ class = <<"right-side">>, body = [
                    case {CanDelete, IsHead} of
                        {true, true} -> #link{ class = <<"link danger compact">>, body = <<"Delete thread">>, postback = {hide_thread, Tid} };
                        {true, _} -> #link{ class = <<"link danger compact">>, body = <<"✕"/utf8>>, postback = {hide_post, Id} };
                        _ -> [] end,
                    case CanEdit of true -> %html:error(wf:to_list(Id)),
                        #link{ class = <<"link info compact">>, body = <<"✎"/utf8>>, postback = {edit_post, Id} }; _ -> [] end,
                    case ThreadType of
                        request when ReqTo =/= undefined andalso IsModerate =:= true -> % andalso IsAdmin =:= true
                            
                            case u:id() of
                                User -> []; % ignore self messages
                                _ ->
                                    % wf:warning(?MODULE,"JOIN: ~p ~p",[get(access),u:check_access(User,ReqTo)]),
                                    {Level,Lid} = ReqTo,
                                    case access:acl({{user,User}, {private,Level,Lid}}) of
                                        {_Time1,_Time2} -> [];
                                        _ -> #link{class= <<"button success">>,
                                            body= "Join user " ++ wf:to_list(User) ++ " to " ++ wf:to_list(ReqTo),
                                            postback = {request, accepted, ReqTo, User, Id} } end end;
                        _ -> []
                    end,
                    case IsHead of true -> []; _ -> #span{ class = <<"username">>, body = case Post#post.user_name of
                        anonymous -> <<"anonymous">>;
                        Uname -> guard:html_escape(Uname) end } end,
                    #span{ class = <<"message">>, body = Text },
                    #link{ id=ReplyId, class = <<"reply link primary compact">>, body = <<"↩"/utf8>>, data_fields=[{<<"data-id">>,wf:to_integer(Id)}] },
                    #panel{ class = <<"post-attachment">>, body = post_attachment(Post) }
                %    ]}
                ]},
                Wire = "qi(\"reply-"++utils:hex_id(Id)++"\").addEventListener(\"click\", function(e) {"++
                "var message = qi(\"message\"); message.value = message.value + \">>\" + this.dataset.id + \" \";});",
                wf:info(?MODULE,"bind post: ~p",[Wire]),
                wf:wire(Wire),
            {ok, Html};
        {ok,_} ->
            case CanRestore of
                true ->
                    Html = #panel { id = utils:hex_id({post,Id}), class = <<"thread-post">>, body = [
                            case IsHead of
                                true -> #link{ class = <<"link success compact">>, body = <<"Restore thread">>, postback = {show_thread, Tid} };
                                _ -> #link{ class = <<"link success compact">>, body = <<"⟳"/utf8>>, postback = {show_post, Id} }
                            end,
                            case IsHead of true -> []; _ -> #span{ class = <<"username">>, body = case Post#post.user_name of
                                anonymous -> <<"anonymous">>;
                                Uname -> guard:html_escape(Uname) end  } end,
                            #span{ class = <<"message">>, body = Text },
                            #panel{ class = <<"post-attachment">>, body = post_attachment(Post) }
                        %    ]}
                        ]},
                    {ok, Html};
                _ -> skip end;
        {Skip,_} -> Skip
    end.
        
post_attachment(#post{ id=Id } = _P) ->
    case kvs:get(feed, {attachment, Id}) of
        {ok, F} ->
            Attachments = kvs:traversal(attachment, F#feed.top, F#feed.entries_count, #iterator.prev),
            lists:map(fun(A) ->
                #image{ image = "/" ++ A#attachment.path, class = <<"image">> }
                end, lists:reverse(Attachments));
        NotFound ->
            wf:info(?MODULE, "Post ~p doesn't has attachments (feed: ~p reason: ~p)", [Id, {post, Id}, NotFound]),
            []
    end.


board_body_private({Access, Board, Thread, {board, Action, Data}}=S) ->
    case u:is_temp() of
        true ->
            html:body(#panel{class= <<"center">>, body=[
                #span{class= <<"content-title">>, body= <<"The board only for the elect">>},
                #span{class= <<"remark">>, body= <<"To use this board authorization is needed and submit a request.">>},
                #link{class= <<"button primary">>, body= <<"Auth">>, postback=signin}]});
        _ ->
            html:body(#panel{class= <<"center">>, body=[
                #span{class= <<"content-title">>, body= <<"The board only for the elect">>},
                case Board#board.request_thread of
                    undefined -> [];
                    Tid -> #link{class= <<"button success">>, body= <<"Make request for access to this board">>,
                        url=qs:ml({thread, gettype(), Board#board.uri, Tid}) }
                end ]})
    end.

board_body({Access, #board{id=Bid,uri=Uri,feed_id=Bf,name=Name,description=Description,category=Cat}=Board, Thread, {board, Action, {Type,Bid}=Data}}=S) ->
    
    TeenGid = 9,
    if Bf =:= {board,TeenGid} -> html:info(wf:to_list(<<"Единый телефон доверия психологической помощи: 8-800-2000-122"/utf8>>)); true -> ok end,
    
    Route=?CTX#cx.path,
    case config:debug() of true -> html:info("ACCESS: " ++ wf:to_list(Access)); _ -> ok end,
    
    AllowRequest = access:is_allow(request,write,Access),
    AllowMessage = access:is_allow(message,write,Access),
    AllowBlog = access:is_allow(blog,write,Access),
    
    BreadCrumbs = breadcrumbs(S),
    Head = [ #panel{class= <<"content-title">>,body=Name}, #span{class= <<"remark">>,body=Description} ],
    Manage = #panel{class= <<"center">>,body=[
        case Data of
            {thread, _Bid} -> [ #link{class = <<"button dark slim">>, body = <<"Blog">>, href=qs:ml({board,blog,Uri}) },
                if AllowMessage -> #link{class = <<"button primary slim">>, body = <<"New">>,
                    postback={thread, create, {thread, Bid}} }; true -> [] end ];
            {blog, _Bid} -> [ #link{class = <<"button dark slim">>, body = <<"Threads">>, href=qs:ml({board,Uri}) },
                if AllowBlog -> #link{class = <<"button primary slim">>, body = <<"New blog">>,
                    postback={thread, create, {blog, Bid}} }; true -> [] end ] end,
        if AllowRequest ->
            case Board#board.request_thread of
                undefined -> #link{class= <<"button info slim">>, body= <<"New request">>,postback={thread, create, {request, {board, Bid}}}};
                Rtid -> #link{class= <<"button success slim">>, body= <<"View request">>, href=qs:ml({thread,gettype(),Uri,Rtid}) }
            end; true -> [] end,
        lists:map(fun({Cid,Ctext}) ->
            Class = case Route#route.category =:= wf:to_binary(Cid) of
                true -> <<"button lime slim selected">>;
                _ -> <<"button lime slim">> end,
            #link{class=Class,body=wf:to_binary(Ctext),href=qs:ml({category,Type,Uri,Cid})}
            end,Cat) ]},
    Main = #panel{id= <<"threads">>, class= <<"line">>, body=board_content(S)},
    Settings = case u:is_admin() of true -> settings_panel(board, Board); _ -> [] end,
    html:body([BreadCrumbs,Head,Manage,Main,Settings,image_viewer()]).

board_content({Access, #board{id=Bid}=Board, Thread, {board, Action, Data}}=S) ->
    IsAdmin = u:is_admin(),
    Route=?CTX#cx.path,
    
    case kvs:get(feed, {thread, Bid}) of
        {ok, TF} ->
            wf:info(?MODULE, "body feed ~p", [TF]),
            Threads = kvs:traversal(thread, TF#feed.top, TF#feed.entries_count, #iterator.prev),
            {HeadPostList3, HeadBlogList2,RequestHeadPostList2} = lists:foldl(fun(T, {PL, BL, RL}) ->
                {ok, #post{type=Type}=P} = kvs:get(post, T#thread.head_post),

                % TODO: Last post must overwrite #thread.last_post_date
                {ok, #feed{top=LastPostId}} = kvs:get(feed, {post, T#thread.id}),
                {ok, #post{created=LastTimestamp}} = kvs:get(post, LastPostId),
                        
                case {is_exist_in_category(T,Route#route.category), T#thread.type} of
                    {true,thread} -> {[{LastTimestamp,T,P}|PL], BL, RL};
                    {true,blog} -> {PL, [{T,P}|BL], RL};
                    {true,request} -> {PL, BL, [{T,P}|RL]};
                    _ -> {PL, BL, RL}
                end
            end,{[],[],[]},Threads),
            wf:info(?MODULE, "Head post list: ~p", [HeadPostList3]),

            case Data of
                {blog, _Bid} -> #panel{ body = [
                        lists:map(fun({T,P}) -> html_thread(Board,T,P) end, lists:reverse(HeadBlogList2))
                    ]};
                {thread, _Bid} ->
                    % Bump sorting TODO: inject thread-element to top in feed when post written
                    SortedPostList = lists:sort(fun({TS1,_T1,_P1},{TS2,_T2,_P2}) -> TS1 >= TS2 end, HeadPostList3),

                    #panel{ body = [
                        lists:map(fun({_LastTimestamp,T,P}) -> html_thread(Board,T,P) end, SortedPostList)
                    ]}
            end;
        _Empty -> []
    end.

is_exist_in_category(_,undefined) -> true;
is_exist_in_category(#thread{category=Categories},RouteCategory) ->
    lists:any(fun(E) -> E =:= RouteCategory end,lists:map(fun(C)-> wf:to_binary(C) end,Categories)).


html_thread(#board{uri=Uri},#thread{id=Id,name=Topic,type=ThreadType}=_Thread,#post{message=Message,created=_Timestamp,user=User,head=IsHead,markup=Markup}=Post) ->
    
    IsBlog = case {IsHead,ThreadType} of {true,blog} -> true; _ -> false end,
    Class = case IsBlog of true -> <<"board-blog">>; _ -> <<"board-thread">> end,
    Text = utils:html_message(Post),
    
    #panel {id=utils:hex_id({thread,Id}),class=Class,body=[
            #panel{class= <<"thread-topic">>,body=[
                #link{class= <<"link">>,
                    body=case guard:html_escape(Topic) of <<>> -> wf:f("#~w",[Id]); T -> T end,
                    href=qs:ml({thread,gettype(),Uri,Id})}
            ]},
            case IsBlog of true -> []; _ -> #span{class= <<"username">>,body= <<"anonymous">>} end,
            #span{class= <<"message">>,body=Text},
            #panel{class= <<"post-attachment">>,body=html:post_attachment(Post)}
        ]}.

% html_blog([]) -> [];
% html_blog(HeadBlogList2) ->
%     {#thread{id=Id, name=Topic}, #post{}} = lists:last(HeadBlogList2),
%     #panel { class = <<"board-thread">>, body = [
%             %         #link{
%             % body = case guard:html_escape(Topic) of <<>> -> wf:f("#~w", [Id]); T -> T end,
%             % href = wf:f("/thread?id=~p", [Id]) },
%         #link{
%             body = guard:html_escape(<<"Вся лента блога ("/utf8, (wf:to_binary(length(HeadBlogList2)))/binary,") >>>>"/utf8>>),
%             href = wf:f("/board?id=~p&blog", [get(?BOARD_ID)]) }
%     ]}.
%
settings_panel(board, #board{}=B) ->
    #panel { body = [
        #panel { class = <<"flex-container">>, body = [
        
            #panel{ class = <<"group">>, body = [
                #span{ body = <<"Private control:"/utf8>>},
                #radio{id= <<"radio1">>, name= <<"board_private">>, body = <<"Any">>, value = <<"radio1">>, checked=true},
                #radio{id= <<"radio2">>, name= <<"board_private">>, body = <<"Registered">>, value = <<"radio2">>},
                #radio{id= <<"radio3">>, name= <<"board_private">>, body = <<"Requested">>, value = <<"radio3">>}
            ]},
            #panel{ class = <<"group">>, body = [
                #span{ body = <<"0:"/utf8>>}
            ]},
            #panel{ class = <<"group">>, body = [
                #span{ body = <<"View options:"/utf8>>},
                #checkbox{id=checkbox_anonymous,
                    body = <<"Anonymous mode for averyone">>,
                    checked=B#board.anonymous}
            ]},
            #panel{ class = <<"group">>, body = [
                #span{ body = <<"Properties:"/utf8>>},
                #textbox{ placeholder = <<"URI">>, value = guard:html_escape(B#board.uri) },
                #textbox{ placeholder = <<"Name">>, value = guard:html_escape(B#board.name) },
                #textbox{ id=unc, placeholder = <<"Description">>, value = guard:html_escape(B#board.description) }
            ]}
        ]},
        #button{class = <<"right">>, body="Apply", postback={apply_board, B#board.id}, source=[checkbox_anonymous]},
        #button{class = <<"right">>, body="UTF test", postback=utf_test, source=[unc]}
    ]}.


image_viewer() ->
    #panel{ id= <<"image-viewer-overlay">>, body = [
        #image{ id = <<"image-viewer-picture-back">> },
        #image{ id = <<"image-viewer-picture-front">> }]}.


username_form() -> username_form({name,lookup}).
username_form(Action) ->
    wf:info(?MODULE,"Action: ~p",[Action]),
    Content = case Action of
        {name,create} -> [ #textbox{id=username,class= <<"textarea">>,placeholder= <<"U name">> },
            #link{class= <<"button success">>, body= <<"Write">>, postback={name,write}, source=[username]},
            #link{class= <<"button warning">>, body= <<"Cancel">>, postback={name,lookup}} ];
        {name,modify,#name{id=Id,birthday=Bd}} -> [
            #span{body=Id},
            #textbox{id=about,class= <<"textarea">>,placeholder= <<"About">>,value=guard:html_escape(Bd) },
            #link{class= <<"button success">>, body= <<"Update">>, postback={name,update,Id}, source=[about]},
            #link{class= <<"button warning">>, body= <<"Cancel">>, postback={name,lookup}} ];
        _ -> username_list()
    end,
    #panel{id= <<"username-manage">>,class= <<"contact-modify-box">>,body=Content }.

username_list() ->
    case u:is_temp() of
        true -> [ #span{class= <<"remark">>,body= <<"U must be logged in to create names">>},
            #link{class= <<"button primary">>, body= <<"Sign in">>, postback=signin}];
        _ ->
            HtmlNameList = lists:foldl(fun(#name{id=Name},Acc) ->
                [ #link{class= <<"button dark">>,body=Name,postback={name,modify,Name}}|Acc ]
                end,[],u:names()),
            AddButton = #link{ class= <<"button primary">>, body= <<"Add name"/utf8>>, postback={name,create}},
            [HtmlNameList,AddButton]
    end.

name_selector(User) ->
    NameList=u:names(User),
    {_,Map,OptList}=lists:foldl(fun(#name{id=Id},{Num,Macc,Acc}) ->
        Opt = #option{id=wf:temp_id(),label="",value=Num,title="",body=guard:html_escape(Id)},
        {Num-1,maps:put(wf:to_list(Num),Id,Macc),[Opt|Acc]}
        end,{length(NameList),#{},[]},NameList),
    erlang:put(namelist_map,Map),
    wf:wire(#transfer{state=[{namelist_map,Map}]}),
    AnonOpt = #option{id=wf:temp_id(),label="",value=0,title="",body= "anonymous"},
    #select{id=name_selector,class= <<"name-selector">>,body=[
        #optgroup{label="Select name",body=[AnonOpt|OptList]} ]}.

name_selector_extract(Number) -> maps:get(Number,erlang:get(namelist_map),anonymous).

categories_list(#board{category=Categories}) ->
    % lists:map(fun({Cid,_Ctext}) -> wf:to_atom(<<"category-",(wf:to_binary(Cid))/binary>>) end,Categories).
    [Cid || {Cid,_Ctext} <- Categories].
categories_extract(#board{category=Categories}) ->
    lists:foldl(fun({Cid,Ctext},Acc) ->
        % QC= wf:to_atom(<<"category-",(wf:to_binary(Cid))/binary>>),
        % wf:info(?MODULE,"Cat: ~p",[wf:q(QC)]),
        case wf:q(Cid) of "on" -> [Cid|Acc]; _ -> Acc end
        end,[],Categories).

% ([0-9]{1,3})\.([0-9]{1})[0-9]+
% $1.$2
%
% ([0-9]{1,3})\.0([^0-9]{1})
% $1$2
%
% OR
%
% ([0-9]{1,3})\.([0-9]{1})[0-9]+
% $1

logo() -> % Different as original
    <<"<svg width='100px' height='20px' viewBox='-2 0 284 50' version='1.1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink'>
        <g stroke='#26c6da' stroke-width='2' fill='black' fill-rule='evenodd'>
            <path d='M21.6,2 L22.2,2 L22.9,1.8 L23,2 L23,24.6 L22.7,24.9 L0.2,24.9 C0,24.9 0,24.8 0,24.5 C0,19.4 1.8,14.3 5.6,9.2 C9.9,4.4 15.3,2 21.6,2 L21.6,2 Z M24.3,1.8 L25.5,1.8 C29.9,1.8 34.6,3.6 39.7,7.2 C42.6,9.9 44.3,12.2 45.1,14.2 C46.5,17.9 47.2,21.2 47.2,24.1 L47.1,24.1 L47.2,24.2 L47.2,24.6 L46.9,24.9 L24.3,24.9 L24,24.6 L24,2.1 L24.3,1.8 Z M22.7,25.7 L23,26 L23,48.7 C23,48.9 22.9,49 22.6,49 C18.7,48.5 15.2,47.6 12.3,46.4 C4.1,42.3 0,35.5 0,25.9 L0.1,25.7 L22.7,25.7 Z M70.8,1.8 L70.9,2 L70.9,24.6 L70.6,24.9 L48.1,24.9 C48,24.9 47.9,24.8 47.9,24.5 C47.9,20.4 49.2,16.1 51.8,11.4 C56,5 62.3,1.8 70.8,1.8 L70.8,1.8 Z M72.2,1.7 L94.8,1.7 L95.1,2 L95.1,3.1 C95.1,6.2 93.9,10.1 91.5,14.9 C87.2,21.6 80.8,24.9 72.1,24.9 L71.9,24.8 L71.9,2 L72.2,1.7 Z M70.6,25.7 L70.9,26 L70.9,48.7 L70.6,49 L70.4,49 C66.2,48.4 62.7,47.5 59.8,46.1 C51.9,42.1 47.9,35.3 47.9,25.9 L48,25.7 L70.6,25.7 Z M96.1,1.8 L96.2,1.8 C101.3,1.8 106.4,3.7 111.5,7.5 C116.3,11.8 118.7,17.2 118.7,23.5 L118.7,24.6 L118.4,24.9 L96.1,24.9 L95.8,24.6 L95.8,2.1 L96.1,1.8 Z M95.9,25.7 L118.6,25.7 L118.8,26 L118.8,48.7 L118.6,49 L118.1,49 C113.9,48.4 110.3,47.4 107.3,46 C102.3,43.4 98.9,39.6 97.2,34.7 C96.2,31.7 95.6,28.9 95.6,26.5 L95.8,26.5 L95.6,26.3 L95.6,26 L95.9,25.7 Z M120.3,25.7 C125.2,26.4 129.2,27.5 132.2,29.1 C139.3,33.4 142.9,39.9 142.9,48.8 L142.8,49 L120.1,49 L119.8,48.7 L119.8,26 C119.8,25.8 120,25.7 120.3,25.7 L120.3,25.7 Z M166.5,1.8 L166.6,2 L166.6,24.6 L166.3,24.9 L143.7,24.9 C143.5,24.9 143.4,24.8 143.4,24.5 C143.4,19.9 145.1,15 148.3,10 C152.6,4.6 158.7,1.8 166.5,1.8 L166.5,1.8 Z M167.8,1.8 L169.2,1.8 C173.4,1.8 178,3.6 183.1,7 C188.1,11.4 190.7,17.3 190.7,24.8 L190.5,24.9 L167.8,24.9 L167.5,24.6 L167.5,2.1 L167.8,1.8 Z M166.2,25.7 L166.5,26 L166.5,48.7 L166.2,49 L166.1,49 C162.4,48.5 159.1,47.7 156.2,46.6 C147.7,42.5 143.4,35.6 143.4,25.9 L143.6,25.7 L166.2,25.7 Z M190.4,25.7 L190.7,26 L190.7,48.7 L190.4,49 L190,49 C186.1,48.4 183.1,47.6 180.7,46.7 C171.9,42.7 167.5,35.8 167.5,25.9 L167.6,25.7 L190.4,25.7 Z M215.7,0.6 L215.8,0.6 C221.4,0.6 226.6,3 231.4,7.7 L231.4,7.9 L215.2,24.1 L199.1,7.9 L199.1,7.7 C204.3,3 209.9,0.6 215.7,0.6 L215.7,0.6 Z M198.4,8.5 L214.5,24.6 L214.5,24.8 L198.4,40.9 L198.2,40.9 C193.5,35.4 191.2,30.1 191.2,25 L191.2,23.2 C191.2,18.2 193.6,13.3 198.4,8.5 L198.4,8.5 Z M215.2,25.6 L215.4,25.6 L231.6,41.8 L231.6,41.9 C226,46.6 220.4,49 214.8,49 L214.7,49 C209,49 203.8,46.6 199.1,41.8 L215.2,25.6 Z M232.4,2 L233.1,2 C237.8,2 242.6,3.8 247.7,7.3 C252.7,11.6 255.2,17.4 255.2,24.9 L232.1,24.9 L232.1,2.3 L232.4,2 Z M279.1,1.8 L279.2,2 L279.2,24.6 L279,24.9 L256.5,24.9 L256.2,24.6 L256.2,23.8 C256.2,18.6 258.3,13.4 262.5,8.2 C266.7,3.9 272.3,1.8 279.1,1.8 L279.1,1.8 Z M279,25.7 L279.2,26 L279.2,48.7 C279.2,48.9 279.1,49 278.8,49 C274.4,48.4 271,47.6 268.5,46.4 C260.3,42.4 256.2,35.5 256.2,25.9 L256.3,25.7 L279,25.7 Z M232.4,25.9 L254.9,25.9 L255.2,26.2 C255.2,29.1 254.2,33 252.1,37.8 C247.8,45.2 241.2,49 232.3,49 L232.1,48.8 L232.1,26.2 L232.4,25.9 Z' id='erlach' fill='#505050'></path></g>
    </svg>">>.

logo2() -> % UGLY
    <<"<svg width='100px' height='20px' viewBox='-2 0 284 50' version='1.1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' xmlns:sketch='http://www.bohemiancoding.com/sketch/ns'>
        <g id='Page-1' stroke='#26c6da' stroke-width='2' fill='black' fill-rule='evenodd' sketch:type='MSPage'>
            <path d='M21,2 L22,2 L22,1 L23,2 L23,24 L22,24 L0,24 C0,24 0,24 0,24 C0,19 1,14 5,9 C9,4 15,2 21,2 L21,2 Z M24,1 L25,1 C29,1 34,3 39,7 C42,9 44,12 45,14 C46,17 47,21 47,24 L47,24 L47,24 L47,24 L46,24 L24,24 L24,24 L24,2 L24,1 Z M22,25 L23,26 L23,48 C23,48 22,49 22,49 C18,48 15,47 12,46 C4,42 0,35 0,25 L0,25 L22,25 Z M70,1 L70,2 L70,24 L70,24 L48,24 C48,24 47,24 47,24 C47,20 49,16 51,11 C56,5 62,1 70,1 L70,1 Z M72,1 L94,1 L95,2 L95,3 C95,6 93,10 91,14 C87,21 80,24 72,24 L71,24 L71,2 L72,1 Z M70,25 L70,26 L70,48 L70,49 L70,49 C66,48 62,47 59,46 C51,42 47,35 47,25 L48,25 L70,25 Z M96,1 L96,1 C101,1 106,3 111,7 C116,11 118,17 118,23 L118,24 L118,24 L96,24 L95,24 L95,2 L96,1 Z M95,25 L118,25 L118,26 L118,48 L118,49 L118,49 C113,48 110,47 107,46 C102,43 98,39 97,34 C96,31 95,28 95,26.5 L95,26.5 L95,26 L95,26 L95,25 Z M120,25 C125,26 129,27 132,29 C139,33 142,39 142,48 L142,49 L120,49 L119,48 L119,26 C119,25 120,25 120,25 L120,25 Z M166,1 L166,2 L166,24 L166,24 L143,24 C143,24 143,24 143,24 C143,19 145,15 148,10 C152,4 158,1 166,1 L166,1 Z M167,1 L169,1 C173,1 178,3 183,7 C188,11 190,17 190,24 L190,24 L167,24 L167,24 L167,2 L167,1 Z M166,25 L166,26 L166,48 L166,49 L166,49 C162,48 159,47 156,46 C147,42 143,35 143,25 L143,25 L166,25 Z M190,25 L190,26 L190,48 L190,49 L190,49 C186,48 183,47 180,46 C171,42 167,35 167,25 L167,25 L190,25 Z M215,0 L215,0 C221,0 226,3 231,7 L231,7 L215,24 L199,7 L199,7 C204,3 209,0 215,0 L215,0 Z M198,8.5 L214,24 L214,24 L198,40 L198,40 C193,35 191,30 191,25 L191,23 C191,18 193,13 198,8.5 L198,8.5 Z M215,25 L215,25 L231,41 L231,41 C226,46 220,49 214,49 L214,49 C209,49 203,46 199,41 L215,25 Z M232,2 L233,2 C237,2 242,3 247,7 C252,11 255,17 255,24 L232,24 L232,2 L232,2 Z M279,1 L279,2 L279,24 L279,24 L256.5,24 L256,24 L256,23 C256,18 258,13 262,8 C266,3 272,1 279,1 L279,1 Z M279,25 L279,26 L279,48 C279,48 279,49 278,49 C274,48 271,47 268,46 C260,42 256,35 256,25 L256,25 L279,25 Z M232,25 L254,25 L255,26 C255,29 254,33 252,37 C247,45 241,49 232,49 L232,48 L232,26 L232,25 Z' id='erlach' fill='#505050' sketch:type='MSShapeGroup'></path></g>
    </svg>">>.

logo3() ->
    <<"<svg width='100px' height='20px' viewBox='-2 0 284 50' version='1.1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' xmlns:sketch='http://www.bohemiancoding.com/sketch/ns'>
        <g id='Page-1' stroke='#26c6da' stroke-width='2' fill='black' fill-rule='evenodd' sketch:type='MSPage'>
            <path d='M21.65625,2.03125 L22.21875,2.03125 L22.921875,1.890625 L23.0625,2.03125 L23.0625,24.671875 L22.78125,24.953125 L0.28125,24.953125 C0.0937490625,24.953125 0,24.8125014 0,24.53125 C0,19.4687247 1.87498125,14.3594008 5.625,9.203125 C9.9843968,4.42185109 15.3280934,2.03125 21.65625,2.03125 L21.65625,2.03125 Z M24.328125,1.890625 L25.59375,1.890625 C29.9062716,1.890625 34.6405992,3.67185719 39.796875,7.234375 C42.6093891,9.92970098 44.3906213,12.2734275 45.140625,14.265625 C46.546882,17.9218933 47.25,21.2031105 47.25,24.109375 L47.109375,24.109375 L47.25,24.25 L47.25,24.671875 L46.96875,24.953125 L24.328125,24.953125 L24.046875,24.671875 L24.046875,2.171875 L24.328125,1.890625 Z M22.78125,25.796875 L23.0625,26.078125 L23.0625,48.71875 C23.0625,48.9062509 22.9218764,49 22.640625,49 C18.7031053,48.5312477 15.2812645,47.6875061 12.375,46.46875 C4.12495875,42.367167 0,35.5234854 0,25.9375 L0.140625,25.796875 L22.78125,25.796875 Z M70.8398438,1.890625 L70.9804688,2.03125 L70.9804688,24.671875 L70.6992188,24.953125 L48.1992188,24.953125 C48.0117178,24.953125 47.9179688,24.8125014 47.9179688,24.53125 C47.9179688,20.4765422 49.2304556,16.1172108 51.8554688,11.453125 C56.0508022,5.07809313 62.3788639,1.890625 70.8398438,1.890625 L70.8398438,1.890625 Z M72.2460938,1.75 L94.8867188,1.75 L95.1679688,2.03125 L95.1679688,3.15625 C95.1679688,6.25001547 93.9492309,10.1874761 91.5117188,14.96875 C87.2929477,21.6250333 80.8242623,24.953125 72.1054688,24.953125 L71.9648438,24.8125 L71.9648438,2.03125 L72.2460938,1.75 Z M70.6992188,25.796875 L70.9804688,26.078125 L70.9804688,48.71875 L70.6992188,49 L70.4179688,49 C66.2695105,48.4374972 62.7539207,47.5000066 59.8710938,46.1875 C51.9023039,42.1327922 47.9179688,35.3828597 47.9179688,25.9375 L48.0585938,25.796875 L70.6992188,25.796875 Z M96.1171875,1.890625 L96.2578125,1.890625 C101.367213,1.890625 106.476537,3.76560625 111.585938,7.515625 C116.367211,11.8984594 118.757812,17.242156 118.757812,23.546875 L118.757812,24.671875 L118.476562,24.953125 L96.1171875,24.953125 L95.8359375,24.671875 L95.8359375,2.171875 L96.1171875,1.890625 Z M95.9765625,25.796875 L118.617188,25.796875 L118.898438,26.078125 L118.898438,48.71875 L118.617188,49 L118.195312,49 C113.976541,48.4374972 110.367203,47.453132 107.367188,46.046875 C102.3281,43.4218619 98.9531336,39.6718994 97.2421875,34.796875 C96.2109323,31.7265471 95.6953125,28.9609498 95.6953125,26.5 L95.8359375,26.5 L95.6953125,26.359375 L95.6953125,26.078125 L95.9765625,25.796875 Z M120.304688,25.796875 C125.273462,26.406253 129.257798,27.5312418 132.257812,29.171875 C139.382848,33.4375213 142.945312,39.9999557 142.945312,48.859375 L142.804688,49 L120.164062,49 L119.882812,48.71875 L119.882812,26.078125 C119.882812,25.8906241 120.023436,25.796875 120.304688,25.796875 L120.304688,25.796875 Z M166.535156,1.890625 L166.675781,2.03125 L166.675781,24.671875 L166.394531,24.953125 L143.753906,24.953125 C143.566405,24.953125 143.472656,24.8125014 143.472656,24.53125 C143.472656,19.9140394 145.113265,15.0859627 148.394531,10.046875 C152.660178,4.60934781 158.706992,1.890625 166.535156,1.890625 L166.535156,1.890625 Z M167.800781,1.890625 L169.207031,1.890625 C173.425802,1.890625 178.066381,3.62498266 183.128906,7.09375 C188.191432,11.4765844 190.722656,17.3827754 190.722656,24.8125 L190.582031,24.953125 L167.800781,24.953125 L167.519531,24.671875 L167.519531,2.171875 L167.800781,1.890625 Z M166.253906,25.796875 L166.535156,26.078125 L166.535156,48.71875 L166.253906,49 L166.113281,49 C162.433575,48.5312477 159.152358,47.7343806 156.269531,46.609375 C147.738239,42.5781048 143.472656,35.6875488 143.472656,25.9375 L143.613281,25.796875 L166.253906,25.796875 Z M190.441406,25.796875 L190.722656,26.078125 L190.722656,48.71875 L190.441406,49 L190.019531,49 C186.1992,48.4374972 183.105481,47.6875047 180.738281,46.75 C171.925737,42.7656051 167.519531,35.8281745 167.519531,25.9375 L167.660156,25.796875 L190.441406,25.796875 Z M215.71875,0.625 L215.859375,0.625 C221.484403,0.625 226.687476,3.01560109 231.46875,7.796875 L231.46875,7.9375 L215.296875,24.109375 L199.125,7.9375 L199.125,7.796875 C204.375026,3.01560109 209.906221,0.625 215.71875,0.625 L215.71875,0.625 Z M198.421875,8.5 L214.59375,24.671875 L214.59375,24.8125 L198.421875,40.984375 L198.28125,40.984375 C193.593727,35.4296597 191.25,30.1328377 191.25,25.09375 L191.25,23.265625 C191.25,18.2734125 193.640601,13.3515868 198.421875,8.5 L198.421875,8.5 Z M215.296875,25.65625 L215.4375,25.65625 L231.609375,41.828125 L231.609375,41.96875 C226.05466,46.6562734 220.476591,49 214.875,49 L214.734375,49 C209.062472,49 203.859399,46.6093989 199.125,41.828125 L215.296875,25.65625 Z M232.453125,2.03125 L233.15625,2.03125 C237.820336,2.03125 242.695287,3.81248219 247.78125,7.375 C252.750025,11.6172087 255.234375,17.4765251 255.234375,24.953125 L232.171875,24.953125 L232.171875,2.3125 L232.453125,2.03125 Z M279.140625,1.890625 L279.28125,2.03125 L279.28125,24.671875 L279,24.953125 L256.5,24.953125 L256.21875,24.671875 L256.21875,23.828125 C256.21875,18.6484116 258.328104,13.4453386 262.546875,8.21875 C266.789084,3.99997891 272.320278,1.890625 279.140625,1.890625 L279.140625,1.890625 Z M279,25.796875 L279.28125,26.078125 L279.28125,48.71875 C279.28125,48.9062509 279.140626,49 278.859375,49 C274.429665,48.4609348 271.007825,47.6171932 268.59375,46.46875 C260.343709,42.4374798 256.21875,35.5937983 256.21875,25.9375 L256.359375,25.796875 L279,25.796875 Z M232.453125,25.9375 L254.953125,25.9375 L255.234375,26.21875 C255.234375,29.1953274 254.203135,33.0859135 252.140625,37.890625 C247.874979,45.296912 241.26567,49 232.3125,49 L232.171875,48.859375 L232.171875,26.21875 L232.453125,25.9375 Z' id='erlach' fill='#505050' sketch:type='MSShapeGroup'></path></g>
    </svg>">>.