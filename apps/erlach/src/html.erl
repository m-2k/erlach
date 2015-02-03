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
            body=[Message]}),
    wf:wire(wf:f("window.setTimeout(function(){var a=qi('~s'); if(a){a.onclick();}},~b);",[Id,size(wf:to_binary(Message))*100+4000])).
    % wf:wire(wf:f("var m=qi('~s');m.style.webkitAnimationName=m.style.animationName='popup-slide-left';", [Id])).


title(_Module, ?ACTION_API) ->
    Text = case Action of view -> Thread#thread.topic; _ -> [<<"New thread in ">>, Board#board.name] end,
    guard:to_binary([Text, ?TITLE_DELIMETER, ?SITE_NAME]).

header() -> header(u:get()).
header(User) -> [#panel{id= <<"header">>,body=[#link{href="/",class= <<"erlach-logo">>,body=logo()},html:menu(User)]}].

menu() -> menu(u:get()).
menu(User) ->
    Class=?HEADER_LINKS,
    [ #panel{class= <<"links right">>,body=[
            #link {class=Class, href= <<"/profile">>, body= <<"Profile">>},
            case u:is_temp(User) of
                true -> #link {class=Class, body= <<"Sign in">>, postback=signin};
                false -> #link {class=Class, body= <<"Logout">>, postback=logout}
            end ]},
        #panel{class= <<"links left">>,body=[
            #link {class=Class, href= <<"#">>, body= <<"News">>},
            #link {class=Class, href= <<"#">>, body= <<"Blog">>} ]} ].
    
footer() ->
    Class=?FOOTER_LINKS,
    #panel{id= <<"footer">>,body = [
        #panel{class= <<"related-links">>,body=[
            #link{class=Class, body= <<"Erlang Powered">>, href= <<"http://erlang.org">>},
            #link{class=Class, body= <<"SynRC">>, href= <<"https://synrc.com">>},
            #link{class=Class, body= <<"EoX">>, href= <<"http://erlangonxen.org">>},
            #link{class=Class, body= <<"Tails">>, href= <<"https://tails.boum.org">>},
            #link{class=Class, body= <<"Tor">>, href= <<"https://www.torproject.org">>},
            #link{class=Class, body= <<"Digital Ocean">>, href= <<"https://digitalocean.com">>},
            #link{class=Class, body= <<"N2O">>, href= <<"https://github.com/5HT/n2o">>},
            #link{class=Class, body= <<"Cowboy">>, href= <<"https://github.com/ninenines/cowboy">>},
            #link{class=Class, body= <<"NoSQL">>, href= <<"http://www.erlang.org/doc/apps/mnesia/">>},
            #link{class=Class, body= <<"BPG Ready">>, href= <<"http://bellard.org/bpg/">>},
            #link{class=Class, body= <<"Flex CSS">>, href= <<"http://pepelsbey.net/pres/flexbox-gotcha/?full#Cover">>},
            #link{class=Class, body= <<"Erlach IBS, 2015"/utf8>>, href= <<"/privacy">>}
        ]}
    ]}.

body(Content) -> [ html:header(u:get()), #panel{ id= <<"content">>, body=Content}, html:footer(),#panel{id= <<"popup-messages">>}].


thread_body({Access, Board, Thread, {thread, Action, Data}}=S) ->
    
    AllowMarkdown = access:is_allow(blog,write,Access),
    AllowWrite = access:is_allow(post,write,Access),
    wf:info(?MODULE, "State: ~p",[{AllowMarkdown,AllowWrite}]),
    
    Content = case Action of
        view ->
            {Topic, ThType, ReqTo} = {Thread#thread.topic,Thread#thread.type,Thread#thread.request_to},
            {ok,Head,Posts}=posts_list(Thread),
            [ #panel{class= <<"content-title">>,body=Topic},
                case ThType of
                    request when ReqTo =/= undefined ->
                        #span{class= <<"remark">>,body= <<"This thread is request for ", (wf:to_binary(wf:to_list(ReqTo)))/binary>>};
                    _ -> []
                end,    
                #panel{id=posts,body=[Head,Posts]},
                if AllowWrite ->
                    #panel{id= <<"drag-input-form">>,class= <<"drag-accepted">>,body=[
                        #panel{id= <<"text-input-form">>,body=[
                            #textarea{id= <<"message">>,class= <<"textarea">>,placeholder= <<"Message">>},
                            #link{id= <<"store">>,class= <<"button primary">>,body= <<"Send message">>,
                                postback={store, finalize},source=[message]},
                            #panel{id= <<"thumbnail-list">>}]}]};
                    true -> [] end ];
        create ->
            [ #panel{id= <<"drag-input-form">>,class= <<"drag-accepted">>,body=[
                #panel{id= <<"text-input-form">>,body=[
                    #textbox{id= <<"topic">>, class = <<"textarea topic">>, placeholder = <<"Topic">> },
                    #textarea{id= <<"message">>, class = <<"textarea">>, placeholder = <<"Message">> },
                    #link{id= <<"store">>,class= <<"button success">>,
                        body = <<"New thread">>,
                        postback={store, finalize}, source=[topic, message]},
                    if AllowMarkdown -> #link{id=markdown,class= <<"button info">>,body= <<"Enable Markdown">>,postback=enable_markdown};
                        true -> [] end,
                    #panel{id= <<"thumbnail-list">>}]}]}]
    end,
    #panel{id=imageboard,body=[Content,image_viewer()]}.


posts_list(#thread{id=Tid,type=Type,request_to=ReqTo}=Thread) ->
    Uid = u:id(),
    IsAdmin = u:is_admin(),
    case kvs:get(feed, {thread, Tid}) of
        {ok, F} ->
            AllPosts = kvs:traversal(post, F#feed.top, F#feed.entries_count, #iterator.prev),
            % wf:wire(#transfer{state=[{items,AllPosts}]}),
            {Head, Posts} = lists:foldl(fun(P, {H,Acc}) ->
                case {H,P#post.head,post(Thread,P,IsAdmin,Uid)} of
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

post_check_visibility(Uid,IsAdmin,#thread{type=ThType,request_to=ReqTo},#post{deleted=Deleted,head=IsHead,temporary=IsTemp,user=User}) ->
    case {IsAdmin,IsHead,IsTemp,Deleted,ThType,User} of
        {_,_,true,_,_,_} -> skip;
        {true,_,_,_,_,_} -> ok;
        {_,_,_,true,_,_} -> skip;
        {_,true,_,_,_,_} -> ok;
        {_,_,_,_,request,Uid} when ReqTo =/= undefined -> ok;
        {_,_,_,_,request,_} -> skip;
        _ -> ok
    end.

post(#post{feed_id={thread,Tid}}=P) ->
    {ok, Thread} = kvs:get(thread,Tid),
    post(Thread,P, u:is_admin(),u:id()).
post(#thread{id=Tid,type=ThreadType,request_to=ReqTo}=Thread, #post{id=Id,type=PostType,feed_id ={thread,Tid},temporary=IsTemp,message=Message, created=Timestamp,user=User,deleted=Deleted,head=IsHead}=Post, IsAdmin,Uid) ->

    wf:info(?MODULE, " >>> html_post Message: ~p", [Message]),
    % {{_Y,_M,_D},{Hour,Minute,Second}} = calendar:now_to_local_time(Timestamp),
    Text = case PostType of
        blog -> markdown:conv_utf8(Message);
        message -> guard:html_escape(Message);
        _ -> guard:html_escape(Message)
    end,
    wf:info(?MODULE, " >>> html_post Converted message: ~p", [Text]),
    
    case {post_check_visibility(Uid,IsAdmin,Thread,Post),Deleted} of
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
                    #panel{ class = <<"post-attachment">>, body = post_attachment(Post) }
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
                            #panel{ class = <<"post-attachment">>, body = post_attachment(Post) }
                        %    ]}
                        ]},
                    {ok, Html};
                _ -> skip end;
        {Skip,_} -> Skip
    end.
        
post_attachment(#post{ id=Id } = _P) ->
    case kvs:get(feed, {post, Id}) of
        {ok, F} ->
            Attachments = kvs:traversal(attachment, F#feed.top, F#feed.entries_count, #iterator.prev),
            lists:map(fun(A) ->
                #image{ image = A#attachment.path, class = <<"image">> }
                end, lists:reverse(Attachments));
        NotFound ->
            wf:info(?MODULE, "Post ~p doesn't has attachments (feed: ~p reason: ~p)", [Id, {post, Id}, NotFound]),
            []
    end.




board_body(#board{id=Bid,name=Name,description=Description}=Board, Content) ->
    Head = [ #panel{class= <<"content-title">>,body=Name}, #span{class= <<"remark">>,body=Description} ],
    Manage = #panel{class= <<"center">>,body=[
        #link{class = <<"button primary slim">>, body = <<"New">>, postback={thread, create, {post, Bid}} },
        case u:is_admin() of
            true ->
                case Board#board.request_thread of
                    undefined -> #link{class= <<"button primary slim">>, body= <<"New request">>,postback={thread, create, {request, {board, Bid}}}};
                    Rtid -> #link{class= <<"button success slim">>, body= <<"View request">>, href=wf:f("/thread?id=~b",[Rtid])}
                end;
            _ -> [] end ]},
    Main = #panel{id= <<"threads">>, class= <<"line">>, body=Content},
    Settings = case u:is_admin() of true -> settings_panel(board, Board); _ -> [] end,
    html:body([Head,Manage,Main,Settings,image_viewer()]).
    
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

username_add() ->
    #panel{id= <<"username-manage">>, class = <<"contact-modify-box">>, body = [
        #button{class= <<"success">>, body= <<"Write">>, postback={username, add}, source=[]} ]}.

username_edit() ->
    #panel{id= <<"username-manage">>, class = <<"contact-modify-box">>, body = [
    ]}.
    
    
logo() ->
    <<"<svg width='100px' height='20px' viewBox='-2 0 284 50' version='1.1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' xmlns:sketch='http://www.bohemiancoding.com/sketch/ns'>
        <g id='Page-1' stroke='#26c6da' stroke-width='2' fill='black' fill-rule='evenodd' sketch:type='MSPage'>
            <path d='M21.65625,2.03125 L22.21875,2.03125 L22.921875,1.890625 L23.0625,2.03125 L23.0625,24.671875 L22.78125,24.953125 L0.28125,24.953125 C0.0937490625,24.953125 0,24.8125014 0,24.53125 C0,19.4687247 1.87498125,14.3594008 5.625,9.203125 C9.9843968,4.42185109 15.3280934,2.03125 21.65625,2.03125 L21.65625,2.03125 Z M24.328125,1.890625 L25.59375,1.890625 C29.9062716,1.890625 34.6405992,3.67185719 39.796875,7.234375 C42.6093891,9.92970098 44.3906213,12.2734275 45.140625,14.265625 C46.546882,17.9218933 47.25,21.2031105 47.25,24.109375 L47.109375,24.109375 L47.25,24.25 L47.25,24.671875 L46.96875,24.953125 L24.328125,24.953125 L24.046875,24.671875 L24.046875,2.171875 L24.328125,1.890625 Z M22.78125,25.796875 L23.0625,26.078125 L23.0625,48.71875 C23.0625,48.9062509 22.9218764,49 22.640625,49 C18.7031053,48.5312477 15.2812645,47.6875061 12.375,46.46875 C4.12495875,42.367167 0,35.5234854 0,25.9375 L0.140625,25.796875 L22.78125,25.796875 Z M70.8398438,1.890625 L70.9804688,2.03125 L70.9804688,24.671875 L70.6992188,24.953125 L48.1992188,24.953125 C48.0117178,24.953125 47.9179688,24.8125014 47.9179688,24.53125 C47.9179688,20.4765422 49.2304556,16.1172108 51.8554688,11.453125 C56.0508022,5.07809313 62.3788639,1.890625 70.8398438,1.890625 L70.8398438,1.890625 Z M72.2460938,1.75 L94.8867188,1.75 L95.1679688,2.03125 L95.1679688,3.15625 C95.1679688,6.25001547 93.9492309,10.1874761 91.5117188,14.96875 C87.2929477,21.6250333 80.8242623,24.953125 72.1054688,24.953125 L71.9648438,24.8125 L71.9648438,2.03125 L72.2460938,1.75 Z M70.6992188,25.796875 L70.9804688,26.078125 L70.9804688,48.71875 L70.6992188,49 L70.4179688,49 C66.2695105,48.4374972 62.7539207,47.5000066 59.8710938,46.1875 C51.9023039,42.1327922 47.9179688,35.3828597 47.9179688,25.9375 L48.0585938,25.796875 L70.6992188,25.796875 Z M96.1171875,1.890625 L96.2578125,1.890625 C101.367213,1.890625 106.476537,3.76560625 111.585938,7.515625 C116.367211,11.8984594 118.757812,17.242156 118.757812,23.546875 L118.757812,24.671875 L118.476562,24.953125 L96.1171875,24.953125 L95.8359375,24.671875 L95.8359375,2.171875 L96.1171875,1.890625 Z M95.9765625,25.796875 L118.617188,25.796875 L118.898438,26.078125 L118.898438,48.71875 L118.617188,49 L118.195312,49 C113.976541,48.4374972 110.367203,47.453132 107.367188,46.046875 C102.3281,43.4218619 98.9531336,39.6718994 97.2421875,34.796875 C96.2109323,31.7265471 95.6953125,28.9609498 95.6953125,26.5 L95.8359375,26.5 L95.6953125,26.359375 L95.6953125,26.078125 L95.9765625,25.796875 Z M120.304688,25.796875 C125.273462,26.406253 129.257798,27.5312418 132.257812,29.171875 C139.382848,33.4375213 142.945312,39.9999557 142.945312,48.859375 L142.804688,49 L120.164062,49 L119.882812,48.71875 L119.882812,26.078125 C119.882812,25.8906241 120.023436,25.796875 120.304688,25.796875 L120.304688,25.796875 Z M166.535156,1.890625 L166.675781,2.03125 L166.675781,24.671875 L166.394531,24.953125 L143.753906,24.953125 C143.566405,24.953125 143.472656,24.8125014 143.472656,24.53125 C143.472656,19.9140394 145.113265,15.0859627 148.394531,10.046875 C152.660178,4.60934781 158.706992,1.890625 166.535156,1.890625 L166.535156,1.890625 Z M167.800781,1.890625 L169.207031,1.890625 C173.425802,1.890625 178.066381,3.62498266 183.128906,7.09375 C188.191432,11.4765844 190.722656,17.3827754 190.722656,24.8125 L190.582031,24.953125 L167.800781,24.953125 L167.519531,24.671875 L167.519531,2.171875 L167.800781,1.890625 Z M166.253906,25.796875 L166.535156,26.078125 L166.535156,48.71875 L166.253906,49 L166.113281,49 C162.433575,48.5312477 159.152358,47.7343806 156.269531,46.609375 C147.738239,42.5781048 143.472656,35.6875488 143.472656,25.9375 L143.613281,25.796875 L166.253906,25.796875 Z M190.441406,25.796875 L190.722656,26.078125 L190.722656,48.71875 L190.441406,49 L190.019531,49 C186.1992,48.4374972 183.105481,47.6875047 180.738281,46.75 C171.925737,42.7656051 167.519531,35.8281745 167.519531,25.9375 L167.660156,25.796875 L190.441406,25.796875 Z M215.71875,0.625 L215.859375,0.625 C221.484403,0.625 226.687476,3.01560109 231.46875,7.796875 L231.46875,7.9375 L215.296875,24.109375 L199.125,7.9375 L199.125,7.796875 C204.375026,3.01560109 209.906221,0.625 215.71875,0.625 L215.71875,0.625 Z M198.421875,8.5 L214.59375,24.671875 L214.59375,24.8125 L198.421875,40.984375 L198.28125,40.984375 C193.593727,35.4296597 191.25,30.1328377 191.25,25.09375 L191.25,23.265625 C191.25,18.2734125 193.640601,13.3515868 198.421875,8.5 L198.421875,8.5 Z M215.296875,25.65625 L215.4375,25.65625 L231.609375,41.828125 L231.609375,41.96875 C226.05466,46.6562734 220.476591,49 214.875,49 L214.734375,49 C209.062472,49 203.859399,46.6093989 199.125,41.828125 L215.296875,25.65625 Z M232.453125,2.03125 L233.15625,2.03125 C237.820336,2.03125 242.695287,3.81248219 247.78125,7.375 C252.750025,11.6172087 255.234375,17.4765251 255.234375,24.953125 L232.171875,24.953125 L232.171875,2.3125 L232.453125,2.03125 Z M279.140625,1.890625 L279.28125,2.03125 L279.28125,24.671875 L279,24.953125 L256.5,24.953125 L256.21875,24.671875 L256.21875,23.828125 C256.21875,18.6484116 258.328104,13.4453386 262.546875,8.21875 C266.789084,3.99997891 272.320278,1.890625 279.140625,1.890625 L279.140625,1.890625 Z M279,25.796875 L279.28125,26.078125 L279.28125,48.71875 C279.28125,48.9062509 279.140626,49 278.859375,49 C274.429665,48.4609348 271.007825,47.6171932 268.59375,46.46875 C260.343709,42.4374798 256.21875,35.5937983 256.21875,25.9375 L256.359375,25.796875 L279,25.796875 Z M232.453125,25.9375 L254.953125,25.9375 L255.234375,26.21875 C255.234375,29.1953274 254.203135,33.0859135 252.140625,37.890625 C247.874979,45.296912 241.26567,49 232.3125,49 L232.171875,48.859375 L232.171875,26.21875 L232.453125,25.9375 Z' id='erlach' fill='#505050' sketch:type='MSShapeGroup'></path></g>
    </svg>">>.