-define(APP, erlach).
-define(ERLACH_VERSION, <<"R1">>).

% -define(BOARD_ID, board_id).
% -define(THREAD_ID, thread_id).
% -define(USER_ID, anonymous).
-define(SITE_ROOT_LOCATION, "apps/erlach/priv").
-define(ATTACHMENT_LOCATION, "static/attachments").
% -define(STORED_POST_ID, stored_post_id).
% -define(STORED_REQUEST_TO, stored_request_to).
% -define(MARKDOWN_ENABLED, markdown_enabled).
-define(NEWEST_POSTS, newest_posts).

-record(cookie, {status :: new | actual | atom(), path, ttl, issued, expire}).
-record(state, {unique, node, user}).

-define(T, temporary).
-define(AUTH, <<"auth">>).
-define(USER, <<"user">>).
-define(COOKIE_PATH, <<"/">>).
-define(SESSION_PARAMETERS_EXPIRATION_TIME, 60*60). % seconds

% {mime, ext}
-define(MIME_IMAGE_JPEG, {<<"image/jpeg">>, <<"jpg">>}).
-define(MIME_IMAGE_GIF,  {<<"image/gif">>,  <<"gif">>}).
-define(MIME_IMAGE_PNG,  {<<"image/png">>,  <<"png">>}).
-define(MIME_IMAGE_BPG,  {<<"image/bpg">>,  <<"bpg">>}).

-define(UPLOAD_MIN_SIZE, 100).
-define(UPLOAD_MAX_SIZE, 10485760). % 10MB
% -define(UPLOAD_MAX_SIZE, 102400). % 100KB

-define(SITE_NAME, <<"Erlach">>).
-define(TITLE_DELIMETER, <<" – "/utf8>>).
-define(REQUESTS_BOARD, 2).

-define(ACTION_API, {Access, Board, Thread, {Level, Action, Data}}).

-define(M,?MODULE).
-define(UNDEF,undefined).
-define(SIMPLE_EVENT,simple_event).
-define(USER_ACTIVATION,activation).


-record(query,{
    q1 :: ?UNDEF | binary(),
    q2 :: ?UNDEF | binary()
    }).

-record(route, {
    query :: #query{},
    module :: atom(),
    render :: atom(),
    level :: binary(),
    board :: ?UNDEF | binary(),
    thread :: ?UNDEF | binary(),
    option :: any(),
    page_id, % page id
    type :: blog|thread, % TODO: remove
    category % TODO: remove
    }).
    
    
-include_lib("erlach_db/include/erlach_db.hrl").


-type action_mode() :: view | create | edit.
-type action() :: action_mode() | {action_mode(), any()}.

% % State actions for all types of UI objects
% -record(act,{
%     board = view :: action(),
%     thread = view :: action(),
%     }).

% Main State
-record(st,{
    route    :: #route{},
    % access, % TODO:
    % allow_js = false :: boolean(),
    action   = view :: action(),
    % user     :: ?UNDEF | #user3{}, % current logged-in user
    level,
    % level    :: ?UNDEF | atom(),
    board,
    thread
    }).

-record(rst, {
    % TODO: +elements (all)
    actions = [] :: list(),
    hidden_elements = [] :: list(),
    image
    }).

% % Mutable Render State: rendering ONLY state (can be mutable after any fun calls)
% % Must be clear with VIEW event calling
% -record(rst, {
%     homework = ?UNDEF :: ?UNDEF | #homework{},
%     users = [] :: [#rst_user{}],
%     elements = [] :: list()
%     }).

% Hierarhy Element Stack for Render
-record(hes,{
    option   :: any(), % rendering flags
    board,
    thread,
    scope    :: any() % forwarding
    }).

% Postback eventing for page switching
-record(postback,{action = view :: atom(), query :: #query{},
    history = true :: boolean(), route_option :: any() }).

-record(render_event, { guard, target, event }).
-record(pubsub,{ target, action, data, from }).
-record(view,  { target, element, level }).
-record(create,{ target, panel, feed }).
-record(edit,  { target, id }).
-record(delete,{ target, id, value }).
-record(cancel,{ target, panel, feed }).
-record(update,{ target, id }).
-record(add,   { target, panel, feed, forms }).
-record(put,   { target, id, forms }).
-record(subscribe,   { target, uid }).
-record(unsubscribe, { target, uid }).
-record(deferred_insert,{target, elements = [] :: list(),
    delay = 0 :: integer(), is_return_elements = false :: bool, state :: #st{} }).


-include_lib("nitro/include/nitro.hrl").
-record(hookup, {?ELEMENT_BASE(element_hookup),href,hreflang,media,rel,target,type,
    url="javascript:void(0);",download,name}).
-record(media_input, {?CTRL_BASE(element_media_input), image, type }).
    
-define(EVENT_ROUTER,erlach_event_router).
-define(SPA,erlach_spa).

-record(bpg_info, {
    pixel_format,
    alpha,
    bit_depth,
    color_space,
    extension_present,
    limited_range,
    animation,
    picture_width,
    picture_height
    }).