-define(APP, erlach).
-define(ERLACH_VERSION, <<"R2">>).

-include_lib("n2o/include/wf.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("kvs/include/config.hrl").
-include_lib("app_lib/include/spa.hrl").
-include_lib("erlach_db/include/erlach_db.hrl").

-define(SITE_ROOT_LOCATION, "apps/erlach/priv").
-define(ATTACHMENT_LOCATION, "static/attachments").
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

-define(SITE_NAME, <<"Erlach">>).
-define(TITLE_DELIMETER, <<" – "/utf8>>).
-define(REQUESTS_BOARD, 2).

-define(ACTION_API, {Access, Board, Thread, {Level, Action, Data}}).

-define(CONFIG,erlach_config).
-define(SIMPLE_EVENT,simple_event).
-define(USER_ACTIVATION,activation).
-define(RPL,<<">>"/utf8>>).
-define(RPL_A,<<"↩"/utf8>>).


-record(query,{
    q1 :: ?UNDEF | binary(),
    q2 :: ?UNDEF | binary(),
    q3 :: ?UNDEF | binary(),
    history = false :: boolean()
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

% Main State
-record(st,{
    route    :: #route{},
    access,
    action   = view :: action(),
    level,
    board,
    thread,
    view = []
    }).

-record(rst, {
    % TODO: +elements (all)
    actions = [] :: list(),
    hidden_elements = [] :: list(),
    image
    }).

% Hierarhy Element Stack for Render
-record(hes,{
    option   :: any(), % rendering flags
    board,
    thread, % #post{type=thread}
    post,
    scope    :: any() % forwarding
    }).

%%% Global events
-record(auth, { user, passwd, logout }).
% Postback eventing for page switching
-record(postback,{action = view :: atom(), query :: #query{},
    history = true :: boolean(), route_option :: any() }).

%%% Render Events
-record(render_event, { mod, target, event }).
-record(pubsub,{ mod, target, action, element, data, from }).
-record(view,  { mod, target, element, level, option, queue = [], partial = false }).
-record(create,{ mod, target, panel, feed }).
-record(edit,  { mod, target, id }).
-record(delete,{ mod, target, id, value }).
-record(cancel,{ mod, target, panel, feed }).
-record(update,{ mod, target, id }).
-record(add,   { mod, target, panel, feed, forms }).
-record(put,   { mod, target, id, forms }).


-include_lib("nitro/include/nitro.hrl").
-record(hookup, {?ELEMENT_BASE(element_hookup),href,hreflang,media,rel,target,type,
    url="javascript:void(0);",download,name}).
-record(media_input, {?CTRL_BASE(element_media_input), image, target, disabled=false }).
    
-define(EVENT_ROUTER,erlach_event_router).
-define(SPA,erlach_spa).

-record(jpeg, {
    width,
    height,
    bit_depth,
    color_space
    }).
-record(png, {
    width,
    height,
    bit_depth,
    color_type
    }).
-record(gif, {
    width,
    height,
    animation,
    frames,
    alpha, % count of frames with alpha channel
    delays = [],
    loop_count = 0
    }).
-record(bpg, {
    width,
    height,
    pixel_format,
    alpha,
    bit_depth,
    color_space,
    extension_present,
    limited_range,
    animation
    }).