-define(APP, erlach).
-define(ERLACH_VERSION, <<"R3 RC5">>).
-define(ERLACH_VERSION_NUMBER, 2.991).

-include_lib("n2o/include/wf.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("kvs/include/config.hrl").
-include_lib("erlach_db/include/erlach_db.hrl").
-include_lib("spa/include/spa_basic.hrl").

-define(SITE_ROOT_LOCATION, "apps/erlach/priv").
-define(ATTACHMENT_LOCATION, "static/attachments").
-define(NEWEST_POSTS, newest_posts).

-define(T, temporary).
-define(AUTH, <<"auth">>).
-define(USER, <<"user">>).
-define(COOKIE_PATH, <<"/">>).
-define(SESSION_PARAMETERS_EXPIRATION_TIME, 60*60). % seconds
-define(URI_SERVICES, <<"services">>).


-define(UPLOAD_MIN_SIZE, 100).
-define(UPLOAD_MAX_SIZE, 10485760). % 10MB

-define(SITE_NAME, <<"Erlach">>).
-define(TITLE_DELIMETER, <<" – "/utf8>>).

-define(CONFIG,erlach_config).
-define(SIMPLE_EVENT,simple_event).
-define(USER_ACTIVATION,activation).
-define(RPL,<<">>"/utf8>>).
-define(RPL_A,<<"↩"/utf8>>).
    
-define(QUERY_EXT,
    q1 :: ?UNDEF | binary(),
    q2 :: ?UNDEF | binary(),
    q3 :: ?UNDEF | binary(),
    q4 :: ?UNDEF | binary(),
    history = false :: boolean()
    ).

-define(ROUTE_EXT,
    board :: ?UNDEF | binary(),
    thread :: ?UNDEF | binary(),
    post :: ?UNDEF | binary(),
    page_id % page id
    ).

-define(HES_EXT,
    party,
    board,
    thread,
    post
    ).

-include_lib("erlach_db/include/erlach_db.hrl").

-type action_mode() :: view | create | edit.
-type action() :: action_mode() | {action_mode(), any()}.

-define(STATE_EXT,
    access,
    board,
    thread,
    post,
    view = [],
    services = false :: false | comments
    ).

-record(rst, {
    % TODO: +elements (all)
    actions = [] :: list(),
    hidden_elements = [] :: list(),
    image_ftp,
    image_stack = #{}
    }).


%%% Global events
-record(auth, { ?RDR_EV(erlach_auth), user, passwd, logout }).
-define(RDR_PUBSUB_RENDER,erlach_subscription).


-include_lib("nitro/include/nitro.hrl").
-record(hookup, {?ELEMENT_BASE(element_hookup),href,hreflang,media,rel,target,type,
    url="javascript:void(0);",download,name}).
-record(media_input, {?CTRL_BASE(element_media_input), image, target, disabled=false }).
    
-define(EVENT_ROUTER,spa_proxy).
-define(SPA,erlach_spa).
-define(SUB,erlach_subscription).
-define(SERVICES,erlach_services).
-define(STREAM,erlach_stream).

-include_lib("spa/include/spa.hrl").
