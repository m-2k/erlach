-define(ERLACH_VERSION, <<"2.2 beta">>).

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

-define(IDS_BASE, 36).

-record(route, {
  module :: atom(),
  board,
  type :: blog|thread,
  thread,
  category,
  option :: any()
  }).