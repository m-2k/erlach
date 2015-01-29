-define(ERLACH_VERSION, <<"0.2 beta">>).

-define(BOARD_ID, board_id).
-define(THREAD_ID, thread_id).
-define(USER_ID, anonymous).
-define(SITE_ROOT_LOCATION, "apps/review/priv").
-define(ATTACHMENT_LOCATION, "static/attachments").
-define(STORED_POST_ID, stored_post_id).
-define(STORED_REQUEST_TO, stored_request_to).
-define(MARKDOWN_ENABLED, markdown_enabled).
-define(NEWEST_POSTS, newest_posts).

-record(cookie, {status :: new | actual | atom(), path, ttl, issued, expire}).
-record(state, {unique, node, user}).

-define(T, temporary).
-define(AUTH, <<"auth">>).
-define(USER, <<"user">>).
-define(COOKIE_PATH, <<"/">>).
-define(SESSION_REDIRECT_TIME_LIMIT, 60). % seconds

% {mime, ext}
-define(MIME_IMAGE_JPEG, {<<"image/jpeg">>, <<"jpg">>}).
-define(MIME_IMAGE_GIF, {<<"image/gif">>, <<"gif">>}).
-define(MIME_IMAGE_PNG, {<<"image/png">>, <<"png">>}).
-define(MIME_IMAGE_BPG, {<<"image/bpg">>, <<"bpg">>}).

