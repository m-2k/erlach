-ifndef(EAUTH_HRL).
-define(EAUTH_HRL, "eauth.hrl").

-define(M,?MODULE).
-define(UNDEF,undefined).

% default config
-define(WRONG_DELAY,1000).
-define(SALT_LOCAL,<<2,4,8,16>>).
-define(HASH_ALGORITHM,sha512).
-define(SALT_LENGTH,4).
-define(PASSWORD_LENGTH,16).
-define(PASSWORD_SYMBOLS,<<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-+!?@#$%^&*()[]{}:;<>">>).
-define(SMTP_TLS,always).
-define(CACHE_ACTIVATION_TAG,eauth_activation).
-define(ACTIVATION_TIME,86400). % 1d
-define(ACTIVATION_KEY_LENGTH,16).
-define(EMAIL_REGEXP,<<"^[a-z0-9._%+-]{1,24}@[a-z0-9-]{2,16}\\.[a-z]{2,4}$">>).

-endif.
