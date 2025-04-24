-ifndef(EAUTH_USER_HRL).
-define(EAUTH_USER_HRL, "eauth.hrl").

-ifndef(USER_EXT).
-define(USER_EXT, email = [], role = member, salt = <<>>, deleted = false).
-endif.

-include_lib("kvs/include/user.hrl").

-endif.
