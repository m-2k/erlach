-include_lib("kvs/include/kvs.hrl").
-include_lib("db.hrl").

-record(name, {?ITERATOR(feed),
  displayname,% not use
  created,    % timestamp
  banned,    % infinity | {expired, date()}
  deleted,
  role,     % = user :: administrator | moderator | user,
  birthday,
  sex,
  photo,    % attachment id
  future = []
  }).

-record(user3, {?DB_ELEMENT(feed),
  balance,
  expired,
  enabled,
  password,  % bcript
  salt,      % random
  tokens,    % [{n2o, <<"sid1">>},{n2o, <<"sid2">>},{twitter, <<"key">>}]
  last_post_date,
  email,
  phone,
  timezone,
  lang
  }).
