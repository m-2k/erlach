-ifndef(DB_HRL).
-define(DB_HRL, "db.hrl").

-include_lib("kvs/include/kvs.hrl").

-define(DB_ELEMENT(Container),
  ?ITERATOR(Container),
  created,        % timestamp
  temporary = true :: false|true,    % machine logic for pre-storing
  published,      % timestamp
  type,           % thread::default|blog|request, post::default|question
  access = [] :: list(tuple()),
  request_thread, % thread id
  hidden,         % undefined|any(), user defined
  deleted,        % undefined|any(), modefator defined
  draft,
  user,
  user_name,
  name,
  view = [] :: list(),
  count,          % count of entry elements
  links = [] :: list(),
  category = [] :: list(), % uses as "allowed categories" for boards & "selected categories" for threads
  tags = [] :: list(),     % tag names
  future = []).   % extends fields for future

-record(db_element,  {?DB_ELEMENT(undefined)}).

-endif.
