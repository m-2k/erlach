-module(kvs_feeds).
-author('Andy').
-behaviour(application).
-behaviour(supervisor).

-include("kvs_feeds.hrl").
-include("kvs_feeds_api.hrl").
-compile(export_all).

start(_,_) -> start_link().
stop(_) -> ok.
start_link() -> supervisor:start_link({local,?MODULE},?MODULE,[]).
init([]) -> {ok, {{one_for_one,5,10}, []} }.

append(Record)         -> kvs_feeds_server:call(append,Record).
delete(Record)         -> kvs_feeds_server:call(delete,Record).
update(Record,Fun)     -> kvs_feeds_server:call(update,Record,Fun).
relink(Record)         -> kvs_feeds_server:call(relink,Record).
purge(Record,Fun)      -> kvs_feeds_server:call(purge,Record,Fun).
eval(Record,Fun)       -> kvs_feeds_server:call(eval,Record,Fun).
