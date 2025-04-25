-module(fulltext_search).
-behaviour(application).
-behaviour(supervisor).

-include("fulltext_search.hrl").
-compile(export_all).

-define(ENGINE,fulltext_search_engine).

start(_,_) -> start_link().
stop(_) -> ok.
start_link() -> supervisor:start_link({local,?MODULE},?MODULE,[]).
init([]) -> {ok, {{one_for_one,5,10}, []} }.

add_index(Table,Id,Text) -> ?ENGINE:add_index(Table,Id,Text).
add_index(Table,Id,Text,Feed) -> ?ENGINE:add_index(Table,Id,Text,Feed).

search(Query) -> ?ENGINE:search(Query).
search(Query,Feed) -> ?ENGINE:search(Query,Feed).