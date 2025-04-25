-module(fulltext_search_db).
-author('Andy').
-compile(export_all).

-include("fulltext_search_db.hrl").
-include_lib("kvs/include/metainfo.hrl").

metainfo() -> 
    #schema{name=kvs,tables=[
        #table{name=fulltext_index,fields=record_info(fields,fulltext_index),keys=[index]}
    ]}.

purge_mnesia() ->
    mnesia:clear_table(fulltext_index),
    mnesia:dirty_delete(id_seq,"fulltext_index"),
    mnesia:dirty_delete(feed,fulltext_index).