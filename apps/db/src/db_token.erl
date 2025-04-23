-module(db_token).
-include_lib("kvs/include/metainfo.hrl").
-include_lib("db/include/token.hrl").
-compile(export_all).

metainfo() ->
  #schema{name=kvs,tables=[
    #table{name=token,fields=record_info(fields,token),keys=[]}
    ]}.