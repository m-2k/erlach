-module(db_attachment).
-include_lib("kvs/include/metainfo.hrl").
-include_lib("db/include/attachment.hrl").
-compile(export_all).

metainfo() ->
  #schema{name=kvs,tables=[
    #table{name=attachment,container=feed,fields=record_info(fields,attachment),keys=[]}
    ]}.