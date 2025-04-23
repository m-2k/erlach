-module(db_user).
-include_lib("kvs/include/metainfo.hrl").
-include_lib("db/include/user.hrl").
-compile(export_all).

metainfo() ->
  #schema{name=kvs,tables=[
    #table{name=user3,container=feed,fields=record_info(fields,user3),keys=[]},
    #table{name=name,container=feed,fields=record_info(fields,name),keys=[]}
    ]}.   