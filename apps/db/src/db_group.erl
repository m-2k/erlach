-module(db_group).
-include_lib("kvs/include/metainfo.hrl").
-include_lib("db/include/group.hrl").
-compile(export_all).

metainfo() ->
    #schema{name=kvs,tables=[
        #table{name=group,container=feed,fields=record_info(fields,group),keys=[]}
        ]}.

new(Name, Description, Access) ->
    kvs:add( #group{ id=kvs:next_id(group, 1),
        created=erlang:now(),
        name=Name,
        user=0,
        description=Description,
        temporary=false,
        access=Access
        }).