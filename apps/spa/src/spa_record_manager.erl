-module(spa_record_manager).
-author('Andy').
-compile(export_all).
-include("spa.hrl").


name(R) -> get(1,R).
set_many(TupleList,E) -> lists:foldl(fun({Field,Value},Acc) -> set(Field,Acc,Value) end,E,TupleList).
get_many(FieldList,E) -> [ get(F,E) || F <- FieldList ].

%%% 12ms
call(Fun,Args) -> case wf:config(?SPA_CONFIG_NAME,record_manager,false) of
        false -> wf:error(?M,"Unknown interface module of record_manager, setup application variables",[]);
        Mod -> apply(Mod,Fun,Args)
    end.

new(Tag) ->                call(new,[Tag]).
new(Tag,InitTupleList) ->  set_many(InitTupleList,new(Tag)).

get(Number,Record) when is_integer(Number) -> element(Number,Record);
get(Field,Record)                          -> call(get,[Field,Record]).

set(Number,Record,Value) when is_integer(Number) -> setelement(Number,Record,Value);
set(Field,Record,Value)                          -> call(set,[Field,Record,Value]).

channel(R) ->              call(channel,[R]).
