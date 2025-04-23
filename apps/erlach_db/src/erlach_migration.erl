-module(erlach_migration).
-author('andy').

-include_lib("erlach_db/include/erlach_db.hrl").

backup_table_list() -> [
        feed,
        id_seq,        
        board,
        thread,
        post,
        attachment
    ].
backup_file(Table) when is_atom(Table) -> "/tmp/backup-" ++ atom_to_list(Table) ++ ".eb1".

backup_ri(feed) ->          { record_info(fields, feed),        #feed{} };
backup_ri(id_seq) ->        { record_info(fields, id_seq),      #id_seq{} };
backup_ri(board) ->         { record_info(fields, board),       #board{} };
backup_ri(thread) ->        { record_info(fields, thread),      #thread{} };
backup_ri(post) ->          { record_info(fields, post),        #post{} };
backup_ri(attachment) ->    { record_info(fields, attachment),  #attachment{} }.


backup() ->
    lists:foldl(fun(E,Acc) ->
        R = backup(E),
        [{E,R}|Acc]
        end,[],backup_table_list()).

backup(Table) when is_atom(Table) ->
    {Fields, _Record} = backup_ri(Table),
    DataList = kvs:all(Table),
    Data={Fields,DataList},
    file:write_file(backup_file(Table), term_to_binary(Data)).

restore() ->
    lists:foldl(fun(E,Acc) ->
        R = restore(E),
        [{E,R}|Acc]
        end,[],backup_table_list()).

restore(Table) when is_atom(Table) ->
    {ok, Data} = file:read_file(backup_file(Table)),
    {Fields,DataList} = binary_to_term(Data),
    lists:foldl(fun(NewRecord,Acc) ->
        Merged = merge_record(Table,NewRecord,Fields),
        kvs:put(Merged),
        [Merged|Acc]
        end,[],DataList),
    ok.

merge_record(Table,NewRecord,Fields) ->
    {NewFields, VoidRecord} = backup_ri(Table),
    {_,Merged} = lists:foldl(fun(F,{Idx,Record}) ->
        case index_of(F,NewFields,1) of
            not_found -> {Idx+1,Record};
            VoidIdx -> {Idx+1,setelement(VoidIdx+1,Record,element(Idx,NewRecord))}
        end end,{2,VoidRecord},Fields),
    Merged.

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).