-module(fulltext_search_engine).
-author('Andy').

-include("fulltext_search.hrl").
-include("fulltext_search_db.hrl").
-compile(export_all).

-define(NEXT,kvs:next_id(fulltext_index,1)).
-define(UFOLD(K,V,PropList),ecod:upropmerge(fun ecod:upropgroup/2,PropList,[{K,[{V,1}]}])).

add_index(Table,Id,Text) -> add_index(Table,Id,Text,?UNDEF).
add_index(Table,Id,Text,Feed) ->
    lists:foreach(fun(Word) ->
        {S1,Word2}=case ecod:utf8_to_list(Word) of
            [SX1] -> {[],[SX1]};
            [SX1|WX] -> {[SX1],WX}
        end,
        lists:foldl(fun(S,Acc) ->
            Acc2=Acc++[S],
            Index=case Feed of
                ?UNDEF -> ecod:list_to_utf8(Acc2);
                _ -> {Feed,ecod:list_to_utf8(Acc2)}
            end,
            case kvs:index(fulltext_index,index,Index) of
                [#fulltext_index{data=DataList}=FI] ->
                    Update=fun(#fulltext_index{}=FI2) -> {ok,FI2#fulltext_index{data=?UFOLD(Table,Id,DataList)}} end,
                    kvs_feeds:update(FI,Update);
                [] ->
                    FI=#fulltext_index{id=?NEXT,index=Index,data=?UFOLD(Table,Id,[])},
                    kvs_feeds:append(FI)
            end,
            Acc2
        end,S1,Word2)
    end,ecod:to_words(Text)).

search(Text) -> search(Text,?UNDEF).
search(Text,Feed) ->
    Start=erlang:timestamp(),
    R=lists:foldl(fun(Word,Acc) ->
        Get=case Feed of ?UNDEF -> Word; _ -> {Feed,Word} end,
        case kvs:index(fulltext_index,index,Get) of
            [#fulltext_index{data=DataList}] -> ecod:upropmerge(fun ecod:upropgroup/2,Acc,DataList);
            _ -> Acc
        end end,[],ecod:to_words(Text)),
    Stop=erlang:timestamp(),
    wf:info(?MODULE,"Execution time (~p:~p):",[Start,Stop]),
    R.
