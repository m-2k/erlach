-module(utils).
-author('Andy').

-compile(export_all).
-include("app_lib.hrl").


join([Head | Tail], Sep) -> join_acc(Tail, Sep, [Head]);
join([], _Sep) -> [].
join_acc([Head | Tail], Sep, Acc) -> join_acc(Tail, Sep, [Head, Sep | Acc]);
join_acc([], _Sep, Acc) -> lists:reverse(Acc).

map_join(Fun, Sep, [Head | Tail]) -> map_join_acc(Fun, Sep, Tail, [Fun(Head)]);
map_join(_Fun, _Sep, []) -> [].
map_join_acc(Fun, Sep, [Head | Tail], Acc) -> map_join_acc(Fun, Sep, Tail, [Fun(Head), Sep | Acc]);
map_join_acc(_Fun, _Sep, [], Acc) -> Acc.

strip(Bin) -> re:replace(Bin, <<"(^\\s+)|(\\s+$)">>, <<>>, [global,{return,binary}]).
squish(Bin) -> re:replace(Bin, <<"\\n{3,}">>, <<"\n\n">>, [global,{return,binary}]).

times(_,0) -> ok;
times(Fun,Count) -> Fun(), times(Fun,Count-1).
