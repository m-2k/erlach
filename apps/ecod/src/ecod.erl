-module(ecod).
-author('Andy').
-behaviour(application).
-behaviour(supervisor).

-include("ecod.hrl").
-compile(export_all).

start(_,_) -> start_link().
stop(_) -> ok.
start_link() -> supervisor:start_link({local,?MODULE},?MODULE,[]).
init([]) -> {ok, {{one_for_one,5,10}, []} }.

utf8_to_list(Bin) -> unicode:characters_to_list(Bin,utf8).
list_to_utf8(List) -> unicode:characters_to_binary(List,utf8,utf8).

to_lower(Bin) when is_binary(Bin) -> list_to_utf8(ux_string:to_lower(utf8_to_list(Bin)));
to_lower(List) -> ux_string:to_lower(List).

to_words(Bin) when is_binary(Bin) -> to_words(utf8_to_list(Bin));
to_words(List) when is_list(List) ->
    lists:map(fun list_to_utf8/1,ux_string:extract_words(ux_string:to_lower(List))).

% Merging direction: PropList1 <-- PropList2
% ([{a,[1,2]},{b,[10]}],[{a,[2,3]},{b,[11]}]) -> [{a,[1,2,3]},{b,[10,11]}]
upropmerge(PropList1,PropList2) -> upropmerge(fun(L1,L2) -> lists:usort(L1++L2) end,PropList1,PropList2).
upropmerge(Fun,PropList1,PropList2) ->
    lists:foldl(fun({K,L},PropList) ->
        {L4,PropList4}=case lists:keytake(K, 1, PropList) of
            {value, {K,L3}, PropList3} -> {Fun(L3,L),PropList3};
            false -> {Fun([],L),PropList}
        end,
        [{K,L4}|PropList4]
    end,PropList1,PropList2).
% Group by count, direction: List1 <-- List2
upropgroup(CountList1,CountList2) ->
    lists:foldl(fun({E,N},Acc) ->
        case lists:keytake(E, 1, Acc) of
            {value, {E,N2}, Acc2} -> [{E,N+N2}|Acc2];
            false -> [{E,N}|Acc]
        end
    end,CountList1,CountList2).