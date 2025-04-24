-module(spa_utils).
-author('Andy').

-compile(export_all).
-include("spa.hrl").

-include_lib("kvs/include/config.hrl").
-include_lib("kvs/include/feed.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%                                                     %%%%%
%%%%%         CODE WARNING: ABSTRACT RECORDS MODE         %%%%%
%%%%%                                                     %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

% Without element sorting. 2000x SLOWER of usort! DONT USE
uniq([])    -> [];
uniq([H|T]) -> [H | [X || X <- uniq(T), X =/= H]].

while(Fun,State,Acc) ->
    case Fun(State,Acc) of
        {next,State2,Acc2} -> while(Fun,State2,Acc2);
        {stop,_State2,Acc2} -> Acc2
    end.

fold(Fun,Table,FeedId,Acc,Dose,Direction) ->
    case kvs:get(feed,FeedId) of
        {ok, #feed{top=FeedTop,count=FeedCount}} ->
            spa_utils:while(fun({Top,Viewed},Acc2) ->
                {Action,ReadCount}=case Viewed+Dose > FeedCount of true -> {stop,FeedCount-Viewed}; false -> {next,Dose} end,
                {Top2,Acc4}=kvs:fold(fun(E,{_,Acc3}) -> {element(Direction,E), Fun(E,Acc3)}
                    end,{Top,Acc2},Table,Top,ReadCount,Direction,#kvs{mod=?DBA}),
                {Action,{Top2,Viewed+ReadCount},Acc4}
            end,{FeedTop,0},Acc);
        _ -> Acc
    end.

fold2(___,Acc,_,Next,0,_,_) -> {ok,Next,0,Acc};
fold2(Fun,Acc,Table,Start,Count,Direction,Driver) ->
    case kvs:get(kvs:rname(Table), Start, Driver) of
        {ok, R} -> Next = element(Direction, R),
            Count2 = case Count of C when is_integer(C) -> C - 1; _-> Count end,
            case Fun(R,Acc) of
                {ok,Acc2} -> fold2(Fun,Acc2,Table,Next,Count2,Direction,Driver);
                {stop,Acc2} -> {ok,Next,Count2,Acc2}
            end;
        Error -> kvs:error(?MODULE,"Error: ~p~n",[Error]), Acc end.

%%% feed helper   
fold3(Fun,Table,Feed,Acc) ->
    case kvs:get(feed,Feed) of
        {ok, C} -> fold2(Fun,Acc,Table,element(#container.top,C),element(#container.count,C),#iterator.prev,#kvs{mod=?DBA});
        _ -> skip
    end.

%%% custom feeds helper
fold4(Fun,Table,Container,Feed,Acc) ->
    case kvs:get(Container,Feed) of
        {ok,C} -> fold2(Fun,Acc,Table,element(#container.top,C),element(#container.count,C),#iterator.prev,#kvs{mod=?DBA});
        _ -> skip
    end.
    
%%% when u want stopped fold, then just raise any error https://github.com/synrc/kvs/blob/3.4/src/kvs.erl#L216
fold5(Fun,Table,Container,Feed,Acc) ->
    case kvs:get(Container,Feed) of
        {ok,C} -> kvs:fold(Fun,Acc,Table,element(#container.top,C),element(#container.count,C),#iterator.prev,#kvs{mod=?DBA});
        _ -> Acc
    end.

times(_,0) -> ok;
times(Fun,Count) -> Fun(), times(Fun,Count-1).

prop_put({K,false},PL) -> lists:keydelete(K, 1, PL);
prop_put({K,V},PL) -> lists:keystore(K, 1, PL, {K,V}).
prop(K,PL,Def) -> case lists:keyfind(K, 1, PL) of {K,V} -> V; _ -> Def end.
is_prop(K,PL) -> case lists:keyfind(K, 1, PL) of {K,true} -> true; _ -> false end.

times(_,0,Acc) -> Acc;
times(Fun,Count,Acc) -> times(Fun,Count-1,[Fun()|Acc]).


%%% Hex 64

divisible_by(Num,Div) when Num rem Div =:= 0 -> 0;
divisible_by(Num,Div) -> (Div*((Num div Div)+1))-Num.

%% fully equal to digit(N) -> C.
-define(DIG, { $0, $1, $2, $3, $4, $5, $6, $7, $8, $9,
    $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
    $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N, $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z,
    $-, $. }).

char($-) -> 62;
char($+) -> 63;
char(H) when H >= $a andalso H =< $z -> H-$a+10;
char(H) when H >= $A andalso H =< $Z -> H-$A+36;
char(H) -> H-$0.

hex64(Bin) when is_binary(Bin) ->
    AddBits=divisible_by(bit_size(Bin),6),
    [ element(A+1,?DIG) || <<A:6>> <= <<0:AddBits, Bin/bitstring>> ].
unhex64(Hex) ->
    case << <<(char(H)):6>> || H <- Hex >> of
        Bin when is_binary(Bin) -> Bin;
        Bitstring ->
            DelBits=8-divisible_by(bit_size(Bitstring),8),
            <<_:DelBits,Shortened/binary>> = Bitstring,
            Shortened
    end.


hash(Term) -> hash(Term,4294967296).
hash(Term,Range) -> hex64(binary:encode_unsigned(erlang:phash2(Term,Range),little)).


option(Key,From) -> option(Key,From,false).
option(Key,List,Default) when is_list(List) -> proplists:get_value(Key,List,Default);
option(Key,Map,Default) when is_map(Map) -> maps:get(Key,Map,Default);
option(_,_,Default) -> Default.

setoption(Key,Value,To) when is_list(To) -> lists:keystore(Key, 1, To, {Key,Value});
setoption(Key,Value,To) when is_map(To) -> maps:put(Key, Value, To);
setoption(Key,Value,?UNDEF) -> [{Key,Value}].

ensure_split(N,List) when length(List) > N -> lists:split(N,List);
ensure_split(N,List) -> {List,[]}.

now_us({MegaSecs,Secs,MicroSecs}) -> (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.
now_js({MegaSecs,Secs,MicroSecs}) -> (MegaSecs*1000000 + Secs)*1000 + trunc(MicroSecs/1000).