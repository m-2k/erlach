-module(utils).
-compile(export_all).

-include_lib("db/include/board.hrl").
-include_lib("db/include/user.hrl").

to_digit(N) when N < 10 -> $0 + N;
to_digit(N) -> $a + N-10.

to_hex([]) -> [];
to_hex(Bin) when is_binary(Bin) -> to_hex(binary_to_list(Bin));
to_hex([H|T]) -> [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

random() -> list_to_binary(to_hex(crypto:rand_bytes(16))).
utc_random() -> utc_suffix(to_hex(crypto:rand_bytes(9))).

utc_suffix(Suffix) ->
    Now = {_, _, Micro} = erlang:now(), % uniqueness is used.
    Nowish = calendar:now_to_universal_time(Now),
    Nowsecs = calendar:datetime_to_gregorian_seconds(Nowish),
    Then = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Prefix = io_lib:format("~14.16.0b", [(Nowsecs - Then) * 1000000 + Micro]),
    list_to_binary(Prefix ++ Suffix).

% 64 bytes (node:8, date:14, random:42)
node_utc_random() -> <<(list_to_binary(to_hex(crypto:hash(md5, atom_to_list(node()))))):8/binary, (utc_suffix(to_hex(crypto:rand_bytes(21))))/binary>>.

init_db() ->
    add_new_board(e, <<"Erlach official">>, <<"Официальная зашкваренная борда эрлача"/utf8>>, 1, false),
    add_new_board(r, <<"Requests">>, <<"Борда реквестов"/utf8>>, 1, false),
    add_new_board(p, <<"Photo">>, <<"Фотография"/utf8>>, 1, false),
    add_new_board(g, <<"Girls">>, <<"Девушки"/utf8>>, 1, false),
    add_new_board(c, <<"Cinema">>, <<"Фильмы"/utf8>>, 1, false),
    add_new_board(ps, <<"Psychology">>, <<"Психология"/utf8>>, 1, false),
    add_new_board(po, <<"Poetry">>, <<"Стихи"/utf8>>, 1, false),
    add_new_board(pr, <<"Prank">>, <<"Пранк"/utf8>>, 1, false),
    add_new_board(ee, <<"Electrical engineering">>, <<"Электротехника"/utf8>>, 1, false).

set_access() ->
    utils:set_ba(1,[{anonymous,read,blog},{anonymous,read,post},
        {registered,read,blog},{registered,read,post},{registered,read,message},
        {private,read,blog}]),
    utils:set_ba(2,[{anonymous,read,blog},{anonymous,write,post},{anonymous,write,message},
        {registered,read,blog},{registered,write,post},{registered,write,message},
        {private,read,blog},{private,write,message},{private,write,request}]),
    utils:set_ba(6,[{private,read,blog},{private,write,message},{private,write,request}]).
    
clear_access() ->
    utils:set_ba(1,[]),
    utils:set_ba(2,[]),
    utils:set_ba(6,[]).

add_new_board(Uri, Name, Description, GroupID, Hidden) ->
    kvs:add( #board {
        id=kvs:next_id(board, 1),
        created=erlang:now(),
        uri=Uri,
        name=Name,
        user=0,
        description=Description,
        hidden=Hidden,
        feed_id={board, GroupID}
        }).

add_name(Uid,Name) ->
    kvs:add(#name{
    id=kvs:next_id(name,1),
    feed_id={name,Uid},
    displayname=Name,
    created=erlang:now()
    }).
    
set_ba(Id,Access) ->
    {ok, Board} = kvs:get(board,Id),
    kvs:put(Board#board{access=Access}).