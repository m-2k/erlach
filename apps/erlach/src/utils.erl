-module(utils).
-compile(export_all).

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
node_utc_random() -> <<(list_to_binary(unique:to_hex(crypto:hash(md5, atom_to_list(node()))))):8/binary, (utc_suffix(unique:to_hex(crypto:rand_bytes(21))))/binary>>.
