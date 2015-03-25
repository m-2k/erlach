-module(guard).
-compile(export_all).

-ifndef(SESSION).
-define(SESSION, (wf:config(n2o,session,erlach_session))).
-endif.

to_integer(I) when is_integer(I) -> I;
to_integer(B) when is_binary(B) -> to_integer(binary_to_list(B));
to_integer(L) when is_list(L) -> case string:to_integer(L) of {I,[]} -> I; _ -> undefined end;
to_integer(_) -> undefined.

to_integer(B,Base) when is_binary(B) andalso is_integer(Base) -> to_integer(binary_to_list(B),Base);
to_integer(L,Base) when is_list(L) andalso is_integer(Base) -> try erlang:list_to_integer(L,Base) catch _:_ -> undefined end;
to_integer(_,_Base) -> undefined.

to_binary(List) -> erlang:iolist_to_binary(List).

is_empty(<<>>) -> true;
is_empty([]) -> true;
is_empty(undefined) -> true;
is_empty(_) -> false.

% http://www.utf8-chartable.de/unicode-utf8-table.pl?utf8=dec
% https://www.owasp.org/index.php/XSS_%28Cross_Site_Scripting%29_Prevention_Cheat_Sheet#XSS_Prevention_Rules
% check correct binaryes: unicode:characters_to_list(<<194,128>>). => iolist() | {error,iolist(),binary()}

% Tail recursion
html_escape(<<>>, Acc) -> Acc;
html_escape(<<U,T/binary>>, Acc) -> % when U =< 62 andalso U >= 0 ->
    R = case U of
        $& ->   <<"&amp;">>;
        $< ->   <<"&lt;">>;
        $> ->   <<"&gt;">>;
        $" ->   <<"&quot;">>;
        $' ->   <<"&#x27;">>;
        $/ ->   <<"&#x2F;">>;
        % $\s ->    <<"&nbsp;">>;
        $\t ->  <<"&nbsp;&nbsp;&nbsp;&nbsp;">>;
        $\n ->  <<"<br/>">>;
        $\r ->  <<>>;
        U when U =< 0 -> <<>>;
        _ -> <<U>>
    end,
    html_escape(T, <<Acc/binary,R/binary>>).

html_escape(B) when is_binary(B) -> html_escape(B, <<>>);
html_escape(B) -> html_escape(wf:to_binary(B), <<>>).

plaintext_escape(<<>>, Acc) -> Acc;
plaintext_escape(<<U,T/binary>>, Acc) ->
    R = case U of
        $\n ->  <<"\\n">>;
        $\t ->  <<"\\t">>;
        $\r ->  <<>>;
        U when U =< 0 -> <<>>;
        _ -> <<U>>
    end,
    plaintext_escape(T, <<Acc/binary,R/binary>>).
plaintext_escape(B) when is_binary(B) -> plaintext_escape(B, <<>>);
plaintext_escape(B) -> plaintext_escape(wf:to_binary(B), <<>>).

strip(Any) -> binary:replace(wf:to_binary(Any),[<<$\n>>,<<$\r>>,<<$\t>>],<<>>,[global]).

prevent_undefined(A,Default) -> case A of undefined -> Default; _ -> A end.

shared_event(profile, {twitter,logintwitter}=E) ->
    avz:event(E);
shared_event(_, signin) ->
    ?SESSION:set_param(profile,signin),
    wf:redirect("/profile");
shared_event(M, logout) ->
    wf:info(?MODULE, "Logot from: ~p", [M]),
    u:logout(),
    wf:redirect("/");
shared_event(Module, Event) ->
    wf:info(Module, "Unknown event: ~p", [Event]),
    skip.
