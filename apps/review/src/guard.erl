-module(guard).
-compile(export_all).

-ifndef(SESSION).
-define(SESSION, (wf:config(n2o,session,erlach_session))).
-endif.

to_integer(I) when is_integer(I) -> I;
to_integer(B) when is_binary(B) -> to_integer(binary_to_list(B));
to_integer(L) when is_list(L) -> case string:to_integer(L) of {I,[]} -> I; _ -> undefined end;
to_integer(_) -> undefined.

% http://www.utf8-chartable.de/unicode-utf8-table.pl?utf8=dec
% https://www.owasp.org/index.php/XSS_%28Cross_Site_Scripting%29_Prevention_Cheat_Sheet#XSS_Prevention_Rules
% check correct binaryes: unicode:characters_to_list(<<194,128>>). => iolist() | {error,iolist(),binary()}

% Test:
% Binary size = 233201
%
% {Time, _} = timer:tc(wf_convert,html_escape,[B]), erlang:display(Time), f(Time).
% => 28121,25585,24934,25426,23616,25503,24235,29047,25047 ms
%
% {Time, _} = timer:tc(wf_convert,html_encode,[B]), erlang:display(Time), f(Time).     
% => 57715,47240,46793,29764,38894,41349,32887,37214,40001 ms
%
% {Time, _} = timer:tc(wf_convert,enc_1,[B,1000]), erlang:display(Time), f(Time).
% => 32391488, 3220238 (100)
%
% {Time, _} = timer:tc(wf_convert,enc_2,[B,1000]), erlang:display(Time), f(Time).
% => 32697245, 3240024 (100)
%
%
% enc_1(V, 0) -> ok;
% enc_1(V, Count) ->
% 	html_encode(V),
% 	enc_1(V, Count-1).
%
% enc_2(V, 0) -> ok;
% enc_2(V, Count) ->
% 	html_escape(V),
% 	enc_1(V, Count-1).

% Tail recursion
html_escape(<<>>, Acc) -> Acc;
html_escape(<<U,T/binary>>, Acc) -> % when U =< 62 andalso U >= 0 ->
	R = case U of
		$& ->	<<"&amp;">>;
		$< ->	<<"&lt;">>;
		$> ->	<<"&gt;">>;
		$" ->	<<"&quot;">>;
		$' ->	<<"&#x27;">>;
		$/ ->	<<"&#x2F;">>;
		$\s ->	<<"&nbsp;">>;
		$\t ->	<<"&nbsp;&nbsp;&nbsp;&nbsp;">>;
		$\n ->	<<"<br/>">>;
		$\r ->	<<>>;
		U when U =< 0 -> <<>>;
		_ -> <<U>>
	end,
	% wf:info(?MODULE, "html_escape/2:1 ~p ~p ~p", [U, T, Acc]),
	% <<R/binary,(html_escape(T))/binary>>;
	html_escape(T, <<Acc/binary,R/binary>>).
% html_escape(<<U,T/binary>>, Acc) ->
	% <<U,(html_escape(T))/binary>>.
	% wf:info(?MODULE, "html_escape/2:2 ~p ~p ~p", [U, T, Acc]),
	% html_escape(T, <<Acc/binary,U>>).

html_escape(B) when is_binary(B) -> html_escape(B, <<>>);
html_escape(B) -> html_escape(wf:to_binary(B), <<>>).

% html_escape(<<U,T/binary>>) when U >= 128 and U <= 191 -> <<U,T/binary>>;
% html_escape(<<U2,U1,T/binary>>) when U2 >= 194 and U2 <= 223 and U1 >= 128 and U1 <= 191 -> <<U,T/binary>>;
% html_escape(<<U4,U3,U2,U1,T/binary>>) -> <<>>;

% html_encode(Binary) when is_binary(Binary) -> wf:html_encode(unicode:characters_to_list(Binary));

prevent_undefined(A,Default) -> case A of undefined -> Default; _ -> A end.

shared_event(profile, {twitter,logintwitter}=E) ->
	avz:event(E);
shared_event(_, signin) ->
	?SESSION:set_param(profile,signin),
	wf:redirect("/profile");
shared_event(_, logout) ->
	u:logout(),
	wf:redirect("/");
shared_event(Module, Event) ->
    wf:info(Module, "Unknown event: ~p", [Event]),
    skip.