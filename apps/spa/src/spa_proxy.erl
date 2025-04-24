-module(spa_proxy).
-author('Andy').
-compile(export_all).

-include("spa.hrl").
-export([proxy/1, unknown/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%                                                     %%%%%
%%%%%         CODE WARNING: ABSTRACT RECORDS MODE         %%%%%
%%%%%                                                     %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

proxy({client,E}) -> proxy(E);
proxy({server,E}) -> proxy(E);

proxy(#postback{}=P) -> {M,F}=wf:config(?SPA_CONFIG_NAME,postback_fun), M:F(P);
proxy(E) ->
    wf:info(?M, "Autoeventing event ~p to ~p",[case is_tuple(E) of true -> element(1,E); false -> E end, target(E)]),
    (target(E)):event(E).

target() -> case spa:st() of S when ?REC(S,st) -> spa:render(S); _ -> ?UNDEF end.
target(E) when is_tuple(E), size(E) >= size(#rdr_ev{}), is_atom(element(1,E)), is_atom(element(#rdr_ev.render,E)) ->
    case element(#rdr_ev.render,E) of
        ?UNDEF -> target();
        Render -> Render
    end;
target(_) -> target().

unknown(M,E) -> wf:warning(?M,"Unknown event: ~p for module: ~p", [format(E),M]).

%%% Prevent MEM-leaking for huge data in events
format(E) when is_tuple(E) -> element(1,E);
format(E) -> E.
