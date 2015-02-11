-module(routes).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-export([init/2, finish/2]).

%% U can use default dynamic routes or define custom static as this
%% Just put needed module name to sys.config:
%% {n2o, [{route,routes}]}
%% Also with dynamic routes u must load all modules before starting Cowboy
%% [code:ensure_loaded(M) || M <- [index, login, ... ]]

finish(State, Ctx) -> {ok, State, Ctx}.
init(State, Ctx) -> 
    Path = wf:path(Ctx#cx.req),
    
    Map = qs:parse_qs(Ctx#cx.req),
    % wf:info(?MODULE,"Binding INIT: ~p",[erlang:get(matched_qs)]),
    
    wf:info(?MODULE,"Route: ~p~n",[Path]),
    % {ok, State, Ctx#cx{path=Path,module=route_prefix(Path,F,S)}}.
    {ok, State, Ctx#cx{path=Path,module=route(maps:get(m,Map,<<>>))}}.

% route_prefix(<<"/ws/",P/binary>>,F,S) -> route(P,F,S);
% route_prefix(<<"/",P/binary>>,F,S) -> route(P,F,S);
% route_prefix(P,F,S) -> route(P,F,S).

% route(<<>>,_,_)                 -> root;
% % route(<<"board">>)          -> board;
% % route(<<"thread">>)         -> thread;
% route(<<"profile">>,_,_)        -> profile;
% % route(<<"new">>)            -> thread;
% route(_,<<"board">>,Id) -> erlang:put(sasay,Id),board;
% route(_,<<"thread">>,Id) -> thread;
% route(<<"favicon.ico">>,_,_)    -> static_file;
% route(_,_,_) -> root.


route(undefined)         -> root;
route(<<"privacy">>)     -> privacy;
route(<<"donate">>)      -> donate;
route(<<"profile">>)     -> profile;
route(<<"board">>)       -> board;
route(<<"thread">>)      -> thread;
route(<<"favicon.ico">>) -> static_file;
route(_) -> root.
