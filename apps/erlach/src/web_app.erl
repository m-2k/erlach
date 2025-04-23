-module(web_app).
-author('andy').

-behaviour(application).
-export([start/0, start/2, stop/1, main/1]).

main(A) ->

   mad_repl:main(A).

start() -> start(normal, []).
start(_StartType, _StartArgs) -> 

	AVZ_Deps = [inets, asn1, public_key, ssl], % AVZ Dependencies
	[ application:start(App) || App <- AVZ_Deps],
    Res = web_sup:start_link(),
    Res.

stop(_State) -> ok.
