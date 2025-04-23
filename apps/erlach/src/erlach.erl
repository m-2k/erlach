-module(erlach).
-author('andy').
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1, main/1]).

-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/user.hrl").

-include("erlach.hrl").

main(A)    -> mad:main(A).
start()    -> start(normal,[]).
start(_,_) -> supervisor:start_link({local,?MODULE},?MODULE,[]).
stop(_)    -> ok.
    
init([]) ->
    wf:info(?M,"INIT ~p",[self()]),
    case cowboy:start_http(http,config:cowboy_nb_acceptors(),port(),env()) of
         {ok, _}   -> ok;
         {error,_} -> halt(abort,[]) end,
    
    kvs:join(),
    spawn(fun() -> wf:info(?M,"Fun ~p",[self()]), erlach_image:start() end),

    {ok, {{one_for_one, 5, 10}, []}}.


env()    -> [ { env, [ { dispatch, points() } ] } ].
static() ->   { dir, "apps/erlach/priv/static", mime() }.
n2o()    ->   { dir, "deps/n2o/priv",           mime() }.
mime()   -> [ { mimetypes, cow_mimetypes, all   } ].
port()   -> [ { port, wf:config(n2o,port,8000)  } ].
points() -> cowboy_router:compile([{'_', [
    {"/static/[...]",         n2o_static, static()},
    {"/n2o/[...]",            n2o_static, n2o()},
    {"/ws/[:q1/[:q2]]",       n2o_stream, []},
    {'_',                     cowboy_static, {priv_file, erlach, "static/erlach.html"}}
    ]}]).
