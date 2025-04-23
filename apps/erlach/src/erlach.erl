-module(erlach).
-author('Andy').
-behaviour(supervisor).
-behaviour(application).

-export([init/1, start/0, start/2, stop/1, main/1]).
-include("erlach.hrl").

main(A)    -> mad:main(A).
start()    -> start(normal,[]).
start(_,_) -> supervisor:start_link({local,?MODULE},?MODULE,[]).
stop(_)    -> ok.
    
init([]) ->
    case cowboy:start_http(http_erlach,?CONFIG:cowboy_nb_acceptors(),port(),env()) of
         {ok, _}   -> ok;
         {error,_} -> halt(abort,[]) end,
    
    kvs:join(),
    
    spawn(fun() -> wf:info(?M,"erlach_image starting from ~p",[self()]), erlach_image:start() end),
    
    {ok, {{one_for_one, 5, 10}, []}}.


env()    -> [ { env, [ { dispatch, points() } ] } ].
static() ->   { dir, "apps/erlach/priv/static", mime() }.
n2o()    ->   { dir, "deps/n2o/priv",           mime() }.
mime()   -> [ { mimetypes, cow_mimetypes, all   } ].
port()   -> [ { port, wf:config(erlach,port,8000)  } ].
all()    -> {priv_file, erlach, "static/erlach.html"}.
points() -> cowboy_router:compile([{'_', [
    {"/static/[...]",         n2o_static,    static() }, % remove in production
    {"/n2o/[...]",            n2o_static,    n2o() },    % remove in production
    {"/ws/[:q1/[:q2/[:q3]]]", n2o_stream,    [] },
    {'_',                     cowboy_static, all() }     % remove in production
    ]}]).
