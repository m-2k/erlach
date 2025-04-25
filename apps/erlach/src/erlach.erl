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
    case wf:config(n2o,mq) of n2o_syn -> syn:init(); _ -> skip end,
    kvs:join(),
    spawn(fun() -> wf:info(?M,"erlach_image starting from ~p",[self()]), erlach_image:start(default) end),
    {ok, {{one_for_one, 5, 10}, [spec()]}}.

spec()     -> ranch:child_spec(http_erlach, wf:config(erlach,acceptors,100), ranch_tcp, port(), cowboy_protocol, env()).
env()      -> [{ env, [{ dispatch, points(wf:config(erlach,env,prod)) }] }].
static()   ->   { dir, "apps/erlach/priv/static", mime() }.
n2o()      ->   { dir, "deps/n2o/priv",           mime() }.
mime()     -> [ { mimetypes, cow_mimetypes, all   } ].
port()     -> [ { port, wf:config(erlach,port,8000)  } ].
all()      -> {priv_file, erlach, "static/erlach.html"}.
services() -> {priv_file, erlach, "static/services.html"}.
services_test() -> {priv_file, erlach, "static/services-test.html"}.
file_bpgdec() ->
    {ok, Dir} = file:get_cwd(),
    filename:join(Dir, "deps/bpg_ww/bpgdec8a-ww.min.js").


points(prod) -> cowboy_router:compile([{'_', [
    {"/ws/[:q1/[:q2/[:q3/[:q4]]]]", n2o_stream,    [] }
    ]}]);
points(_) -> cowboy_router:compile([{'_', [
    {"/messages/[...]",             cowboy_static, services_test() },
    {"/services/[...]",             cowboy_static, services() },
    {"/static/bpgdec.js",           cowboy_static, {file, file_bpgdec()} },
    {"/static/[...]",               n2o_static,    static() },
    {"/n2o/[...]",                  n2o_static,    n2o() },
    {"/ws/[:q1/[:q2/[:q3/[:q4]]]]", n2o_stream,    [] },
    {'_',                           cowboy_static, all() }
    ]}]).
