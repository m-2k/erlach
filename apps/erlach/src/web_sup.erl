-module(web_sup).
-author('andy').

-behaviour(supervisor).
-export([start_link/0, init/1]).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/user.hrl").
-define(APP, erlach).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    case cowboy:start_http(http, 3, [{port, wf:config(n2o,port,8000)}],
                                    [{env, [{dispatch, dispatch_rules()}]}]) of
        {ok, _} -> ok;
        {error,{{_,{_,_,{X,_}}},_}} -> io:format("Can't start Web Server: ~p\r\n",[X]), halt(abort,[]);
        X -> io:format("Unknown Error: ~p\r\n",[X]), halt(abort,[]) end,

    kvs:join(),
    qs:init(),
    
	ChildSpec = {image, {image, start_link, []}, permanent,1000,worker,[image]},
    {ok, {{one_for_one, 5, 10}, [ChildSpec]}}.

mime() -> [{mimetypes,cow_mimetypes,all}].

dispatch_rules() ->
    cowboy_router:compile(
        [{'_', [
            {"/static/[...]", n2o_dynalo, {dir, "apps/erlach/priv/static", mime()}},
            {"/ws/[:q1/[:q2/[:q3]]]", bullet_handler, [{handler, n2o_bullet}]},
            {"/[:q1/[:q2/[:q3]]]", n2o_cowboy, []}
        ]}]).
