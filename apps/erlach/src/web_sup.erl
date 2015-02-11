-module(web_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/user.hrl").
-define(APP, erlach).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).


% start_child(Name) ->
% 	case whereis(Name) of
% 		P when is_pid(P) -> already_started;
% 		_ -> supervisor:start_child(?MODULE,{Name,{Name,start_link,[]},transient,1000,worker,[Name]})
% 	end.

init([]) ->

    case cowboy:start_http(http, 3, [{port, wf:config(n2o,port,8000)}],
                                    [{env, [{dispatch, dispatch_rules()}]}]) of
        {ok, _} -> ok;
        {error,{{_,{_,_,{X,_}}},_}} -> io:format("Can't start Web Server: ~p\r\n",[X]), halt(abort,[]);
        X -> io:format("Unknown Error: ~p\r\n",[X]), halt(abort,[]) end,

    kvs:join(),
    case kvs:get(board,1) of {ok, _} -> ok; _ -> utils:init_db() end,
	
	% start_child(image),
	% supervisor:start_child(web_sup, {image, {image, start_link, []}, transient,1000,worker,[image]}),
	ChildSpec = {image, {image, start_link, []}, permanent,1000,worker,[image]},
    {ok, {{one_for_one, 5, 10}, [ChildSpec]}}.

mime() -> [{mimetypes,cow_mimetypes,all}].

dispatch_rules() ->
    cowboy_router:compile(
        [{'_', [
            {"/static/[...]", n2o_dynalo, {dir, "apps/erlach/priv/static", mime()}},
            {"/n2o/[...]", n2o_dynalo, {dir, "deps/n2o/priv", mime()}},
            {"/rest/:resource", rest_cowboy, []},
            {"/rest/:resource/:id", rest_cowboy, []},
            % {"/ws/[...]", bullet_handler, [{handler, n2o_bullet}]},
            {"/ws/[:module/[:id/[:extra/[...]]]]", bullet_handler, [{handler, n2o_bullet}]},
            % {"/[:module/[:id/[blog]]]", [{id, int}], n2o_cowboy, []}
            {"/[:module/[:id/[:extra/[...]]]]", n2o_cowboy, []}
            % {'_', n2o_cowboy, []}
    ]}]).
