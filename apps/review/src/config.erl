-module(config).
-compile(export_all).

log_modules() ->
  [
    % n2o_websocket,
	n2o_document,
    n2o_query,
    n2o_bullet,
    % login,
	% n2o_dynalo,
    n2o_dynroute,
    n2o_nitrogen,
    root,
	board,
	thread,
	profile,
    % js_session2,
	% erlach_session,
    % n2o_nitrogen,
    n2o_event,
    % js_session,
    n2o_rails,
    kvs,
	twitter,
	avz,
	wf_convert,
	image,
	u
    % n2o_binary
  ].

websocket_port() -> {ws, 8000, wss, 443}.

info() ->  spawn(fun()-> wf:info(index,"~p",[mnesia:info()]) end).