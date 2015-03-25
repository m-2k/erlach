-module(config).
-compile(export_all).

log_modules() ->
    [
        % active,
        % wf,
        % % n2o_websocket,
        % % n2o_document,
        % n2o_query,
        % n2o_bullet,
        % % login,
        % % n2o_dynalo,
        % n2o_dynroute,
        % n2o_nitrogen,
        % root,
        board,
        thread,
        % profile,
        % % js_session2,
        % % erlach_session,
        % % n2o_nitrogen,
        % n2o_event,
        % % js_session,
        % n2o_rails,
        % kvs,
        % twitter,
        % avz,
        % wf_convert,
        % image,
        % utils,
        % u,
        html,
        % access,
        routes,
        % guard,
        qs
        % n2o_binary
    ].

log_level() -> warning.

% debug() -> false.
debug() -> false.

info() ->  spawn(fun()-> wf:info(index,"~p",[mnesia:info()]) end).

expire_time_to_edit_messages() -> 30*60. % 30 min