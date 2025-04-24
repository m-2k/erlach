-module(erlach_config).
-compile(export_all).

debug() -> true.
log_level() -> wf:config(erlach,log_level,info).
log_modules() ->
    wf:config(erlach,log_modules,[

        n2o_async,
        n2o_proto,
    %    n2o_client,
    %    n2o_static,
        n2o_stream,
        n2o_nitrogen,
        n2o_session,

        n2o_document,
        n2o_proto,
        n2o_relay,
        n2o_file,

        spa_event,
        spa_feeds,
        spa_filter,
        spa_image,
        spa_lambda_event,
        % spa_proxy,
        spa_record_manager,
        spa_render,
        spa_statistic,
        spa_utils,
        spa,
        
        action_update,
        
        debug,
        
        erlach_db,

        erlach,
        erlach_qs,
        erlach_spa,
        erlach_main,
        erlach_board,
        erlach_thread,
        erlach_stream,
        erlach_services,
        erlach_event_router,
        erlach_utils,
        erlach_image,
        erlach_about,
        erlach_auth,
        % erlach_subscription,
        erlach_stat,
        erlach_filter,
        erlach_feeds,
        erlach_settings
        ]).
info() ->  spawn(fun()-> wf:info(hd(log_modules()),"~p",[mnesia:info()]) end).

expire_time_to_edit_messages() -> 30*60. % 30 min

render_preload_count(_) -> 10.
render_part_count(_) -> 10.

cowboy_nb_acceptors() -> 100.

password_length() -> 16.
password_chars_allowed() ->
    <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!?@#$%^&*()[]{}|:;<>">>.
hash_algorithm() -> sha512.
hash_delay() -> 0.
salt_length() -> 4.
salt_local() -> <<2,224,10,38>>.
join_auth_key_length() -> 24.

notification_read_timeout() -> 10000.

image_convert_timeout() -> 600000.