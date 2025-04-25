-module(erlach_log).
-compile(export_all).

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
        spa_filter,
        spa_image,
        spa_lambda_event,
        spa_proxy,
        spa_record_manager,
        spa_render,
        spa_statistic,
        spa_utils,
        spa,

        
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
        erlach_markup,
        erlach_settings,
        erlach_signin,
        erlach_join,
        erlach_search,
        erlach_ban,
        
        eauth,
        eauth_event,
        eauth_mail,
        eauth_user,
        eauth_utils,
        
        % kvs_feeds,
        % kvs_feeds_server,
        
        fulltext_search,
        fulltext_search_engine
        ]).

info() ->  spawn(fun()-> wf:info(hd(log_modules()),"~p",[mnesia:info()]) end).
