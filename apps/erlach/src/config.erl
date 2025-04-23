-module(config).
-author('andy').
-compile(export_all).

log_modules() ->
    [
        % n2o_stream,
        % n2o_proto,
        % n2o_document,
        % cowboy_websocket,

        erlach,
        erlach_qs,
        erlach_spa,
        erlach_main,
        erlach_board,
        erlach_thread,
        erlach_event_router,
        erlach_image,
        
        % wf_convert,
        % n2o_file,
        n2o_async,
        
        ghjvkfhvghk
    ].

log_level() -> info.
debug() -> false.
info() ->  spawn(fun()-> wf:info(hd(log_modules()),"~p",[mnesia:info()]) end).

post_max_length() -> 2000.
topic_max_length() -> 100.

expire_time_to_edit_messages() -> 30*60. % 30 min

render_preload_count(_) -> 10.
render_part_count(_) -> 10.

cowboy_nb_acceptors() -> 100.

event_renders() -> [main,board,thread].

password_length() -> 16.
password_chars_allowed() ->
    <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!?@#$%^&*()[]{}|:;<>">>.
hash_algorithm() -> sha512.
hash_delay() -> 0.
salt_length() -> 4.
salt_local() -> <<2,224,10,38>>.
join_auth_key_length() -> 24.

notification_read_timeout() -> 10000.

filename(_Ftp) -> integer_to_list(erlang:unique_integer()).

image_convert_timeout() -> 600000.