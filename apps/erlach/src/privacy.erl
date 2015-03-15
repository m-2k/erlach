-module(privacy).
-vsn('0.0.0').

-compile(export_all).
-include_lib("n2o/include/wf.hrl").
% -include_lib("kvs/include/kvs.hrl").
% -include_lib("kvs/include/feed.hrl").
% -include_lib("db/include/post.hrl").
% -include_lib("db/include/attachment.hrl").
%
% -include_lib("db/include/board.hrl").
% -include_lib("db/include/user.hrl").
-include("erlach.hrl").

% -ifndef(SESSION).
% -define(SESSION, (wf:config(n2o,session,erlach_session))).
% -endif.

-define(PRIVACY_POST_ID, 10).
-define(PRIVACY_THREAD_ID, 6).
-define(PRIVACY_BOARD_URI, e).

main() -> #dtl{file="erlach",app=erlach,bindings=[{body,body()}, {theme,<<"glassy privacy">>}, {title,<<"Privacy">>}]}.
body() ->
    Content = case kvs:get(post, ?PRIVACY_POST_ID) of
        {ok, Post} ->
            {ok, Html} = html:post(Post),
            [ #panel{class= <<"content-title">>,body= <<"Privacy2">>},
                #panel{id=posts,body=[Html]},
                #panel{class= <<"center">>,body=[
                    #link{class= <<"button primary">>, href=qs:ml({thread,?PRIVACY_BOARD_URI,?PRIVACY_THREAD_ID}), body= <<"View thread">>}
                    ]} ];
        _ -> []
    end,
    html:body(Content).

event(init) -> ok;

event(terminate) -> skip;
event(Event) -> guard:shared_event(?MODULE, Event).
