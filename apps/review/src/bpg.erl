-module(bpg). %% BOARD LISTENING
-vsn('0.0.0').

-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/feed.hrl").
-include_lib("db/include/post.hrl").
-include_lib("db/include/attachment.hrl").

-include_lib("db/include/board.hrl").
-include_lib("db/include/thread.hrl").
-include("erlach.hrl").

peer()    -> io_lib:format("~p",[wf:peer(?REQ)]).
main()    -> #dtl{file="bpg",app=review,bindings=[{title,"BPG"}]}.
body() ->
    wf:info(?MODULE, "body ~p", [0]),
    [ #span{ body = <<"404 - Not found">> }].
    


event(init) -> ok;
event(terminate) -> skip;
event(Event) ->
    wf:info(?MODULE, "Unknown event: ~p", [Event]),
    skip.
event(#ev{msg={chat,Pid}},#cx{req=_Req}=Cx) -> Pid ! {peer(), wf:qp(message,Cx)};
event(_Event,_) -> skip.
