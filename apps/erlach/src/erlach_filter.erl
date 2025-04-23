-module(erlach_filter).
-compile(export_all).
-author('Andy').

-include("erlach.hrl").

% M - {pickle,<<"e-303422326">>,<<"VpoJ***0=">>,[{{"e-303422326",<<"detail">>},[]},{"e-303422310","false"},{"e-303422334","gbgfbhf"},{"e-303422318","on"}]}
%     {server,{pubsub,undefined,anal,update,{post,total},{1,72},<0.677.0>}}
% R - https://github.com/ninenines/cowboy/blob/2.0.0-pre.3/src/cowboy_req.erl#L108-L147
% S - https://github.com/synrc/n2o/blob/master/include/wf.hrl#L5
% Protos - [n2o_heart,n2o_nitrogen,n2o_file,n2o_client]
% Acc - []
filter(M,R,S,Protos,Acc) ->
    {_,_,NS}=erlang:timestamp(),
    n2o_proto:push(M,R,S,Protos,Acc).


filter2({client,_}=M,    R,S,Protos,Acc) -> checkSesExpiring(filter_action(M),{M,R,S,Protos,Acc});
filter2({direct,_}=M,    R,S,Protos,Acc) -> checkSesExpiring(filter_action(M),{M,R,S,Protos,Acc});
filter2({pickle,_,_,_}=M,R,S,Protos,Acc) -> checkSesExpiring(filter_action(M),{M,R,S,Protos,Acc});
filter2(M,               R,S,Protos,Acc) -> n2o_proto:push(M,R,S,Protos,Acc).

filter_action(M) -> true.

checkSesExpiring(true,{M,R,S,Protos,Acc}) -> n2o_proto:push(M,R,S,Protos,Acc);
checkSesExpiring(_,{_,R,S,_,_})           -> {reply,wf:format({io,n2o_nitrogen:render_actions(wf:actions()),<<>>}),R,S}.