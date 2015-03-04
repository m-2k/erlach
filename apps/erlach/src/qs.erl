-module(qs).
-compile(export_all).

-include("erlach.hrl").


% /
% /g
% /g/7000
% /thread
% /board
% /privacy
%
%
% /board/343
% /thread/




parse_qs(Req) ->
    {M, _} = cowboy_req:binding(module,  Req),
    {I, _} = cowboy_req:binding(id, Req),
    {E, _} = cowboy_req:binding(extra, Req),
    Map = #{m=>M,i=>I,e=>E},
    erlang:put(matched_qs, Map), Map.

% make link
ml({thread,create})       -> "/thread";
ml({thread,Id})       -> "/thread/" ++ erlang:integer_to_list(Id, ?IDS_BASE);
ml({board,Id})        -> "/board/"  ++ erlang:integer_to_list(Id, ?IDS_BASE);
ml({board,blog,Id}) -> "/board/"  ++ erlang:integer_to_list(Id, ?IDS_BASE) ++ "/blog".

