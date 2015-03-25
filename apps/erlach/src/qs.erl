-module(qs).
-compile(export_all).

-include("erlach.hrl").
-include_lib("db/include/board.hrl").

% make link
ml(root)                         -> "/";
ml({thread,create,blog,Uri})     -> "/"++wf:to_list(Uri)++"/blog/new";
ml({thread,create,_,Uri})        -> "/"++wf:to_list(Uri)++"/new";
ml({thread,blog,Uri,Id})         -> "/"++wf:to_list(Uri)++"/blog/"++erlang:integer_to_list(Id,?IDS_BASE);
ml({thread,_T,Uri,Id})           -> "/"++wf:to_list(Uri)++"/"++erlang:integer_to_list(Id,?IDS_BASE);
ml({board,blog,Uri})             -> "/"++wf:to_list(Uri)++"/blog";
ml({board,_T,Uri})               -> "/"++wf:to_list(Uri);
ml({board,Uri})                  -> "/"++wf:to_list(Uri); % TODO: remove
ml({category,blog,Uri,Category}) -> "/"++wf:to_list(Uri)++"/blog/:"++wf:to_list(Category);
ml({category,_T,Uri,Category})   -> "/"++wf:to_list(Uri)++"/:"++wf:to_list(Category).

init() ->
    Proplist=lists:foldl(fun(#board{id=Id,uri=Uri},Acc) -> [{wf:to_binary(Uri),Id}|Acc] end,[],kvs:all(board)),
    ets:insert(globals,{board_uri,Proplist}), Proplist.

board_uri_to_id(Uri) ->
    proplists:get_value(Uri, ets:lookup_element(globals,board_uri,2), undefined).
board_id_to_uri(Id) ->
    case lists:keyfind(Id,2,ets:lookup_element(globals,board_uri,2)) of false -> undefined; {Uri,Id} -> Uri end.
