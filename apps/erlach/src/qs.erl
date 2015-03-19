-module(qs).
-compile(export_all).

-include("erlach.hrl").
-include_lib("db/include/board.hrl").

% parse_qs(Req) ->
%     wf:info(?MODULE,"TEST", []),
%     {Bindings,_}=cowboy_req:bindings(Req),
%
%     lists:foldl(fun({Key,Value}, R) ->
%         case Key of
%             board    -> R#route{board=Value};
%             type     -> R#route{type=Value};
%             thread   -> R#route{thread=Value};
%             category -> R#route{category=Value};
%             new      -> R#route{new=Value}
%         end end,#route{},Bindings).

% make link
ml({thread,create,Uri})          -> "/"++wf:to_list(Uri)++"/new";
ml({thread,Uri,Id})              -> "/"++wf:to_list(Uri)++"/"++erlang:integer_to_list(Id,?IDS_BASE);
ml({board,Uri})                  -> "/"++wf:to_list(Uri);
ml({category,blog,Uri,Category}) -> "/"++wf:to_list(Uri)++"/blog/:"++wf:to_list(Category);
ml({category,_,Uri,Category})    -> "/"++wf:to_list(Uri)++"/:"++wf:to_list(Category);
ml({board,blog,Uri})             -> "/"++wf:to_list(Uri)++"/blog".

init() ->
    Proplist=lists:foldl(fun(#board{id=Id,uri=Uri},Acc) -> [{wf:to_binary(Uri),Id}|Acc] end,[],kvs:all(board)),
    ets:insert(globals,{board_uri,Proplist}), Proplist.

board_uri_to_id(Uri) ->
    proplists:get_value(Uri, ets:lookup_element(globals,board_uri,2), undefined).
