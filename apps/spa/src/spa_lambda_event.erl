-module(spa_lambda_event).
-author('Andy').

-compile(export_all).
-include_lib("nitro/include/nitro.hrl").

new(Module,Postback,Code) -> #event{postback=Postback,module=?MODULE,delegate=Module,source=Code}.

render_action(#event{postback=P,delegate=D,source=S}) ->
    E=wf_event:new(P,"lambda",D,api_event,S,[]),
    [list_to_binary(["{ (function() { ",E," })(); }"])].
    

%%% Usage
% pickup_data() -> wf:wire(lambda_event:new(?MODULE,{event,custom},"tuple(number(1),number(2))")).
% api_event(Pb,Linked,_State) -> wf:info(?M,"Custom api event: ~p~n~p~n",[Pb,Linked]).