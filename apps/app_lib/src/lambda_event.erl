-module(lambda_event).
-author('Andy').

-compile(export_all).
-include_lib("nitro/include/nitro.hrl").

new(Module,Postback,Code) -> #event{postback=Postback,module=?MODULE,delegate=Module,source=Code}.

render_action(#event{postback=P,delegate=D,source=S}) ->
    E=wf_event:new(P,"lambda",D,api_event,S,[]),
    [list_to_binary(["{ (function() { ",E," })(); }"])].
    
