-module(element_a).
-author('Andy').
-compile(export_all).

-include("spa_elements.hrl").

render_element(Record) ->
    Validate=case Record#a.postback of
        undefined -> Record#a.validate;
        _ -> wf:to_list([
            % "typeof pushScroll === 'function' && pushScroll();",
            "if(event.button === 1) return;",
            "event.preventDefault();",
            case Record#a.validate of undefined -> []; V -> V end])
    end,
    Record2=setelement(#link.module,setelement(#link.validate,setelement(1,Record,link),Validate),element_link),
    nitro:render(Record2).