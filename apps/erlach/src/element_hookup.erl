-module(element_hookup).
-author('Andy').
-compile(export_all).
-include("erlach.hrl").

reflect() -> record_info(fields, hookup).

render_element(Record) -> 
    Id = case Record#hookup.postback of
        undefined -> Record#hookup.id;
        Postback ->
            ID = case Record#hookup.id of undefined -> spa:temp_id(); I -> I end,
            wf:wire(#event{ type=click,postback=Postback,target=ID,module=element_hookup,
                            source=Record#hookup.source,delegate=Record#hookup.delegate}),
            ID end,
    List = [
      % global
      {<<"accesskey">>, Record#hookup.accesskey},
      {<<"class">>, Record#hookup.class},
      {<<"contenteditable">>, case Record#hookup.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#hookup.contextmenu},
      {<<"dir">>, case Record#hookup.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#hookup.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#hookup.dropzone},
      {<<"hidden">>, case Record#hookup.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Id},
      {<<"lang">>, Record#hookup.lang},
      {<<"spellcheck">>, case Record#hookup.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#hookup.style},
      {<<"tabindex">>, Record#hookup.tabindex},
      {<<"title">>, Record#hookup.title},
      {<<"translate">>, case Record#hookup.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"href">>, wf:coalesce([Record#hookup.href,Record#hookup.url])},
      {<<"hreflang">>, Record#hookup.hreflang},
      {<<"target">>, Record#hookup.target},
      {<<"media">>, Record#hookup.media},
      {<<"rel">>, Record#hookup.rel},
      {<<"type">>, Record#hookup.type},
      {<<"download">>, Record#hookup.download},
      {<<"name">>, Record#hookup.name},
      {<<"onclick">>, Record#hookup.onclick},
      {<<"onmouseover">>, Record#hookup.onmouseover} | Record#hookup.data_fields ],
    wf_tags:emit_tag(<<"a">>, wf:render(Record#hookup.body), List).

%%% Render from: nitro/src/actions/action_event.erl
%%% -record(event, {?ACTION_BASE(action_event), ...}).
%%% -define(ACTION_BASE(Module), ..., module=Module, actions, source=[]).
render_action(#event{postback=P,actions=_A,source=S,target=C,type=T,delegate=D,validation=V}) ->
    Element=wf:to_list(C),
    Data=list_to_binary([<<"[tuple(tuple(utf8_toByteArray('">>,Element,<<"'),bin('detail')),event.detail)">>,
         [ begin {SrcType,Src2}=case is_atom(Src) of
                 true -> { <<"atom">>,atom_to_list(Src) };
                 false -> { <<"utf8_toByteArray">>,Src } end,
             [ <<",tuple(">>,SrcType,<<"('">>,Src2,<<"'),querySource('">>,Src2,<<"'))">> ]
             end || Src <- S ],<<"]">>]),
    PostbackBin = [wf_event:new(P, Element, D, event, Data, S, V),<<" event.preventDefault();">>],
    wf:render(#bind{postback=PostbackBin,target=Element,type=wf:to_binary(T)}).
