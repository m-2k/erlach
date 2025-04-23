-module(element_media_input).
-author('andy').
-compile(export_all).
-include("erlach.hrl").

-include_lib("n2o/include/wf.hrl").
-include_lib("nitro/include/nitro.hrl").

reflect() -> record_info(fields, media_input).

render_element(#media_input{id=Id,image=ImId,type=Type}) when Type =:= thread orelse Type =:= post ->
    Panel=case Id of undefined -> wf:temp_id(); _ -> Id end,
    Topic=wf:temp_id(),
    Input=wf:temp_id(),
    Image=case ImId of undefined -> wf:temp_id(); _ -> ImId end,
    Store=wf:temp_id(),
    Selector=wf:temp_id(),
    Sage=wf:temp_id(),
    Forms=case Type of thread -> [Topic,Input,Selector]; post -> [Sage,Input,Selector] end,
    BeforeJs="",
    Value="["
        "tuple(tuple(utf8_toByteArray('"++Store++"'), bin('detail')),[]),"++
        case Type of thread -> "tuple(utf8_toByteArray('"++Topic++"'), utf8_toByteArray(qi('"++Topic++"').innerText)),";
            _ -> "tuple(utf8_toByteArray('"++Sage++"'), utf8_toByteArray(querySourceRaw('"++Sage++"')))," end ++
        "tuple(utf8_toByteArray('"++Input++"'), utf8_toByteArray(qi('"++Input++"').innerText)),"
        "tuple(utf8_toByteArray('"++Selector++"'), utf8_toByteArray(querySourceRaw('"++Selector++"')))"
        "]",
        
    wf:wire("loadFromLocalStorage();"),
    wf:wire(#event{type=click,postback=#add{target=post,forms=Forms,feed=Type},target=Store,module=element_media_input,source=Value}),
    wf:wire(#jq{target=Selector,property=checked,right="true"}),
    
    MakeAsContentEditable=fun(Target) ->
        wf:wire(#bind{target=Target,type=focusout,postback="e.target.innerText=trim(e.target.innerText);"}),
        wf:wire("unrich('"++Target++"');"),
        wf:wire(#jq{target=Target,property=contentEditable,right="true"})
    end,
    case Type of thread -> MakeAsContentEditable(Topic); _ -> skip end,
    MakeAsContentEditable(Input),
    
    Controls=#panel{class= <<"input-controls center">>,body=[
        % submit(Store),
        #button{id=Store,body= <<"Store">>},
        case Type of
            thread -> #button{body= <<"Cancel">>,postback=#cancel{target=input,panel=Panel}};
            post -> #button{id=Sage,body= <<"Sage">>,postback=#render_event{target=input,event={sage,true,Sage}},value=false}
        end
        ]},
    
    Elements=#panel{id=Panel,class= <<"post">>,body=[
        #input{id=Selector,type= <<"checkbox">>,class= <<"input-selector">>,postback=?UNDEF},
        case Type of thread -> Controls; _ -> [] end,
        #panel{class= <<"post-content">>,body=[
            #label{id=Image,for=Selector,class= <<"post-image empty">>,body=#panel{body=[
                #panel{class= <<"image-process fl cent">>,body=wf:jse(progress(<<"white">>))},
                #canvas{}
                ]}},
            case Type of thread -> #panel{id=Topic,class= <<"post-topic single-line">>,data_fields=[{placeholder,<<"Type Topic Here...">>}]};
                post -> [] end,
            #panel{id=Input,class= <<"post-message">>,data_fields=[{placeholder,<<"Type Message Here...">>}]}
        ]},
        case Type of thread -> []; post -> Controls end
    ]},
    wf:render(Elements).

render_action(#event{postback=P,actions=_A,source=S,target=C,type=T,delegate=D,validation=V}) ->
    Element=wf:to_list(C),
    Data=S,
    PostbackBin = wf_event:new(P, Element, D, event, Data, []),
    wf:render(#bind{postback=PostbackBin,target=Element,type=wf:to_binary(T)}).
    
submit(Id) ->
    #svg{id=Id,class= <<"svg">>,width="40px", height="40px", viewBox="0 0 251 251", version="1.1", 
        xmlns="http://www.w3.org/2000/svg", xmlnsxlink="http://www.w3.org/1999/xlink",
        body= wf:jse(<<"<g stroke='none' stroke-width='1' fill='none' fill-rule='evenodd'><circle fill='#26C6DA' cx='125.5' cy='125.5' r='125.5'></circle><g transform='translate(83.000000, 63.000000)' stroke='#FFFFFF' stroke-width='5' stroke-linecap='square'><path d='M43.5,3.5 L43.5,122.768608'></path><path d='M43.5,0.5 L0.314354241,43.6856461'></path><path d='M43.5,0.5 L86.5,43.5'></path></g></g>">>)}.
    

progress(Color) ->
    <<"<svg width='100px' height='100px' viewBox='0 0 100 100'
         version='1.1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink'>
      <g transform='translate(50, 50)'>
        <circle cx='0' cy='0' r='40' style='fill:rgba(0,0,0,0);stroke:",Color/binary,"; stroke-width:1; stroke-dasharray:75, 50;'>
          <animateTransform attributeType='xml' attributeName='transform' type='rotate'
              from='0' to='360' begin='0' dur='15s' repeatCount='indefinite' />
        </circle>
      </g>
    </svg>">>.