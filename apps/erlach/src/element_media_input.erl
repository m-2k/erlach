-module(element_media_input).
-author('Andy').
-compile(export_all).
-include("erlach.hrl").

reflect() -> record_info(fields, media_input).

render_element(#media_input{id=Id,image=ImId,target=Target,disabled=Disabled}) when Target =:= thread orelse Target =:= post ->
    Class=black,
    ClassStore=orange,
    Panel=case Id of undefined -> spa:temp_id(); _ -> Id end,
    Topic=spa:temp_id(),
    Input=spa:temp_id(),
    Image=case ImId of undefined -> spa:temp_id(); _ -> ImId end,
    Store=spa:temp_id(),
    Selector=spa:temp_id(),
    Sage=spa:temp_id(),
    AddImage=spa:temp_id(),
    AddImageInput=spa:temp_id(),
    Forms=case Target of thread -> [Topic,Input,Selector]; post -> [Sage,Input,Selector] end,
    Value="["
        "tuple(tuple(utf8_toByteArray('"++Store++"'), bin('detail')),[]),"++
        case Target of thread -> "tuple(utf8_toByteArray('"++Topic++"'), utf8_toByteArray(qi('"++Topic++"').innerText)),";
            _ -> "tuple(utf8_toByteArray('"++Sage++"'), utf8_toByteArray(querySourceRaw('"++Sage++"')))," end ++
        "tuple(utf8_toByteArray('"++Input++"'), utf8_toByteArray(qi('"++Input++"').innerText)),"
        "tuple(utf8_toByteArray('"++Selector++"'), utf8_toByteArray(querySourceRaw('"++Selector++"')))"
        "]",
        
    wf:wire("textRestore();"),
    wf:wire(#event{type=click,postback=#add{target=Target,forms=Forms},target=Store,module=?M,source=Value}),
    wf:wire(#jq{target=Selector,property=checked,right="true"}),
    % Key: which=13 char=0 key=13 meta=true ctrl=false
    
    
    MakeAsContentEditable=fun(Editable) ->
        wf:wire(#bind{target=Editable,type=keydown,postback=wf:f("if(e.metaKey && !e.ctrlKey && e.which === 13){qi('~s').click();};",[Store])}),
        wf:wire(#bind{target=Editable,type=focusout,postback="e.target.innerText=trim(e.target.innerText);"}),
        wf:wire(#bind{target=Editable,type=blur,postback="e.target.innerText=trim(e.target.innerText);"}), % FF fix
        wf:wire("unrich('"++Editable++"');"),
        wf:wire(#jq{target=Editable,property=contentEditable,right="true"})
    end,
    case Target of thread -> MakeAsContentEditable(Topic); _ -> skip end,
    MakeAsContentEditable(Input),
    
    wf:wire(wf:f("qi('~s').addEventListener('change', handleFileSelect, false);",[AddImageInput])),
    
    Controls=#panel{class= <<"input-controls center">>,body=[
        % submit(Store),
        #button{id=Store,class=ClassStore,disabled=Disabled,body= <<"Store">>},
        case Target of
            thread -> #button{class=Class,body= <<"Cancel">>,postback=#cancel{target=input,panel=Panel}};
            post -> #button{id=Sage,class=Class,body= <<"Sage">>,postback=#render_event{target=input,event={sage,true,Sage}},value=false}
        end,
        #button{id=AddImage,class=Class,body= <<"Image">>,onclick=wf:jse(wf:f("qi('~s').click();",[AddImageInput]))},
        #input{id=AddImageInput,type=file}
        ]},
    
    Elements=#panel{id=Panel,class= <<"post">>,body=[
        #input{id=Selector,type= <<"checkbox">>,class= <<"input-selector">>,postback=?UNDEF},
        case Target of thread -> Controls; _ -> [] end,
        #panel{class= <<"post-content">>,body=[
            #label{class= <<"image-manage">>,for=Selector,body=#panel{body=[ % TODO: js fix replacing label to div
                #panel{body=#button{class=["control-thumb",sea,checked],body= <<"Size">>}}
                ]}},
            #panel{id=Image,class= <<"post-image empty">>,body=[
                #panel{class= <<"image-process fl cent">>,body=wf:jse(progress(<<"white">>))},
                #canvas{class=[media,image]}
            ]},
            case Target of
                thread -> #panel{id=Topic,class= <<"post-topic single-line">>,data_fields=[{placeholder,<<"Type topic here…"/utf8>>}]};
                post -> [] end,
            #panel{id=Input,class= <<"post-message">>,data_fields=[{placeholder,<<"Type message here…"/utf8>>}]}
        ]},
        case Target of thread -> []; post -> Controls end
    ]},
    wf:render(Elements).

render_action(#event{postback=P,actions=_A,source=S,target=C,type=T,delegate=D,validation=V}) ->
    Element=wf:to_list(C),
    Data=S,
    PostbackBin = wf_event:new(P, Element, D, event, Data, []),
    wf:render(#bind{postback=PostbackBin,target=Element,type=wf:to_binary(T)}).
    
submit() ->
    <<"<svg width='40px' height='40px' viewBox='0 0 251 251' version='1.1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' <g stroke='none' stroke-width='1' fill='none' fill-rule='evenodd'><circle fill='#26C6DA' cx='125.5' cy='125.5' r='125.5'></circle><g transform='translate(83.000000, 63.000000)' stroke='#FFFFFF' stroke-width='5' stroke-linecap='square'><path d='M43.5,3.5 L43.5,122.768608'></path><path d='M43.5,0.5 L0.314354241,43.6856461'></path><path d='M43.5,0.5 L86.5,43.5'></path></g></g></svg>">>.
    
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