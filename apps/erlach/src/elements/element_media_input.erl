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
    
    wf:wire("textRestore();"),
    
    Value = [ case Target of thread -> {Topic,"utf8_toByteArray(qi('"++Topic++"').innerText)"};
                                  _ -> {Sage,"utf8_toByteArray(querySourceRaw('"++Sage++"'))"} end,
                {Input,"utf8_toByteArray(qi('"++Input++"').innerText)"},
                {Selector,"utf8_toByteArray(querySourceRaw('"++Selector++"'))"} ],
    wf:wire(#event{type=click,target=Store,postback=#add{target=Target,forms=Forms},source=Value,
        validation="this.setAttribute('disabled','disabled');"}),
    wf:wire(["qi('",Selector,"').checked='true';"]),
    
    MakeAsContentEditable=fun(Editable) ->
        wf:wire(#bind{target=Editable,type=keydown,
            postback=["if((event.metaKey || event.ctrlKey) && event.which === 13){qi('",Store,"').click();};"]}),
        wf:wire(#bind{target=Editable,type=focusout,postback="event.target.innerText=trim(event.target.innerText);"}),
        wf:wire(#bind{target=Editable,type=blur,postback="event.target.innerText=trim(event.target.innerText);"}), % FF fix
        wf:wire("unrich('"++Editable++"');"),
        wf:wire(#jq{target=Editable,property=contentEditable,right="true"})
    end,
    case Target of thread -> MakeAsContentEditable(Topic); _ -> skip end,
    MakeAsContentEditable(Input),
    
    wf:wire(wf:f("qi('~s').addEventListener('change', handleFileSelect, false);",[AddImageInput])),
    Controls=#panel{class= <<"input-controls center">>,body=[
        % submit(Store),
        #button{id=Store,class=[ClassStore,<<"store-button">>],disabled=Disabled,body= <<"Отправить"/utf8>>},
        case Target of
            thread -> #button{class=Class,body= <<"Отмена"/utf8>>,postback=#cancel{target=input,panel=Panel}};
            post ->
                #button{id=Sage,class=[Class,<<"sage-button">>],body= <<"Сажи"/utf8>>,
                    postback=#render_event{target=input,event={sage,true,Sage}},value=false}
        end,
        #button{id=AddImage,class=Class,body= <<"Картинка"/utf8>>,onclick=wf:jse(wf:f("qi('~s').click();",[AddImageInput]))},
        #input{id=AddImageInput,type=file}
        ]},
    
    ImageControls=#panel{class= <<"image-manage">>,body=[
        #label{for=Selector,class=["control-thumb",l,sea,checked],body= <<"Формат"/utf8>>}
    ]},
    III=[
        #input{id=Selector,type= <<"checkbox">>,class= <<"input-selector">>,postback=?UNDEF},
        case Target of thread -> Controls; _ -> [] end,
        #panel{class= <<"post-content">>,body=[
            #panel{class= <<"post-flash">>,body=[
                image_panel(Image,empty,false,?UNDEF,?UNDEF,[],?UNDEF,?UNDEF,ImageControls),
                case Target of
                    thread -> #panel{id=Topic,class= <<"post-topic single-line">>,
                        data_fields=[{placeholder,<<"С чем пожаловал, ананас?"/utf8>>}]};
                    post -> [] end,
                #panel{id=Input,class= <<"post-message">>,data_fields=[{placeholder,<<"Сообщение…"/utf8>>}]}
            ]}
        ]},
        case Target of thread -> []; post -> Controls end
    ],
    wf_tags:emit_tag(<<"div">>, nitro:render(III), [{id,Panel},{class,post}]).

render_action(#event{postback=P,actions=_A,source=S,target=C,type=T,delegate=D,validation=V}) ->
    Element=wf:to_list(C),
    Data=S,
    PostbackBin = wf_event:new(P, Element, D, event, Data, []),
    wf:render(#bind{postback=PostbackBin,target=Element,type=wf:to_binary(T)}).
    
image_panel(Id,Class,IsProgress,W,H,Fields,Url,Type,ImageControls) ->
    ProgressClass=["image-process fl cent",case IsProgress of true -> "visibled"; _ -> [] end],
    #panel{id=Id,class=["post-image",Class],data_fields=Fields,body=[
        ImageControls,
        #link{class= <<"media-link">>,body=[
            #panel{class=ProgressClass,body=wf:jse(erlach_svg:progress(<<"#fe8675">>))},
            case Type of
                bpg ->
                    #panel{class=[media,image,shadowed,hidden],
                        data_fields=[{<<"data-url">>,Url},{<<"data-bpg">>,<<"true">>}],
                        actions=[#wire{actions=["shadowOnLoadBpg('",Id,"');"]}]};
                _ -> #image{class=[media,image,shadowed,hidden],src=Url,
                    data_fields=[{<<"onload">>,<<"shadowOnLoad(this);">>},{<<"onerror">>,<<"shadowOnError(this);">>}]}
            end,
            #canvas{class=[media,image,case IsProgress of true -> []; _ -> hidden end],width=W,height=H}
        ]}
    ]}.
