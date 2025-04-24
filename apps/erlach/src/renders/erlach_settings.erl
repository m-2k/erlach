-module(erlach_settings).
-compile(export_all).
-author('Andy').

-include("erlach.hrl").

render(settings=Panel) ->
    #panel{id=Panel,class=fl,body=#panel{class= <<"settings-controls">>,body=[
    
        #panel{class= <<"setting-option">>,body=[
            #span{class= <<"opt-name">>,body= <<"Имя"/utf8>>},
            #textbox{id= <<"option-nickname">>,maxlength=wf:config(erlach,nickname_length,14),
                placeholder= <<"Аноним"/utf8>>,autofocus=autofocus,disabled=true}
        ]},
        #panel{class= <<"setting-option">>,body=[
            #span{body= <<"Тема"/utf8>>},
            #dropdown{id= <<"option-theme">>,title= <<"Выбери тему"/utf8>>,
                source=[theme],
                options=[
                    #option{value= <<"dark">>,label= <<"Северное сияние"/utf8>>,selected=true},
                    #option{value= <<"light">>,label= <<"Нейтронная звезда"/utf8>>,selected=false}
                ]}
        ]},
        
        #button{body= <<"Готово"/utf8>>,postback=#view{render=erlach_settings,target=window,option=hide}}
    ]},actions=[
        "loadSettings();",
        #bind{target= <<"option-theme">>,type=change,postback= <<"saveSettings();">>}
    ]}.

event(#view{target=window,option=show}) ->
    wf:update(settings,render(settings)),
    wf:wire([
        spa:add_class(settings,active),
        "animate(qi('settings'), 'viewer-open');"
    ]);
event(#view{target=window,option=hide}) ->
    wf:wire([
        "saveSettings();",
        spa:remove_class(settings,active)
    ]);
event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).