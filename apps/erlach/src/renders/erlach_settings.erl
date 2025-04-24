-module(erlach_settings).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").

render(settings=Panel) ->
    #panel{id=Panel,class=fl,body=#panel{class= <<"settings-controls">>,body=[
    
        #panel{class= <<"setting-option">>,body=[
            #span{class= <<"opt-title opt-name">>,body=?TR(<<"Имя"/utf8>>,<<"Name"/utf8>>)},
            #textbox{id= <<"option-nickname">>,maxlength=wf:config(erlach,nickname_length,14),
                placeholder= <<"Аноним"/utf8>>,autofocus=autofocus,disabled=true}
        ]},
        #panel{class= <<"setting-option">>,body=[
            #span{class= <<"opt-title">>,body= <<"Language"/utf8>>},
            #dropdown{id= <<"option-language">>,title= <<"Select u\\'r language">>,
                options=[
                    #option{value= <<"ru">>,label= <<"Russian"/utf8>>,selected=true},
                    #option{value= <<"en">>,label= <<"English"/utf8>>,selected=false}
                ]}
        ]},
        #panel{class= <<"setting-option">>,body=[
            #checkbox{id= <<"option-fullwidth">>,body=?TR(
                <<"Растянуть по ширине окна"/utf8>>,
                <<"Fullwidth page mode">>)}
        ]},
        #panel{class= <<"setting-option">>,body=[
            #checkbox{id= <<"option-no-cacach">>,body=?TR(
                <<"Не отправлять данные о новых тредах на 1chan.ca"/utf8>>,
                <<"Don\\'t send data of the new thread to 1chan.ca"/utf8>>)}
        ]},
        #panel{class= <<"setting-option">>,body=[
            #checkbox{id= <<"option-no-soundcloud">>,body=?TR(
                <<"Не загружать плеер с soundcloud.com"/utf8>>,
                <<"Don\\'t load audio player from soundcloud.com"/utf8>>)}
        ]},
        #panel{class= <<"setting-option">>,body=[
            #checkbox{id= <<"option-snow">>,body=?TR(
                <<"Снег"/utf8>>,
                <<"Snow"/utf8>>)}
        ]},
        #panel{class= <<"setting-option">>,body=[
            #span{class= <<"opt-title">>,body=?TR(<<"Тема"/utf8>>,<<"Theme"/utf8>>)},
            #dropdown{id= <<"option-theme">>,title=?TR(<<"Выбери тему"/utf8>>,<<"Select theme"/utf8>>),
                options=[
                    #option{value= <<"dark">>,label= <<"Северное сияние / Northern Lights"/utf8>>,selected=false},
                    #option{value= <<"light">>,label= <<"Нейтронная звезда / Neutron Star"/utf8>>,selected=true}
                ]}
        ]},
        
        #button{body=?TR(<<"Готово"/utf8>>,<<"Done"/utf8>>),postback=#view{render=erlach_settings,target=window,option=hide}}
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