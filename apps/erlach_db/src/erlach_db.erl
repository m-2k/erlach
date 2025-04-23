-module(erlach_db).
-author('Andy').
-behaviour(application).
-behaviour(supervisor).

-include_lib("kvs/include/metainfo.hrl").
-include_lib("erlach_db/include/erlach_db.hrl").
-compile(export_all).

start(_,_) -> start_link().
stop(_) -> ok.
start_link() -> supervisor:start_link({local,?MODULE},?MODULE,[]).
init([]) -> {ok, {{one_for_one,5,10}, []} }.

metainfo() ->
    #schema{name=kvs,tables=[
        #table{name=party,container=feed,fields=record_info(fields,party),keys=[]},
        #table{name=board,container=feed,fields=record_info(fields,board),keys=[urn]},
        #table{name=post,container=feed,fields=record_info(fields,post),keys=[]},
        #table{name=attachment,container=feed,fields=record_info(fields,attachment),keys=[]}
        ]}.

init() ->
    Group=fun(Name,Desc,Boards) ->
        Gid=kvs:next_id(party,1),
        kvs:add(#party{id=Gid,created=erlang:system_time(),
            name=Name,desc=Desc}),
        lists:foreach(fun({BoardUrn,BoardName,BoardDesc}) ->
            kvs:add(#board{id=kvs:next_id(board,1),created=erlang:system_time(),
                feed_id={board,Gid},name=BoardName,urn=BoardUrn,desc=BoardDesc})
            end,Boards)
        end,

    Group(<<"SPECIAL"/utf8>>,<<"Специальное"/utf8>>,[
        {<<"e"/utf8>>, <<"Оффициальный Эрлачъ"/utf8>>, <<"Зашкваренная борда Эрлача"/utf8>>},
        {<<"r"/utf8>>, <<"Реквесты"/utf8>>, <<"Борда реквестов"/utf8>>},
        {<<"b"/utf8>>, <<"/Б/ред"/utf8>>, <<"Здарова, снова"/utf8>>},
        {<<"br"/utf8>>, <<"Барахолка"/utf8>>, <<"Барыгам и торгашам"/utf8>>}
    ]),
    Group(<<"CONNECT"/utf8>>,<<"Общайся"/utf8>>,[
        {<<"bg"/utf8>>, <<"Настольные игры"/utf8>>, <<""/utf8>>},
        {<<"soc"/utf8>>, <<"Общение"/utf8>>, <<""/utf8>>},
        {<<"lv"/utf8>>, <<"Любовь и отношения"/utf8>>, <<""/utf8>>}
    ]),
    Group(<<"OPEN AIR"/utf8>>,<<"На природе и в городе"/utf8>>,[
        {<<"bi"/utf8>>, <<"Велосипеды"/utf8>>, <<""/utf8>>},
        {<<"ac"/utf8>>, <<"Активный отдых"/utf8>>, <<""/utf8>>},
        {<<"tr"/utf8>>, <<"Путешествия и отдых"/utf8>>, <<""/utf8>>}
    ]),
    Group(<<"IMAGINE"/utf8>>,<<"Воображение"/utf8>>,[
        {<<"p"/utf8>>, <<"Фотография"/utf8>>, <<""/utf8>>},
        {<<"pa"/utf8>>, <<"Живопись"/utf8>>, <<""/utf8>>},
        {<<"wr"/utf8>>, <<"Работа и карьера"/utf8>>, <<""/utf8>>},
        {<<"dy"/utf8>>, <<"Хобби"/utf8>>, <<""/utf8>>},
        {<<"iz"/utf8>>, <<"Графомания"/utf8>>, <<""/utf8>>},
        {<<"ms"/utf8>>, <<"Музыканты"/utf8>>, <<""/utf8>>}
    ]),
    Group(<<"TEEN"/utf8>>,<<"Подросткам"/utf8>>,[
        {<<"dr"/utf8>>, <<"Дневнички"/utf8>>, <<""/utf8>>},
        {<<"vg"/utf8>>, <<"Видеоигры"/utf8>>, <<""/utf8>>},
        {<<"un"/utf8>>, <<"Образование"/utf8>>, <<""/utf8>>},
        {<<"ph"/utf8>>, <<"Философия"/utf8>>, <<""/utf8>>},
        {<<"wp"/utf8>>, <<"Обои и Hi-resolution"/utf8>>, <<""/utf8>>},
        {<<"34"/utf8>>, <<"Правило 34"/utf8>>, <<""/utf8>>}
    ]),
    Group(<<"GIRL"/utf8>>,<<"Девушкам"/utf8>>,[
        {<<"bt"/utf8>>, <<"Красота"/utf8>>, <<""/utf8>>},
        {<<"in"/utf8>>, <<"Интимчик"/utf8>>, <<"Близкое общение и секс"/utf8>>},
        {<<"di"/utf8>>, <<"Столовая"/utf8>>, <<""/utf8>>},
        {<<"fz"/utf8>>, <<"Физкультура"/utf8>>, <<""/utf8>>},
        {<<"tv"/utf8>>, <<"Сериалы"/utf8>>, <<""/utf8>>},
        {<<"fa"/utf8>>, <<"Мода и стиль"/utf8>>, <<""/utf8>>},
        {<<"psy"/utf8>>, <<"Психология"/utf8>>, <<""/utf8>>}
    ]),
    Group(<<"MAN"/utf8>>,<<"Мужикам"/utf8>>,[
        {<<"g"/utf8>>, <<"Девушки"/utf8>>, <<""/utf8>>},
        {<<"sx"/utf8>>, <<"Секс"/utf8>>, <<""/utf8>>},
        {<<"au"/utf8>>, <<"Автомобили"/utf8>>, <<""/utf8>>},
        {<<"sp"/utf8>>, <<"Спорт"/utf8>>, <<""/utf8>>},
        % {<<"ft"/utf8>>, <<"Футбол"/utf8>>, <<""/utf8>>},
        {<<"mt"/utf8>>, <<"Мотоциклы"/utf8>>, <<""/utf8>>},
        {<<"dom"/utf8>>, <<"Домострой"/utf8>>, <<""/utf8>>},
        {<<"av"/utf8>>, <<"Авиация"/utf8>>, <<""/utf8>>},
        {<<"w"/utf8>>, <<"Оружие"/utf8>>, <<""/utf8>>},
        {<<"h"/utf8>>, <<"Охота"/utf8>>, <<""/utf8>>},
        {<<"wm"/utf8>>, <<"Военная техника"/utf8>>, <<""/utf8>>},
        {<<"sci"/utf8>>, <<"Наука"/utf8>>, <<""/utf8>>},
        {<<"spc"/utf8>>, <<"Космос и астрономия"/utf8>>, <<""/utf8>>}
    ]),
    Group(<<"CULTURE"/utf8>>,<<"Субкультуры"/utf8>>,[
        {<<"jp"/utf8>>, <<"Японская культура"/utf8>>, <<""/utf8>>},
        {<<"fe"/utf8>>, <<"Фетиш"/utf8>>, <<""/utf8>>},
        {<<"hk"/utf8>>, <<"Хакеры"/utf8>>, <<""/utf8>>}
    ]),
    Group(<<"CLOSURE"/utf8>>,<<"В одиночестве"/utf8>>,[
        {<<"bo"/utf8>>, <<"Книги"/utf8>>, <<""/utf8>>},
        {<<"mv"/utf8>>, <<"Фильмы"/utf8>>, <<""/utf8>>},
        {<<"c"/utf8>>, <<"Комиксы и мультфильмы"/utf8>>, <<""/utf8>>},
        {<<"mu"/utf8>>, <<"Музыка"/utf8>>, <<""/utf8>>},
        {<<"sm"/utf8>>, <<"Курение"/utf8>>, <<""/utf8>>}
    ]),
    Group(<<"GEEK"/utf8>>,<<"Увлеченным"/utf8>>,[
        {<<"cc"/utf8>>, <<"Криптовалюта"/utf8>>, <<""/utf8>>},
        {<<"gd"/utf8>>, <<"Геймдев"/utf8>>, <<""/utf8>>},
        {<<"hw"/utf8>>, <<"Железо"/utf8>>, <<""/utf8>>},
        {<<"mb"/utf8>>, <<"Мобилы и приложения"/utf8>>, <<""/utf8>>},
        {<<"pr"/utf8>>, <<"Программирование"/utf8>>, <<""/utf8>>},
        {<<"fp"/utf8>>, <<"Функциональное программирование"/utf8>>, <<""/utf8>>},
        {<<"ra"/utf8>>, <<"Радиотехника"/utf8>>, <<""/utf8>>},
        {<<"s"/utf8>>, <<"Софт"/utf8>>, <<""/utf8>>},
        {<<"t"/utf8>>, <<"Техника"/utf8>>, <<""/utf8>>},
        {<<"wd"/utf8>>, <<"Web девелопинг"/utf8>>, <<""/utf8>>}        
    ]),
    ok.

% threads in board 1
populate() ->
    Bid = 1,
    Thread=fun() ->
        Pid=kvs:next_id(post,1),
        kvs:add(#post{type=thread,id=Pid,created=erlang:system_time(),feed_id={thread,Bid},urn=erlach_utils:id_to_urn(Pid),
            name_escaped=wf:to_binary(wf:temp_id()),message_escaped=wf:to_binary(wf:temp_id())})
        end,
    [ Thread() || _ <- lists:seq(1,10) ],
    ok.

% x-counted posts in custom-thread
populate(Tid,Count) when is_binary(Tid) -> populate(erlach_qs:urn_to_id(Tid),Count);
populate(Tid,Count) when is_integer(Tid)->
    {ok,T}=kvs:get(post,Tid),
    Post=fun() ->
        Pid=kvs:next_id(post,1),
        kvs:add(#post{id=Pid,created=erlang:system_time(),feed_id={post,Tid},type=post,
            urn=erlach_qs:id_to_urn(Pid),message_escaped=wf:to_binary(wf:temp_id())})
    end,
    utils:times(Post,Count).