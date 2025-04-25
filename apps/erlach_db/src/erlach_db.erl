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

%% rotation chunk length
limit(_Table, _Key) -> 250000.
%% change schema the table if count more than:
forbid(_Table) -> 5.

fold_check() ->
    case kvs:get(feed,{board,10}) of
        {ok, #feed{top=Top,count=Count}} ->
            kvs:fold(fun(T,Acc) -> [ T | Acc] end,[],board,Top,Count,#iterator.prev);
        _ -> []
    end.

metainfo() ->
    #schema{name=kvs,tables=[
        #table{name=party,container=feed,fields=record_info(fields,party),keys=[]},
        #table{name=board,container=feed,fields=record_info(fields,board),keys=[urn]},
        #table{name=post,container=feed,fields=record_info(fields,post),keys=[]},
        #table{name=attachment,container=feed,fields=record_info(fields,attachment),keys=[hash]},
        #table{name=statistic,fields=record_info(fields,statistic),keys=[]},
        #table{name=bastard,fields=record_info(fields, bastard),keys=[]}
        ]}.

init_db() ->
    0 = kvs:count(board), % guard
    Group=fun(Name,Desc,Boards) ->
        Gid=kvs:next_id(party,1),
        kvs:add(#party{id=Gid,created=erlang:timestamp(),
            name=Name,desc=Desc}),
        lists:foreach(fun({BoardUrn,BoardName,BoardDesc}) ->
            kvs:add(#board{id=kvs:next_id(board,1),created=erlang:timestamp(),
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
    
add_party(N,D,H) -> kvs:add(#party{id=kvs:next_id(party,1),created=erlang:timestamp(),name=N,desc=D,hidden=H}).


int() ->
    {ok,#party{id=P}}=add_party(<<"INTERNATIONAL">>,<<"">>,false),
    add_board(P,<<"~b">>,<<"/B/ullshit">>,<<"Welcome back, bitard">>,false).

set_board_view(BinName,Property,Value) ->
    [#board{}=B]=kvs:index(board,urn,BinName),
    FunUpdate=fun(#board{view=V0}=B0) -> {ok,B0#board{view=spa:setoption(Property,Value,V0)}} end,
    erlach_feeds:update(B,FunUpdate).

show_party(Party,On) when is_boolean(On) ->
    {ok,P}=kvs:get(party,Party),
    kvs:put(P#party{hidden=not On}).
    
    
add_board(rec) -> add_board(1,<<"rec">>,<<"Recursion"/utf8>>,<<"Доска конференции Recursion"/utf8>>,false);


add_board(bdr) -> add_board(8,<<"bdr">>,<<"Бандеровцы"/utf8>>,<<"Для укрофашистов и работников львовского метрополитена"/utf8>>,false);
add_board(stn) -> add_board(8,<<"stn">>,<<"Сталинисты"/utf8>>,<<"Для тех, кто душой в СССР и скучает по Берии и продразверстке"/utf8>>,false);

add_board(fap) -> add_board(11,<<"fapfapfapchan.ga">>,<<"Фапчан"/utf8>>,<<"Оверчан всех порнографических досок галактики"/utf8>>,false);
add_board(rf) -> add_board(9,<<"rf">>,<<"Убежище"/utf8>>,<<"Хиккую как хочу"/utf8>>,true);
add_board(eot) -> add_board(9,<<"eot">>,<<"Есть Одна Тян"/utf8>>,<<"Одна из миллиардов"/utf8>>,false);
add_board(a) -> add_board(8,<<"Аниме"/utf8>>,<<"a">>,<<"Лунная призма, дай мне силу"/utf8>>,false).
add_board(Party,Urn,Name,Desc,Hidden) ->
    kvs:add(#board{id=kvs:next_id(board,1),created=erlang:timestamp(),feed_id={board,Party},
        name=wf:to_binary(Name),urn=wf:to_binary(Urn),desc=wf:to_binary(Desc),hidden=Hidden=:=true}).
    
hide_board(N) -> [#board{}=B]=kvs:index(board,urn,wf:to_binary(N)), kvs:put(B#board{hidden=true}).

set_party_type(Id,Type) -> {ok,P}=kvs:get(party,Id), kvs:put(P#party{type=Type}).

update_record(Table,Id,Field,Value) ->
    {ok,E}=kvs:get(Table,Id),
    erlach_feeds:update(E,fun(E2) -> {ok,setelement(Field,E2,Value)} end).

% updating all schemas of tables
update_schema() -> update_schema(undefined).
update_schema(Default) ->
    [ {N, mnesia:transform_table(N,fun(X) -> new_record(N,X,F,Default) end, F)} || #table{name=N, fields=F} <- (metainfo())#schema.tables ].
new_record(N,R,F,_) when N =:= element(1,R), size(R) =:= length(F) + 1 -> R;
new_record(Name,Record,NewFields,Default) ->
    {_,NR}=lists:foldl(fun(_,{Index,Acc}) ->
        V=case Index > size(Record) of true -> Default; _ -> element(Index,Record) end,
        {Index - 1, [V | Acc]}
        end,{length(NewFields) + 1, []},[ Name | NewFields ]),
    setelement(1,list_to_tuple(NR),Name).
    
ec() -> kvs:add(#party{id=kvs:next_id(party,1),feed_id=comments,created=erlang:timestamp(),name= <<"Comment Services">>,desc= <<"Erlach anonymous comments as service">>,hidden=true}).
ecb() -> kvs:add(#board{id=kvs:next_id(board,1),created=erlang:timestamp(),
                feed_id={board,13},name= <<"GitHub">>,desc= <<"github.io">>,urn= <<"github.io">>}).

% threads in board 1
populate() ->
    Bid = 1,
    Thread=fun() ->
        Pid=kvs:next_id(post,1),
        kvs:add(#post{type=thread,id=Pid,created=erlang:timestamp(),feed_id={thread,Bid},urn=erlach_qs:id_to_urn(Pid),
            name_escaped=wf:to_binary(wf:temp_id()),message_escaped=wf:to_binary(wf:temp_id())})
        end,
    [ Thread() || _ <- lists:seq(1,10) ],
    ok.

% x-counted posts in custom-thread
populate(Tid,Count) when is_binary(Tid) -> populate(erlach_qs:urn_to_id(Tid),Count);
populate(Tid,Count) when is_integer(Tid)->
    {ok,_}=kvs:get(post,Tid),
    lists:map(fun(Num) ->
        Pid=kvs:next_id(post,1),
        P=#post{id=Pid,created=erlang:timestamp(),feed_id={post,Tid},type=post,
            urn=erlach_qs:id_to_urn(Pid),message=wf:to_binary(["test-message-",wf:to_list(Num)])},
        {ok,_}=erlach_feeds:append(P)
    end,lists:seq(1,Count)), ok.
