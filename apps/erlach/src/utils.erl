-module(utils).
-author('andy').
-compile(export_all).

-include_lib("n2o/include/wf.hrl").

-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/feed.hrl").
-include_lib("kvs/include/acl.hrl").
-include_lib("kvs/include/comment.hrl").
-include_lib("kvs/include/entry.hrl").
-include_lib("kvs/include/subscription.hrl").

-include_lib("db/include/post.hrl").
-include_lib("db/include/thread.hrl").
-include_lib("db/include/board.hrl").
-include_lib("db/include/user.hrl").
-include_lib("db/include/attachment.hrl").
-include_lib("db/include/token.hrl").

to_digit(N) when N < 10 -> $0 + N;
to_digit(N) -> $a + N-10.


hex_id(Id) when is_integer(Id) -> integer_to_list(Id,36);
hex_id({Atom,Id}) when is_atom(Atom) -> [atom_to_list(Atom),"-",hex_id(Id)].

hex_id_lower(Id) -> string:to_lower(lists:flatten(hex_id(Id))).

to_hex(Int) when is_integer(Int) -> integer_to_list(Int,36);
to_hex([]) -> [];
to_hex(Bin) when is_binary(Bin) -> to_hex(binary_to_list(Bin));
to_hex([H|T]) -> [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

random() -> list_to_binary(to_hex(crypto:rand_bytes(16))).
utc_random() -> utc_suffix(to_hex(crypto:rand_bytes(9))).

utc_suffix(Suffix) ->
    Now = {_, _, Micro} = erlang:now(), % uniqueness is used.
    Nowish = calendar:now_to_universal_time(Now),
    Nowsecs = calendar:datetime_to_gregorian_seconds(Nowish),
    Then = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Prefix = io_lib:format("~14.16.0b", [(Nowsecs - Then) * 1000000 + Micro]),
    list_to_binary(Prefix ++ Suffix).

% 64 bytes (node:8, date:14, random:42)
node_utc_random() -> <<(list_to_binary(to_hex(crypto:hash(md5, atom_to_list(node()))))):8/binary, (utc_suffix(to_hex(crypto:rand_bytes(21))))/binary>>.

init_db() ->
    Access = access:default_access(board),
    GAccess = access:default_access(group),
    
    db_group:new(<<"SPECIAL"/utf8>>,<<"Специальное"/utf8>>,GAccess),
    db_group:new(<<"CONNECT"/utf8>>,<<"Общайся"/utf8>>,GAccess),
    db_group:new(<<"CLOSURE"/utf8>>,<<"В одиночестве"/utf8>>,GAccess),
    db_group:new(<<"CULTURE"/utf8>>,<<"Субкультуры"/utf8>>,GAccess),
    db_group:new(<<"OPEN AIR"/utf8>>,<<"На природе и в городе"/utf8>>,GAccess),
    db_group:new(<<"IMAGINE"/utf8>>,<<"Воображение"/utf8>>,GAccess),
    db_group:new(<<"GIRL"/utf8>>,<<"Девушкам"/utf8>>,GAccess),
    db_group:new(<<"MAN"/utf8>>,<<"Мужикам"/utf8>>,GAccess),
    db_group:new(<<"TEEN"/utf8>>,<<"Подросткам"/utf8>>,GAccess),
    db_group:new(<<"GEEK"/utf8>>,<<"Увлеченным"/utf8>>,GAccess),
    
    % {,<<""/utf8>>}
    
    % SPECIAL
    db_board:new(<<"e"/utf8>>, <<"Оффициальный Эрлачъ"/utf8>>, <<"Зашкваренная борда Эрлача"/utf8>>, 1, 
        [{news,<<"Новости"/utf8>>},{updates,<<"Обновления"/utf8>>},{rules,<<"Правила"/utf8>>}], false, Access),
    db_board:new(<<"r"/utf8>>, <<"Реквесты"/utf8>>, <<"Борда реквестов"/utf8>>, 1,
        [{access,<<"Доступ"/utf8>>},{erlach,<<"Эрлач"/utf8>>},{caterory,<<"Категории"/utf8>>}], false, Access),
    db_board:new(<<"b"/utf8>>, <<"/Б/ред"/utf8>>, <<"Здарова, снова"/utf8>>, 1, [], false, Access),
    db_board:new(<<"br"/utf8>>, <<"Барахолка"/utf8>>, <<"Барыгам и торгашам посвящается"/utf8>>, 1,
        [{sell,<<"Продам"/utf8>>},{buy,<<"Куплю"/utf8>>}], false, Access),
    
    % CONNECT
    db_board:new(<<"bg"/utf8>>, <<"Настольные игры"/utf8>>, <<""/utf8>>, 2,
        [{mahjong,<<"Маджонг"/utf8>>},{anticafe,<<"Антикафе"/utf8>>},{erotic,<<"Эротические"/utf8>>}], false, Access),
    db_board:new(<<"soc"/utf8>>, <<"Общение"/utf8>>, <<""/utf8>>, 2,
        [{'check-it-out',<<"Зацени"/utf8>>},{party,<<"Тусовки"/utf8>>}], false, Access),
    db_board:new(<<"lve"/utf8>>, <<"Любовь и отношения"/utf8>>, <<""/utf8>>, 2,
        [{virgin,<<"Девственница"/utf8>>},{'long-term',<<"ЛТР"/utf8>>},{pickup,<<"Пикап"/utf8>>}], false, Access),
    
    % CLOSURE
    db_board:new(<<"bo"/utf8>>, <<"Книги"/utf8>>, <<""/utf8>>, 3,
        [{social,<<"Социальное"/utf8>>},{fantastic,<<"Фантастика"/utf8>>},{science,<<"Научные"/utf8>>}], false, Access),
    db_board:new(<<"mov"/utf8>>, <<"Фильмы"/utf8>>, <<""/utf8>>, 3,
        [{comedy,<<"Комедии"/utf8>>},{horror,<<"Ужасы"/utf8>>},{arthouse,<<"Артхаус"/utf8>>}], false, Access),
    db_board:new(<<"c"/utf8>>, <<"Комиксы и мультфильмы"/utf8>>, <<""/utf8>>, 3, [], false, Access),
    db_board:new(<<"mu"/utf8>>, <<"Музыка"/utf8>>, <<""/utf8>>, 3,
        [{pop,<<"Попса"/utf8>>},{underground,<<"Дети подземелья"/utf8>>},{metal,<<"Метал"/utf8>>},
        {rap,<<"Рэп"/utf8>>},{classic,<<"Классическая"/utf8>>},{lossless,<<"Высокое качество"/utf8>>},
        {hifi,<<"Hi-Fi системы"/utf8>>}], false, Access),
    db_board:new(<<"smo"/utf8>>, <<"Курение"/utf8>>, <<""/utf8>>, 3,
        [{hate,<<"Ненависть"/utf8>>}], false, Access),
    
    % CULTURE
    db_board:new(<<"jp"/utf8>>, <<"Японская культура"/utf8>>, <<""/utf8>>, 4,
        [{manga,<<"Манга"/utf8>>},{anime,<<"Аниме"/utf8>>}], false, Access),
    db_board:new(<<"fet"/utf8>>, <<"Фетиш"/utf8>>, <<""/utf8>>, 4, [], false, Access),
    db_board:new(<<"hc"/utf8>>, <<"Хардкор"/utf8>>, <<""/utf8>>, 4,
        [{anal,<<"Анал"/utf8>>},{bdsm,<<"БДСМ"/utf8>>}], false, Access),
    db_board:new(<<"hk"/utf8>>, <<"Хакеры"/utf8>>, <<""/utf8>>, 4,
        [{white,white},{gray,gray},{black,black},{coolhacker,<<"Кулхакеры"/utf8>>}], false, Access),
    
    % OPEN AIR
    db_board:new(<<"bi"/utf8>>, <<"Велосипеды"/utf8>>, <<""/utf8>>, 5,
        [{dirt,<<"Дёрт"/utf8>>},{downhill,<<"Даунхилл"/utf8>>},{cross,<<"Кросс"/utf8>>},
        {matras,<<"Матрас"/utf8>>}], false, Access),
    db_board:new(<<"out"/utf8>>, <<"Активный отдых"/utf8>>, <<""/utf8>>, 5,
        [{sky,<<"В небе"/utf8>>},{ground,<<"На земле"/utf8>>},{water,<<"В воде"/utf8>>}], false, Access),
    db_board:new(<<"trv"/utf8>>, <<"Путешествия и отдых"/utf8>>, <<""/utf8>>, 5,
        [{international,<<"За бугор"/utf8>>},{intercity,<<"Внутри страны"/utf8>>}], false, Access),
    
    % CREATIVE
    db_board:new(<<"p"/utf8>>, <<"Фотография"/utf8>>, <<""/utf8>>, 6,
        [{film,<<"Плёнка"/utf8>>},{light,<<"Свет"/utf8>>},{camera,<<"Камера"/utf8>>},{models,<<"Модели"/utf8>>},
        {locations,<<"Локации"/utf8>>}], false, Access),
    db_board:new(<<"pa"/utf8>>, <<"Живопись"/utf8>>, <<""/utf8>>, 6, [], false, Access),
    db_board:new(<<"wrk"/utf8>>, <<"Работа и карьера"/utf8>>, <<""/utf8>>, 6, [], false, Access),
    db_board:new(<<"diy"/utf8>>, <<"Хобби"/utf8>>, <<""/utf8>>, 6, [], false, Access),
    db_board:new(<<"izd"/utf8>>, <<"Графомания"/utf8>>, <<""/utf8>>, 6, [], false, Access),
    db_board:new(<<"mus"/utf8>>, <<"Музыканты"/utf8>>, <<""/utf8>>, 6, [], false, Access),
    
    % GIRL
    % как заниматься сексом и не прослыть шлюхой
    % как правильно делать минет
    
    db_board:new(<<"bt"/utf8>>, <<"Красота"/utf8>>, <<""/utf8>>, 7,
        [{makeup,<<"Мэйкап"/utf8>>},{epilation,<<"Эпиляция"/utf8>>},{spa,<<"Спа"/utf8>>}], false, Access),
    db_board:new(<<"int"/utf8>>, <<"Интимчик"/utf8>>, <<"Близкое общение и секс"/utf8>>, 7,
        [{'first-time',<<"Первый раз"/utf8>>},{he,<<"Он"/utf8>>},{oral,<<"Минет"/utf8>>},{anal,<<"Анал"/utf8>>}], false, Access),
    db_board:new(<<"di"/utf8>>, <<"Столовая"/utf8>>, <<""/utf8>>, 7,
        [{morning,<<"Завтрак"/utf8>>},{'for-him',<<"Для него"/utf8>>},{party,<<"Вечеринка"/utf8>>},
        {dinner,<<"Ужин"/utf8>>}], false, Access),
    db_board:new(<<"fiz"/utf8>>, <<"Физкультура"/utf8>>, <<""/utf8>>, 7,
        [{ass,<<"Попа"/utf8>>},{tits,<<"Грудь"/utf8>>},{food,<<"Питание"/utf8>>}], false, Access),
    db_board:new(<<"tv"/utf8>>, <<"Сериалы"/utf8>>, <<""/utf8>>, 7, [], false, Access),
    db_board:new(<<"fa"/utf8>>, <<"Мода и стиль"/utf8>>, <<""/utf8>>, 7,
        [{shoping,<<"Шопинг"/utf8>>},{shoes,<<"Обувь"/utf8>>}], false, Access),
    db_board:new(<<"psy"/utf8>>, <<"Психология"/utf8>>, <<""/utf8>>, 7, [], false, Access),
    
    % MAN
    db_board:new(<<"g"/utf8>>, <<"Девушки"/utf8>>, <<""/utf8>>, 8,
        [{cute,<<"Милашки"/utf8>>},{selfie,<<"Селфи"/utf8>>},{tp,<<"ТП"/utf8>>}], false, Access),
    db_board:new(<<"sex"/utf8>>, <<"Секс"/utf8>>, <<""/utf8>>, 8, [], false, Access),
    db_board:new(<<"au"/utf8>>, <<"Автомобили"/utf8>>, <<""/utf8>>, 8,
        [{tuning,<<"Тюнинг"/utf8>>},{taz,<<"Тазы"/utf8>>},{germany,<<"Немцы"/utf8>>},
        {japan,<<"Японки"/utf8>>}], false, Access),
    db_board:new(<<"sp"/utf8>>, <<"Спорт"/utf8>>, <<""/utf8>>, 8,
        [{yard,<<"Двор"/utf8>>},{gym,<<"Качалка"/utf8>>},{food,<<"Питание"/utf8>>},
        {program,<<"Программы"/utf8>>}], false, Access),
    db_board:new(<<"ft"/utf8>>, <<"Футбол"/utf8>>, <<""/utf8>>, 8, [], false, Access),
    db_board:new(<<"mo"/utf8>>, <<"Мотоциклы"/utf8>>, <<""/utf8>>, 8, [], false, Access),
    db_board:new(<<"dom"/utf8>>, <<"Домострой"/utf8>>, <<""/utf8>>, 8,
        [{cottage,<<"Дом"/utf8>>},{flat,<<"Квартира"/utf8>>}], false, Access),
    db_board:new(<<"tr"/utf8>>, <<"Транспорт и авиация"/utf8>>, <<""/utf8>>, 8,
        [{plane,<<"Самолёты"/utf8>>}], false, Access),
    db_board:new(<<"w"/utf8>>, <<"Оружие"/utf8>>, <<""/utf8>>, 8, [], false, Access),
    db_board:new(<<"h"/utf8>>, <<"Охота"/utf8>>, <<""/utf8>>, 8, [], false, Access),
    db_board:new(<<"wm"/utf8>>, <<"Военная техника"/utf8>>, <<""/utf8>>, 8, [], false, Access),
    db_board:new(<<"ne"/utf8>>, <<"Животные и природа"/utf8>>, <<""/utf8>>, 8, [], false, Access),
    db_board:new(<<"sci"/utf8>>, <<"Наука"/utf8>>, <<""/utf8>>, 8, [], false, Access),
    db_board:new(<<"spc"/utf8>>, <<"Космос и астрономия"/utf8>>, <<""/utf8>>, 8,
        [{astrograp,<<"Астрограф"/utf8>>},{telescope,<<"Телескоп"/utf8>>},{observations,<<"Наблюдения"/utf8>>}], false, Access),
    
    % TEEN
    db_board:new(<<"dr"/utf8>>, <<"Дневнички"/utf8>>, <<""/utf8>>, 9,
        [{school,<<"Школа"/utf8>>},{'cool-story',<<"Кулстори"/utf8>>},{institute,<<"Институт"/utf8>>},{relationships,<<"Отношения"/utf8>>},
        {hate,<<"Ненависть"/utf8>>}], false, Access),
    db_board:new(<<"vg"/utf8>>, <<"Видеоигры"/utf8>>, <<""/utf8>>, 9,
        [{peka,<<"Пека"/utf8>>},{console,<<"Консоль"/utf8>>},{rpg,<<"РПГ"/utf8>>}], false, Access),
    db_board:new(<<"un"/utf8>>, <<"Образование"/utf8>>, <<""/utf8>>, 9, [], false, Access),
    db_board:new(<<"ph"/utf8>>, <<"Философия"/utf8>>, <<""/utf8>>, 9, [], false, Access),
    db_board:new(<<"wp"/utf8>>, <<"Обои и Hi-resolution"/utf8>>, <<""/utf8>>, 9,
        [{screenshots,<<"Скриншоты"/utf8>>},{desktop,<<"Рабочий стол"/utf8>>},
        {sky,<<"Ночное небо"/utf8>>},{deepsky,<<"Глубокий космос"/utf8>>},{macro,<<"Макро"/utf8>>}], false, Access),
    db_board:new(<<"r34"/utf8>>, <<"Правило 34"/utf8>>, <<""/utf8>>, 9, [], false, Access),
    
    % GEEK
    db_board:new(<<"cc"/utf8>>, <<"Криптовалюта"/utf8>>, <<""/utf8>>, 10, [], false, Access),
    db_board:new(<<"gd"/utf8>>, <<"Геймдев"/utf8>>, <<""/utf8>>, 10, [], false, Access),
    db_board:new(<<"hw"/utf8>>, <<"Железо"/utf8>>, <<""/utf8>>, 10, [], false, Access),
    db_board:new(<<"mb"/utf8>>, <<"Мобилы и приложения"/utf8>>, <<""/utf8>>, 10, [], false, Access),
    db_board:new(<<"pr"/utf8>>, <<"Программирование"/utf8>>, <<""/utf8>>, 10, [], false, Access),
    db_board:new(<<"fp"/utf8>>, <<"Функциональное программирование"/utf8>>, <<""/utf8>>, 10,
        [{erlang,<<"Erlang"/utf8>>},{haskell,<<"Haskell"/utf8>>},{clojure,<<"Clojure"/utf8>>}], false, Access),
    db_board:new(<<"ra"/utf8>>, <<"Радиотехника"/utf8>>, <<""/utf8>>, 10, [], false, Access),
    db_board:new(<<"s"/utf8>>, <<"Программы"/utf8>>, <<""/utf8>>, 10, [], false, Access),
    db_board:new(<<"t"/utf8>>, <<"Техника"/utf8>>, <<""/utf8>>, 10, [], false, Access),
    db_board:new(<<"wb"/utf8>>, <<"Web девелопинг"/utf8>>, <<""/utf8>>, 10, [], false, Access),
    
    qs:init().


add_name(Uid,Name) ->
    kvs:add(#name{
    id=kvs:next_id(name,1),
    feed_id={name,Uid},
    displayname=Name,
    created=erlang:now()
    }).
    
set_ba(Id,Access) ->
    {ok, Board} = kvs:get(board,Id),
    kvs:put(Board#board{access=Access}).
    

backup_table_list() ->
    [access,acl,attachment,board,comment,entry,feed,id_seq,name,post,subscription,thread,token,user3].
backup_file(Table) when is_atom(Table) ->
    "/tmp/backup-" ++ atom_to_list(Table) ++ ".eb1".

backup_ri(access) ->        { record_info(fields, access),      #access{} };
backup_ri(acl) ->           { record_info(fields, acl),         #acl{} };
backup_ri(attachment) ->    { record_info(fields, attachment),  #attachment{} };
backup_ri(board) ->         { record_info(fields, board),       #board{} };
backup_ri(comment) ->       { record_info(fields, comment),     #comment{} };
backup_ri(entry) ->         { record_info(fields, entry),       #entry{} };
backup_ri(feed) ->          { record_info(fields, feed),        #feed{} };
backup_ri(id_seq) ->        { record_info(fields, id_seq),      #id_seq{} };
backup_ri(name) ->          { record_info(fields, name),        #name{} };
backup_ri(post) ->          { record_info(fields, post),        #post{} };
backup_ri(subscription) ->  { record_info(fields, subscription),#subscription{} };
backup_ri(thread) ->        { record_info(fields, thread),      #thread{} };
backup_ri(token) ->         { record_info(fields, token),       #token{} };
backup_ri(user3) ->         { record_info(fields, user3),       #user3{} }.


backup() ->
    lists:foldl(fun(E,Acc) ->
        R = backup(E),
        [{E,R}|Acc]
        end,[],backup_table_list()).

backup(Table) when is_atom(Table) ->
    {Fields, _Record} = backup_ri(Table),
    DataList = kvs:all(Table),
    Data={Fields,DataList},
    file:write_file(backup_file(Table), term_to_binary(Data)).

restore() ->
    lists:foldl(fun(E,Acc) ->
        R = restore(E),
        [{E,R}|Acc]
        end,[],backup_table_list()).

restore(Table) when is_atom(Table) ->
    {ok, Data} = file:read_file(backup_file(Table)),
    {Fields,DataList} = binary_to_term(Data),
    lists:foldl(fun(NewRecord,Acc) ->
        Merged = merge_record(Table,NewRecord,Fields),
        kvs:put(Merged),
        [Merged|Acc]
        end,[],DataList),
    ok.

merge_record(Table,NewRecord,Fields) ->
    {NewFields, VoidRecord} = backup_ri(Table),
    {_,Merged} = lists:foldl(fun(F,{Idx,Record}) ->
        case index_of(F,NewFields,1) of
            not_found -> {Idx+1,Record};
            VoidIdx -> {Idx+1,setelement(VoidIdx+1,Record,element(Idx,NewRecord))}
        end end,{2,VoidRecord},Fields),
    Merged.

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

% <a(?!\ ?href\=\"https\:\/\/erlach\.ru\/[^\"]*\")[^>]*>([^<]*)<\/a>
link_regexp() -> <<"<a(?!\\ ?href\\=\\\"https\\:\\/\\/erlach\\.ru\\/[^\\\"]*\\\")[^>]*>([^<]*)<\\/a>">>.
img_regexp() -> <<"<img[^>]+\\>">>.
html_message(#post{markup=Markup,message=Message,view=View}) ->
    AsIs = proplists:get_value(as_is,View),
    Escaped =case Markup of
        markdown ->
            MD = guard:strip(markdown:conv_utf8(Message)),
            if AsIs -> MD; true ->
                re:replace(MD,<<"(",(link_regexp())/binary,")|(",(img_regexp())/binary,")">>,<<>>,[global,{return,binary}])
            end;
        _ -> guard:html_escape(Message)
    end,
    re:replace(Escaped,<<"&gt;&gt;[0-9]+\\ ">>,<<"<a href='#'>&</a>">>,[global,{return,binary}]);
html_message(_) -> <<>>.


expired(Timestamp,TTL) ->
    NowSec = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    ExpireSec = calendar:datetime_to_gregorian_seconds(calendar:now_to_local_time(Timestamp)) + TTL,
    NowSec > ExpireSec.

online() -> [X||X<-qlc:e(gproc:table()),element(1,X)=={p,l,broadcast}].
online2() -> ets:select(gproc,[{{{{'$1','$2','$3'},'$4'},'$5','$6'},[{'=:=',broadcast,'$3'},{'=:=',p,'$1'},{'=:=',l,'$2'}],['$$']}]).
online3() -> gproc:select({local,all},[{'_',[],['$$']}]).

-include_lib("stdlib/include/ms_transform.hrl").
online4() -> ets:select(gproc,ets:fun2ms(fun({{{_,_,broadcast},_},D,E}) -> {D,E} end)).
online5() -> ets:select_count(gproc,ets:fun2ms(fun({{{_,_,broadcast},_},D,E}) -> true end)).
