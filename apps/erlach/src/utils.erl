-module(utils).
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


hex_id({Atom,Id}) -> [atom_to_list(Atom),"-",integer_to_list(Id,36)].
hex_id_lower({Atom,Id}) -> string:to_lower(lists:flatten([atom_to_list(Atom),"-",integer_to_list(Id,36)])).

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
    add_new_board(e, <<"Erlach official">>, <<"Официальная зашкваренная борда эрлача"/utf8>>, 1, false),
    add_new_board(r, <<"Requests">>, <<"Борда реквестов"/utf8>>, 1, false),
    add_new_board(p, <<"Photo">>, <<"Фотография"/utf8>>, 1, false),
    add_new_board(g, <<"Girls">>, <<"Девушки"/utf8>>, 1, false),
    add_new_board(c, <<"Cinema">>, <<"Фильмы"/utf8>>, 1, false),
    add_new_board(ps, <<"Psychology">>, <<"Психология"/utf8>>, 1, false),
    add_new_board(po, <<"Poetry">>, <<"Стихи"/utf8>>, 1, false),
    add_new_board(pr, <<"Prank">>, <<"Пранк"/utf8>>, 1, false),
    add_new_board(ee, <<"Electrical engineering">>, <<"Электротехника"/utf8>>, 1, false).

set_access() ->
    utils:set_ba(1,[{anonymous,read,blog},{anonymous,read,post},
        {registered,read,blog},{registered,read,post},{registered,read,message},
        {private,read,blog}]),
    utils:set_ba(2,[{anonymous,read,blog},{anonymous,write,post},{anonymous,write,message},
        {registered,read,blog},{registered,write,post},{registered,write,message},
        {private,read,blog},{private,write,message},{private,write,request}]),
    utils:set_ba(6,[{private,read,blog},{private,write,message},{private,write,request}]).
    
clear_access() ->
    utils:set_ba(1,[]),
    utils:set_ba(2,[]),
    utils:set_ba(6,[]).

add_new_board(Uri, Name, Description, GroupID, Hidden) ->
    kvs:add( #board {
        id=kvs:next_id(board, 1),
        created=erlang:now(),
        uri=Uri,
        name=Name,
        user=0,
        description=Description,
        hidden=Hidden,
        feed_id={board, GroupID},
        access=access:default_access(board)
        }).

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
% html_message(#post{markup=Markup,message=Message}) -> Message;
html_message(#post{markup=Markup,message=Message,view=View}) ->
    AsIs = proplists:get_value(as_is,View),
    case Markup of
        markdown ->
            MD = guard:strip(markdown:conv_utf8(Message)),
            if AsIs -> MD; true ->
                re:replace(MD,<<"(",(link_regexp())/binary,")|(",(img_regexp())/binary,")">>,<<>>,[global,{return,binary}])
                % re:replace(MD2,img_regexp(),<<>>,[global,{return,binary}])
            end;
        _ -> guard:html_escape(Message)
    end;
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