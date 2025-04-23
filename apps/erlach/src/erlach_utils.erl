-module(erlach_utils).
-author('andy').

-compile(export_all).

-include_lib("n2o/include/wf.hrl").

-include_lib("erlach_db/include/erlach_db.hrl").
-include("erlach.hrl").

id_to_urn(Id) -> list_to_binary(string:to_lower(integer_to_list(Id,36))).

% Render State
rst() -> erlang:get(rst).
rst(Value) -> erlang:put(rst,Value).
rst_erase() -> rst(#rst{}).

hidden(H) -> R=rst(), rst(R#rst{hidden_elements=R#rst.hidden_elements++[H]}).
hidden() -> R=rst(), H=R#rst.hidden_elements,rst(R#rst{hidden_elements=[]}), H.


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















% ################################
% ################################
% ################################


% unknown(Module,Unknown) -> wf:warning(Module,"Unknown event: ~p from module: ~p", [Unknown,Module]).
% not_allowed(Module,NotAllowed) -> wf:warning(Module,"Not allowed event: ~p from module: ~p", [NotAllowed,Module]).

times(Fun,Count) -> times(Fun,Count,[]).
times(_,0,Acc) -> Acc;
times(Fun,Count,Acc) -> times(Fun,Count-1,[Fun()|Acc]).

escape_line_breaks(<<>>, Acc) -> Acc;
escape_line_breaks(<<U,T/binary>>, Acc) ->
    R = case U of
        $\n ->  <<"<br>">>;
        $\t ->  <<"&nbsp;">>;
        $\r ->  <<>>;
        U when U =< 0 -> <<>>;
        _ -> <<U>>
    end,
    escape_line_breaks(T, <<Acc/binary,R/binary>>).
escape_line_breaks(B) when is_binary(B) -> escape_line_breaks(B, <<>>);
escape_line_breaks(B) -> escape_line_breaks(wf:to_binary(B), <<>>).

js_multiline(<<>>,Acc) -> Acc;
js_multiline(<<H,T/binary>>,Acc) ->
    R=case H of
        $\n -> <<"\\n">>;
        _ -> <<H>>
    end,
    js_multiline(T,<<Acc/binary,R/binary>>).
js_multiline(B) -> js_multiline(B,<<>>).

html_escape(<<>>, Acc) -> Acc;
html_escape(<<U,T/binary>>, Acc) -> % when U =< 62 andalso U >= 0 ->
    R = case U of
        $& ->   <<"&amp;">>;
        $< ->   <<"&lt;">>;
        $> ->   <<"&gt;">>;
        $" ->   <<"&quot;">>;
        $' ->   <<"&#x27;">>;
        $/ ->   <<"&#x2F;">>;
        % $\s ->    <<"&nbsp;">>;
        $\t ->  <<"&nbsp;&nbsp;&nbsp;&nbsp;">>;
        $\n ->  <<"<br/>">>;
        $\r ->  <<>>;
        U when U =< 0 -> <<>>;
        _ -> <<U>>
    end,
    html_escape(T, <<Acc/binary,R/binary>>).

html_escape(B) when is_binary(B) -> html_escape(B, <<>>);
html_escape(B) -> html_escape(wf:to_binary(B), <<>>).

% onclick(Str) -> onclick(Str, erlang:get(state)).
% onclick(Str, #st{allow_js=true}) -> re:replace(Str,"'","\\\\'",[{return,list},global]);
% onclick(Str, #st{allow_js=false}) -> Str.

% hex_id(Int) -> list_to_binary(integer_to_list(Int,36)).
temp_id() -> wf:temp_id().
temp_id(Count) -> [ temp_id() || _ <- lists:seq(1,Count) ].

list_id(R) -> {Scope,Id}=element(#db_element.feed_id,R), id({{scope,Scope},Id}).
list_id(Scope,Id) -> id({{scope,Scope},Id}).

id(A) when is_atom(A) -> id({panel,A});
id(B) when is_binary(B) -> id({panel,B});
id(R) when is_tuple(R) -> id(element(1,R),element(2,R)).
id(Scope,Id) ->
    case erlang:get({temp_id,Scope,Id}) of
        ?UNDEF -> TempId=temp_id(), erlang:put({temp_id,Scope,Id},TempId),TempId;
        TempId -> TempId end.

id_list() ->
    lists:foldl(fun({{temp_id,_,_},_}=V,A) -> [V|A]; (_,A) -> A end,[],erlang:get()).
id_erase() ->
    [ case Key of {temp_id,_,_} -> erlang:erase(Key); _ -> skip end || {Key,_} <- erlang:get() ], ok.

update(_Module,?UNDEF) -> skip;
update(Module,Record) when is_tuple(Record) ->
    wf:info(?M," update 1 ~p ~p ~p",[{element(1,Record),element(2,Record)},self(),id(Record)]),
    wf:update(id(Record),Module:render({element(1,Record),Record},erlang:get(state)));
update(Module,Panel) ->
    wf:info(?M," update 2 ~p ~p",[Panel,self()]),
    wf:update(Panel,Module:render(Panel,erlang:get(state))).

update_element(_Module,?UNDEF,_Hes,_State) -> skip;
update_element(Module,Element,Hes,State) when is_atom(Element) ->
    wf:info(?M," update_element 1 ~p",[Element]),
    wf:update(Element,Module:render_element(Element,Hes,State));
update_element(Module,Element,Hes,State) ->
    wf:info(?M," update_element 2 ~p ~p",[id(Element),self()]),
    wf:update(id(Element),Module:render_element(Element,Hes,State)).



% chapter() -> erlang:get(chapter).
% chapter(Value) -> erlang:put(chapter,Value).

to_s(Term) -> wf:html_encode(wf:f("~p",[Term])).
to_s(Text,Term) -> wf:html_encode(wf:f("~s: ~p",[Text,Term])).



% State helper
action(Action) -> action(Action,erlang:get(state)).
action(Action,S) -> S2=S#st{action=Action},erlang:put(state,S2),S2.

% Route Level Helper
level(#st{route=#route{level=Level}}) -> Level.
level(#st{route=#route{}=R}=S,Level) -> S2=S#st{route=R#route{level=Level}},erlang:put(state,S2),S2.


% [ {"temp34245",return_value_1}, {"temp43245",return_value_2}, ... ]
% Type=erlach_utils:selected_radio([{R1,omission},{R2,test},{R3,table},{R4,matching}]),
selected_radio([{_,Default}|TupleList]) ->
    lists:foldl(fun({Rid,T},Acc) -> case wf:q(Rid) of <<>> -> Acc; _ -> T end end,Default,TupleList).

selecting_radio(Type,List) -> [ case E of Type -> true; _ -> false end || E <- List].

remove_duplicates([])    -> [];
remove_duplicates([H|T]) -> [H | [X || X <- remove_duplicates(T), X /= H]].


% strip left & right whitespaces
strip(Bin) -> re:replace(Bin, <<"(^\\s+)|(\\s+$)">>, <<>>, [global,{return,binary}]).
strip_all(Any) -> binary:replace(wf:to_binary(Any),[<<$\n>>,<<$\r>>,<<$\t>>],<<>>,[global]).

% non_empty(BinList) -> lists:filter(fun(<<>>) -> false; (_) -> true end,BinList).
non_empty(BinList) -> [ Bin || Bin <- BinList, Bin =/= <<>> ].

sublist_random(List,Len) -> % lists:sublist(List,Len).
    randomize(),
    ListLen=length(List),
    Len2=case Len of 0 -> ListLen; _ when Len > ListLen -> ListLen; _ -> Len end,
    {List2,_}=lists:foldl(fun(_,{ListAcc,MapAcc}) ->
        Pos=lists:nth(random:uniform(length(MapAcc)),MapAcc),
        {[ lists:nth(Pos,List)|ListAcc], lists:delete(Pos,MapAcc) }
    end,{[],lists:seq(1,ListLen)},lists:seq(1,Len2)), List2.

list_random(List) -> sublist_random(List,0).

map_tail(_MapFun,[]) -> [];
map_tail(_MapFun,[Item]) -> [Item];
map_tail(MapFun,[Header|Tail]) -> [ Header | MapFun(Tail) ].

lists_find(Fun, [H|T]) -> case Fun(H) of true -> H; false -> lists_find(Fun, T) end.

password_hash(Pass,Salt) ->
    SaltLocal=config:salt_local(),
    case config:hash_delay() of D when is_integer(D) andalso D > 0 -> timer:sleep(D); _ -> ok end,
    crypto:hash(config:hash_algorithm(),<<Pass/binary,Salt/binary,SaltLocal/binary>>).
randomize() -> {A1,A2,A3}=now(), random:seed(A1,A2,A3).
salt() -> randomize(), crypto:strong_rand_bytes(config:salt_length()).
password() ->
    randomize(),
    Allowed=config:password_chars_allowed(),Count=size(Allowed)-1,
    << << (binary:at(Allowed,random:uniform(Count))) >> || _ <- lists:seq(1,config:password_length()) >>.

random_hex(Length) -> randomize(), wf_convert:hex(crypto:strong_rand_bytes(Length)).
random_bin(Length) -> randomize(), crypto:strong_rand_bytes(Length).


ensure_urn(_Table,<<>>,_Default) ->
    {ok,<<>>};
ensure_urn(_Table,Default,Default) ->
    {ok,Default};
ensure_urn(Table,Value,Default) ->
    case is_valid_urn(Value) of
        false -> {invalid,Default};
        true -> case kvs:index(Table,urn,Value) of
            [] -> {ok,Value};
            _Exist -> {already_exist,Default} end
    end.
is_valid_urn(Binary) ->
    {ok,Re}=re:compile(<<"^[a-zA-Z0-9_-]*$">>),
    case re:run(Binary,Re) of {match,_} -> true; nomatch -> false end.

info(Message) -> message(Message, <<"info-message">>).
success(Message) -> message(Message, <<"success-message">>).
warning(Message) -> message(Message, <<"warning-message">>).
error(Message) -> message(Message, <<"error-message">>).
message(Message, Class) ->
    Id=temp_id(),
    wf:insert_top(<<"popup-messages">>, #hookup{id=Id, class=Class,
        onclick=wf:f("qi(\\'~s\\').parentNode.removeChild(qi(\\'~s\\'));",[Id,Id]),
        body=html_escape(Message)}),
    wf:wire(wf:f("window.setTimeout(function(){var a=qi('~s'); if(a){a.onclick();}},~b);",
        [Id,size(wf:to_binary(Message))*100+4000])), ok.


format_timestamp(Timestamp) ->
    {{Y,Mo,D},{H,Mi,Sec}}=calendar:now_to_datetime(Timestamp),
    io_lib:format("~4.4.0w.~2.2.0w.~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",[Y,Mo,D,H,Mi,Sec]).

% to_lower(Bin) when is_binary(Bin) ->
%     unicode:characters_to_binary(ux_string:to_lower(unicode:characters_to_list(Bin,utf8)),utf8,utf8);
% to_lower(List) ->
%     ux_string:to_lower(List).

