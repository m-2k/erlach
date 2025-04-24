-module(erlach_markup).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").

message(Type, Elements, Delay) ->
    wf:wire(["message('",wf:jse(nitro:render(Elements)),"','",wf:to_binary(Type),"',",wf:to_binary(Delay),");"]).
    
highlight(Type) ->
    wf:wire(["highlight('",wf:to_binary(Type),"');"]).

enc(D,Limit) when is_integer(Limit) ->
    End=#span{class=moar,body= <<"…"/utf8>>},
    [enc(erlach_utils:cut(D,Limit,"")),End];
enc(D,_) -> enc(D).

enc(D) ->
    X=wf:html_encode(D),
    re:replace(X,<<"\\\\">>,<<"\\&#92;">>, [global,{return,binary}]). % \\& for 're' syntax

html(#post{message=Raw},#hes{}=Hes,#st{}=S) ->
    Message=re:replace(Raw,<<"\\n{3,}">>,<<"\n\n">>, [global,{return,binary}]),

    FunHookups=fun(capture,[T]) ->
            case kvs:get(post,erlach_qs:urn_to_id(T)) of
                {ok,#post{}=P} -> {form,[?RPL,T],fun(_) -> erlach_utils:link(#a{class= <<"l related-link">>},P,S) end};
                _ -> {text,[?RPL,T]}
            end;
        (text,T) -> {text,T}
    end,
    FunBoards=fun(capture,[Uri]) ->
            case kvs:index(board,urn,Uri) of
                [#board{}=B] -> {form,[?RPL,Uri],fun(_) -> erlach_utils:board_link(#a{class= <<"l related-link">>},B,S) end};
                _ -> {text,[?RPL,Uri]}
            end;
        (text,T) -> {text,T}
    end,
    FunSpoiler=fun(capture,T) -> {form,T,fun(LL) -> #span{class=spoiler,body=enc(T,LL)} end}; (text,T) -> {text,T} end,
    FunBold=fun(capture,T) -> {form,T,fun(LL) -> #span{class=strong,body=enc(T,LL)} end}; (text,T) -> {text,T} end,
    FunItalic=fun(capture,T) -> {form,T,fun(LL) -> #span{class=italic,body=enc(T,LL)} end}; (text,T) -> {text,T} end,
    FunStrikeOut=fun(capture,T) -> {form,T, fun(LL) -> #span{class=strikeout,body=enc(T,LL)} end}; (text,T) -> {text,T} end,
    FunCitate=fun(capture,T) -> {form,T,fun(LL) -> #span{class=citate,body=enc(T,LL)} end}; (text,T) -> {text,T} end,
    CutUri=fun(Uri) -> erlach_utils:cut(Uri,wf:config(erlach,uri_max_length,50)) end,
    FunWebUrls=fun(capture,[Scheme,Uri]) ->
            Sh=case Scheme of <<>> -> <<"http://">>; _ -> Scheme end,
            T=CutUri(Uri),
            {form,T,fun(LL) -> #link{target="_blank",class=l,body=enc(T,LL),href=enc([Sh,Uri])} end};
        (text,T) -> {text,T}
    end,

    Stage1=parse_stage(post_link,[{text,Message}],FunHookups),
    Stage2=parse_stage(board_link,Stage1,FunBoards),
    Stage3=parse_stage(citate,Stage2,FunCitate),
    Stage4=parse_stage(spoiler,Stage3,FunSpoiler),
    Stage5=parse_stage(bold,Stage4,FunBold),
    Stage6=parse_stage(url_replace,Stage5,FunWebUrls),
    Stage7=parse_stage(italic,Stage6,FunItalic),
    Stage8=parse_stage(strikeout,Stage7,FunStrikeOut),
    wf:info(?M,"~p~n~n~n~n",[Stage8]),
    TotalLimit=spa:option(limit,Hes),
    TextCutMin=wf:config(erlach,text_cut_min_char_count,5),

    element(1,lists:foldl(fun(X,{Acc,Count}) ->
        Text=case X of {form,T,_} -> T; {text,T} -> T; _Raw -> X end,
        Length=erlach_utils:char_length(Text),
        LocalLimit=case is_integer(TotalLimit) of
            true when TotalLimit - Count >= Length -> false;
            true -> TotalLimit - Count;
            false -> false
        end,
        Generate=case is_integer(LocalLimit) of
            true when LocalLimit >= TextCutMin -> true;
            true -> false;
            false -> true
        end,
        X2=case Generate of
            true -> Acc++[ case X of {form,_,Eval} -> Eval(LocalLimit); _ -> enc(Text,LocalLimit) end ];
            false -> Acc
        end,
        {X2, Length + Count}
    end,{[],0},Stage8)).


parse_stage(Tag,Data,Fun) ->
    lists:foldr(fun({text,Text},Acc) -> parse_replace(Text,Tag,Fun) ++ Acc;
                   ({form,Text,Eval},Acc) -> [ {form,Text,Eval} | Acc ]
        end,[],Data).

parse_replace(Data,RegexTag,Fun) ->
    lists:foldr(fun([T|C],A) ->
        case C of
            [] -> [Fun(text,T)|A];
            _ -> [Fun(text,T),Fun(capture,C)|A]
        end
    end,[],re:split(Data,re_compiled(RegexTag),[group,{return,binary}])). % TODO: re()


image_filter(Topic) ->
    case re:run(Topic,erlach_markup:re_compiled(thread_filter_bw),[]) of
        {match,_} -> bw;
        nomatch -> ?UNDEF
    end.


% Regexp. For output use: rp(erlach_markup:re(links_replace)).
% re(link_replace_escaped) -> {ok,Re}=re:compile(<<"&gt;&gt;([a-z0-9]{1,10})\\b">>),Re;
re(post_link) -> {ok,Re}=re:compile(<<">>([a-z0-9]{1,10})\\b">>),Re;
re(board_link) -> {ok,Re}=re:compile(<<">>\\/([a-z0-9]{1,3})\\/?">>),Re;

re(spoiler) -> {ok,Re}=re:compile(<<"(?:(?:\\%\\%(.+?)\\%\\%)|(?:\\[s\\](.+?)\\[\\/s\\]))">>,[dotall]),Re;
re(bold) -> {ok,Re}=re:compile(<<"(?:(?:\\*\\*(.+?)\\*\\*)|(?:\\_\\_(.+?)\\_\\_)|(?:\\[b\\](.+?)\\[\\/b\\]))">>,[dotall]),Re;
re(italic) -> {ok,Re}=re:compile(<<"(?:(?:\\*(.+?)\\*)|(?:\\_(.+?)\\_)|(?:\\[i\\](.+?)\\[\\/i\\]))">>,[dotall]),Re;
re(strikeout) -> {ok,Re}=re:compile(<<"(\\p{Xwd}+)\\^W">>,[unicode]),Re;
re(citate) -> {ok,Re}=re:compile(<<"(?<=\\s|^)(\\>.+)(?=\\s|$)">>),Re; % http://stackoverflow.com/a/6713378/3676060
% re(paragraph) -> {ok,Re}=re:compile(<<"(\\n\\n)">>),Re;

% https://gist.github.com/m-2k/f176e8eea40f2decc8ed571b0ab8411f
% http://erlang.org/doc/man/re.html#generic_character_types
re(url_replace) ->
    {ok,Re}=re:compile(<<"([Hh]ttps?://)?","((?:",            % scheme
                         "(?:(?:[\\p{Xan}]-*)*\\p{Xan}+)",    % host name
                         "(?:\\.(?:[a-z0-9]-*)*[a-z0-9]+)*",  % domain name
                         "(?:\\.(?:[a-z]{2,}))",              % TLD identifier
                         "(?:\\.)?",                          % TLD may end with dot
                         ")(?:[/?#][\\S_]*)?)">>,[unicode]),Re;  % resource path

re(thread_filter_bw) ->
    {ok,Re}=re:compile(<<"\\[bw\\]"/utf8>>,[unicode,caseless]), Re.

re_compiled(post_link) ->
    {re_pattern,1,0,0,
            <<69,82,67,80,122,0,0,0,0,0,0,0,81,0,0,0,255,255,255,255,
              255,255,255,255,62,0,62,0,1,0,1,0,0,0,64,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,125,0,54,
              29,62,29,62,127,0,43,0,1,106,0,0,0,0,0,0,255,3,0,0,0,0,
              254,255,255,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,104,0,1,
              0,10,114,0,43,5,114,0,54,0>>};
re_compiled(url_replace) ->
    {re_pattern,2,1,0,
            <<69,82,67,80,100,1,0,0,0,8,0,0,65,0,0,0,255,255,255,255,
              255,255,255,255,0,0,46,0,0,0,2,0,0,0,64,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,125,1,32,
              140,127,0,52,0,1,106,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,29,116,29,116,29,112,37,
              115,29,58,29,47,29,47,114,0,52,127,0,226,0,2,125,0,176,
              125,0,24,140,125,0,13,108,0,8,0,3,5,0,0,33,45,115,0,13,
              87,16,5,0,114,0,24,140,125,0,81,29,46,140,125,0,38,106,
              0,0,0,0,0,0,255,3,0,0,0,0,254,255,255,7,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,33,45,115,0,38,106,0,0,0,0,0,0,255,3,
              0,0,0,0,254,255,255,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              100,115,0,81,125,0,49,29,46,125,0,41,106,0,0,0,0,0,0,0,
              0,0,0,0,0,254,255,255,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,104,0,2,0,0,114,0,41,114,0,49,140,125,0,5,29,46,114,
              0,5,114,0,176,140,125,0,38,106,0,0,0,0,8,128,0,128,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,85,8,114,0,
              38,114,0,226,114,1,32,0>>};

re_compiled(Tag) -> wf:info(?M,"Using non-compiled regexp",[]), re(Tag).
