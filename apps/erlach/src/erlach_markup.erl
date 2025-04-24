-module(erlach_markup).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").

enc(D) ->
    X=wf:html_encode(D),
    re:replace(X,<<"\\\\">>,<<"\\&#92;">>, [global,{return,binary}]). % \\& for 're' syntax

html(#post{message=Raw},#hes{}=Hes,#st{}=S) ->
    
    Raw2=re:replace(Raw,<<"\\n{3,}">>,<<"\n\n">>, [global,{return,binary}]),
    Message=case spa_utils:option(limit,Hes) of
        Lim when is_integer(Lim) -> erlach_utils:cut(Raw2,Lim);
        _ -> Raw2
    end,
    
    FunHookups=fun(capture,[C]) ->
            case kvs:get(post,erlach_qs:urn_to_id(C)) of
                {ok,#post{}=P} -> {form,erlach_utils:link(#a{class= <<"l related-link">>},P,S)};
                _ -> {text,[?RPL,C]}
            end;
        (text,T) -> {text,T}
    end,
    
    FunBoards=fun(capture,[Uri]) ->
            case kvs:index(board,urn,Uri) of
                [#board{}=B] -> {form,erlach_utils:board_link(#a{class= <<"l related-link">>},B,S)};
                _ -> {text,[?RPL,Uri]}
            end;
        (text,T) -> {text,T}
    end,
    
    FunSpoiler=fun(capture,SpoilerList) -> {form,#span{class=spoiler,body=enc(SpoilerList)}}; (text,T) -> {text,T} end,
    FunBold=fun(capture,Data) -> {form,#span{class=strong,body=enc(Data)}}; (text,T) -> {text,T} end,
    FunItalic=fun(capture,Data) -> {form,#span{class=italic,body=enc(Data)}}; (text,T) -> {text,T} end,
    FunStrikeOut=fun(capture,Data) -> {form,#span{class=strikeout,body=enc(Data)}}; (text,T) -> {text,T} end,
    FunCitate=fun(capture,Data) -> {form,#span{class=citate,body=enc(Data)}}; (text,T) -> {text,T} end,
    CutUri=fun(Uri) -> erlach_utils:cut(Uri,wf:config(erlach,uri_max_length,50)) end,
    
    FunWebUrls=fun(capture,[Scheme,Uri]) ->
            Sh=case Scheme of <<>> -> <<"http://">>; _ -> Scheme end,
            {form,#link{target="_blank",class=l,body=enc(CutUri(Uri)),href=enc([Sh,Uri])}};
        (text,T) -> {text,T}
    end,
    
    Stage=fun(Tag,Data,Fun) -> lists:flatten([
        case Type of text -> parse_replace(Text,Tag,Fun); form -> {form,Text} end || {Type,Text} <- Data ]) end,
    
    Markup=fun(Stage0) ->
        Stage1=Stage(post_link,Stage0,FunHookups),
        Stage2=Stage(board_link,Stage1,FunBoards),
        
        Stage3=Stage(citate,Stage2,FunCitate),
        Stage4=Stage(spoiler,Stage3,FunSpoiler),
        Stage5=Stage(bold,Stage4,FunBold),
        
        % Stage5a=Stage(ym_url_replace,Stage5,FunYandexMusicUrls),
        Stage6=Stage(url_replace,Stage5,FunWebUrls),
         
        Stage7=Stage(italic,Stage6,FunItalic),
        Stage8=Stage(strikeout,Stage7,FunStrikeOut),
        
        [ case X of {form,F} -> F; {text,T} -> enc(T); Raw -> enc(Raw) end || X <- Stage8 ]
    end,
    
    Markup([{text,Message}]).


parse_replace(Data,RegexTag,Fun) ->
    lists:foldr(fun([T|C],A) -> case C of [] -> [Fun(text,T)|A]; _ -> [Fun(text,T),Fun(capture,C)|A] end end,
        [],re:split(Data,re_compiled(RegexTag),[group,{return,binary}])). % TODO: re()


% Regexp. For output use: rp(erlach_markup:re(links_replace)).
% re(link_replace_escaped) -> {ok,Re}=re:compile(<<"&gt;&gt;([a-z0-9]{1,10})\\b">>),Re;
re(post_link) -> {ok,Re}=re:compile(<<">>([a-z0-9]{1,10})\\b">>),Re;
re(board_link) -> {ok,Re}=re:compile(<<">>\\/([a-z0-9]{1,3})\\/?">>),Re;

re(spoiler) -> {ok,Re}=re:compile(<<"(?:(?:\\%\\%(.+?)\\%\\%)|(?:\\[s\\](.+?)\\[\\/s\\]))">>,[dotall]),Re;
re(bold) -> {ok,Re}=re:compile(<<"(?:(?:\\*\\*(.+?)\\*\\*)|(?:\\_\\_(.+?)\\_\\_)|(?:\\[b\\](.+?)\\[\\/b\\]))">>,[dotall]),Re;
re(italic) -> {ok,Re}=re:compile(<<"(?:(?:\\*(.+?)\\*)|(?:\\_(.+?)\\_)|(?:\\[i\\](.+?)\\[\\/i\\]))">>,[dotall]),Re;
re(strikeout) -> {ok,Re}=re:compile(<<"(\\p{Xwd}+)\\^W">>,[unicode]),Re;
re(citate) -> {ok,Re}=re:compile(<<"(\\>.+\\S)">>),Re;
% re(paragraph) -> {ok,Re}=re:compile(<<"(\\n\\n)">>),Re;

% https://gist.github.com/m-2k/f176e8eea40f2decc8ed571b0ab8411f
% http://erlang.org/doc/man/re.html#generic_character_types
re(url_replace) ->
    {ok,Re}=re:compile(<<"([Hh]ttps?://)?","((?:",            % scheme
                         "(?:(?:[\\p{Xan}]-*)*\\p{Xan}+)",    % host name
                         "(?:\\.(?:[a-z0-9]-*)*[a-z0-9]+)*",  % domain name
                         "(?:\\.(?:[a-z]{2,}))",              % TLD identifier
                         "(?:\\.)?",                          % TLD may end with dot
                         ")(?:[/?#][\\S_]*)?)">>,[unicode]),Re.  % resource path

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
