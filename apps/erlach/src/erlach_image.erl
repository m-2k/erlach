-module(erlach_image).
-author('andy').
-compile(export_all).

-include_lib("n2o/include/wf.hrl").

-include("erlach.hrl").

-record(convert_entry,{original,converted,port,from,target,group}).
-define(SUP_CLASS,service).
-define(SUP_NAME,converter).

path(Original) -> <<(wf:to_binary(Original))/binary,".bpg">>.
path2(Original) -> <<(wf:to_binary(Original))/binary,"-x.bpg">>.

start() ->
    wf:info(?M,"Convert supervisor starting...",[]),
    n2o_async:stop(service,converter),
    n2o_async:start(#handler{module=?MODULE,group=?APP,class=?SUP_CLASS,name=?SUP_NAME,state=[]}).
convert(Path,Group,Target) ->
    case n2o_async:pid({?SUP_CLASS,?SUP_NAME}) of Pid when is_pid(Pid) -> ok; _ -> start() end,
    n2o_async:send(?SUP_CLASS,?SUP_NAME,{convert,Path,Group,Target,self()}).
target(Path,Target) ->
    n2o_async:send(?SUP_CLASS,?SUP_NAME,{target,Path,Target}).

proc(init,H) -> wf:info(?M,"Convert supervisor started: ~p",[self()]), {ok,H};
proc({convert,Path,Group,Target,From}=M,#handler{state=S}=H) when is_pid(From) ->
    Original=wf:to_binary(Path),
    Converted=path(Original),
    Cmd= <<"bpgenc -q 21 -o ",Converted/binary," ",Original/binary>>,
    Port = erlang:open_port({spawn, Cmd},[exit_status]),
    wf:info(?M,"Converting to BPG (~p): ~p",[Port,Cmd]),
    Entry=#convert_entry{original=Original,converted=Converted,port=Port,from=From,target=Target,group=Group},
    {reply,ok,H#handler{state=[ Entry | S ]}};
proc({target,Path,Target},#handler{state=S}=H) ->
    wf:info(?M,"Linking target ~p with ~p~n~p",[Target,Path,S]),
    case lists:keytake(Path,#convert_entry.original,S) of
        {value, #convert_entry{}=E, S2} -> {reply,ok,H#handler{state=[ E#convert_entry{target=Target} | S2 ]}};
        _ -> {reply,error,H}
    end;
% proc({convert,RelPath}) -> wf:info(?M,"Convert to BPG ~p",[1]);

% получены данные Data от внешнего приложения
% proc({Port,{data,Data}},H) -> {reply,ok,H};
% ответ на {Port, close}
% proc({Port,closed},H) -> {reply,ok,H};
% ответ на {Port, {connect, NewPid}}
% proc({Port,connected},H) -> {reply,ok,H};
% передается если при создании порта использовалась опция exit_status и внешнее приложение завершило свою работу
proc({Port,{exit_status,0}},#handler{state=S}=H) when is_port(Port) ->
    case lists:keytake(Port,#convert_entry.port,S) of
        {value, #convert_entry{original=Original,converted=Converted,from=From,target=Target,group=Group}, S2} ->
            wf:info(?M,"Converting to BPG (~p) finished",[Port]),
            file:delete(Original),
            case Target of
                ?UNDEF -> From ! {server,#pubsub{action=bpg,data=Converted,from=From,target=Target}};
                _ -> wf:send(Group,{server,#pubsub{action=bpg,data=Converted,from=From,target=Target}})
            end,
            {reply,ok,H#handler{state=S2}};
        _ -> {reply,error,H}
    end;
% proc({Port,{exit_status,Status}},H) when is_port(Port) -> {reply,ok,H};
% работа порта была прервана
% proc({'EXIT',Port,Reason},H) -> {reply,ok,H};

proc(Unknown,H) -> wf:warning(?M,"Unknown event ~p",[Unknown]), {reply,unknown,H}.

clear_temporary(#rst{image=Name}) ->
    wf:info(?M,"Clear temporary attachment",[]); % TODO:
clear_temporary(Rst) -> wf:warning(?M,"Wrong state for clear temporary ~p",[Rst]).

abort() -> clear.  % TODO:


bpg_info({filename,FileName}) ->
    case file:open(FileName, [read,raw,binary]) of
        {ok, IoDevice} ->
            case file:read(IoDevice, 14) of
                {ok,Data} -> file:close(IoDevice), bpg_info({data,Data});
                eof -> file:close(IoDevice), {error,small_file};
                {error, Reason} -> file:close(IoDevice), {error,Reason}
            end;
        _ -> {error,wrong_file_name}
    end;
bpg_info({data,<<Header:14/binary,_/binary>>}) ->
    FileMagic = 16#425047fb, % "BPGû"    
    case Header of
        <<FileMagic:32,PixelFormat:3,Alpha1Flag:1,BitDepthMinus8:4,ColorSpace:4,
            ExtensionPresentFlag:1,Alpha2Flag:1,LimitedRangeFlag:1,AnimationFlag:1,
            PictureDimensions:8/binary>> ->
            {ok,PictureWidth,PictureHeight}=bpg_dimensions(PictureDimensions),
            {ok,#bpg_info{
                pixel_format=case PixelFormat of
                    0 -> grayscale;
                    1 -> '4:2:0-jpeg';
                    2 -> '4:2:2-jpeg';
                    3 -> '4:4:4';
                    4 -> '4:2:0-mpeg2';
                    5 -> '4:2:2-mpeg2' end,
                alpha=case {Alpha1Flag,Alpha2Flag} of
                    {0,0} -> no_alpha;
                    {1,0} -> alpha_color_not_premultiplied;
                    {1,1} -> alpha_color_premultiplied;
                    {0,1} -> alpha_CMYK end,
                bit_depth=BitDepthMinus8+8,
                color_space=case ColorSpace of
                    0 -> {'YCbCr','BT 601', 5};
                    1 -> {'RGB', undefined,unedfined};
                    2 -> {'YCgCo', undefined, 8};
                    3 -> {'YCbCr', 'BT 709', 1};
                    4 -> {'YCbCr', 'BT 2020', 9};
                    5 -> { reserved, 'BT 2020', undefined};
                    _ -> { reserved, undefined, undefined} end,
                extension_present=case ExtensionPresentFlag of 0 -> false; 1 -> true end,
                limited_range=case LimitedRangeFlag of 0 -> false; 1 -> true end,
                animation=case AnimationFlag of 0 -> false; 1 -> true end,
                picture_width=PictureWidth,
                picture_height=PictureHeight}};
        _ -> {error,wrong_bpg_header}
    end.


bpg_dimensions(Bytes) ->
    ReadUE7 = fun ReadUE7(<<1:1,Byte7:7,Tail/binary>>,Number) ->
            ReadUE7(Tail,<<Number/bitstring,Byte7:7>>);
        ReadUE7(<<0:1,Byte7:7,Tail/binary>>,Number) ->
            Size=bit_size(Number)+7,
            <<UE7:Size>> = <<Number/bitstring,Byte7:7>>,
            {ok,UE7,Tail}
        end,
    ReadUE7Array = fun ReadUE7Array(<<>>,_,Acc) -> {error,wrong_data};
        ReadUE7Array(_,0,Acc) -> Acc;
        ReadUE7Array(ByteList,Count,Acc) ->
            {ok,Num,Tail}=ReadUE7(ByteList,<<>>),
            ReadUE7Array(Tail,Count-1,[Num|Acc])
        end,
    case ReadUE7Array(Bytes,2,[]) of
        [H,W] -> {ok,W,H};
        Error -> Error end.
