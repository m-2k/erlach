-module(erlach_image).
-author('Andy').
-compile(export_all).

% apt-get install libjpeg-progs

-include("erlach.hrl").

-record(convert_entry,{original,converted,port,from,target,group,image_info,stage}).
-define(SUP_CLASS,service).
-define(SUP_NAME,converter).

path(P) -> <<(wf:to_binary(P))/binary,".image">>.

mime(#jpeg{}) -> 'image/jpeg';
mime(#png{}) -> 'image/png';
mime(#gif{}) -> 'image/gif';
mime(#bpg{}) -> 'image/bpg'.

dim(#jpeg{width=W,height=H}) -> {ok,W,H};
dim(#png{width=W,height=H}) -> {ok,W,H};
dim(#gif{width=W,height=H}) -> {ok,W,H};
dim(#bpg{width=W,height=H}) -> {ok,W,H}.


reply(Target,From,Group,Message) ->
    case Target of ?UNDEF -> From ! {server,Message}; _ -> wf:send(Group,{server,Message}) end.

start() ->
    wf:info(?M,"Convert supervisor starting...",[]),
    n2o_async:stop(?SUP_CLASS,?SUP_NAME),
    n2o_async:start(#handler{module=?MODULE,group=?APP,class=?SUP_CLASS,name=?SUP_NAME,state=[]}).
convert(Path,Group,Target) ->
    case n2o_async:pid({?SUP_CLASS,?SUP_NAME}) of Pid when is_pid(Pid) -> ok; _ -> start() end,
    n2o_async:send(?SUP_CLASS,?SUP_NAME,{convert,Path,Group,Target,self()}).
target(Path,Target) ->
    n2o_async:send(?SUP_CLASS,?SUP_NAME,{target,Path,Target}).
    
is_alpha(#gif{alpha=0}) -> false;
is_alpha(#gif{}) -> true;
is_alpha(#png{color_type='grayscale-alpha'}) -> true;
is_alpha(#png{color_type='truecolor-alpha'}) -> true;
is_alpha(#png{}) -> false;
is_alpha(#jpeg{}) -> false;
is_alpha(#bpg{alpha=true}) -> true;
is_alpha(#bpg{}) -> false.

proc(init,H) -> wf:info(?M,"Convert supervisor started: ~p",[self()]), {ok,H};
proc({convert,Path,Group,Target,From}=M,#handler{state=S}=H) when is_pid(From) ->
    Original=wf:to_binary(Path),
    case image_info_file(Original) of
        {ok,#jpeg{width=Width,height=Height},_} when Width > 8688 orelse Height > 6192 ->
            reply(Target,From,Group,#pubsub{target=image,action=convert,element=Target,data={error,{high_resolution,Width,Height}},from=From}),
            {reply,ok,H};
        {ok,#jpeg{width=Width,height=Height}=ImageInfo,_} ->
            Mem= <<"524288">>, % Maximum memory to use (in kbytes)
            Converted=path(Original),
            Cmd= <<"jpegtran -copy none -optimize -maxmemory ",Mem/binary," -outfile ",Converted/binary," ",Original/binary>>,
            Port = erlang:open_port({spawn, Cmd},[exit_status]),
            wf:info(?M,"Jpegtran (~p): ~p",[Port,Cmd]),
            Entry=#convert_entry{original=Original,converted=Converted,port=Port,from=From,target=Target,group=Group,image_info=ImageInfo,stage=convert},
            {reply,ok,H#handler{state=[ Entry | S ]}};
        {ok,#gif{width=Width,height=Height},_} when Width > 8688 orelse Height > 6192 ->
            reply(Target,From,Group,#pubsub{target=image,action=convert,element=Target,data={error,{high_resolution,Width,Height}},from=From}),
            {reply,ok,H};
        {ok,#gif{width=Width,height=Height}=ImageInfo,_} ->
            Converted=path(Original),
            Cmd= <<"mv ",Original/binary," ",Converted/binary>>,
            Port = erlang:open_port({spawn, Cmd},[exit_status]),
            wf:info(?M,"Gif fake (~p): ~p",[Port,Cmd]),
            Entry=#convert_entry{original=Original,converted=Converted,port=Port,from=From,target=Target,group=Group,image_info=ImageInfo,stage=convert},
            {reply,ok,H#handler{state=[ Entry | S ]}};
        {ok,ImageInfo,_} ->
            PngFormat= case is_alpha(ImageInfo) of true -> <<" PNG32:">>; false -> <<" PNG24:">> end,
            Resize= wf:config(erlach,imagemagick_resize,<<" -resize 8688x8688\\>">>),
            % 8688 × 5792 Canon 3DS
            % 8256 x 6192 Pentax 645Z
            % 7360 × 4912 Nikon D810
            {CmdOptions,CmdFileExt}=case ImageInfo of
                #gif{animation=true,delays=Delays} ->
                    case Delays of
                        [_|_] -> file:write_file(<<Original/binary,".delays">>,<< <<(integer_to_binary(D))/binary,$\n>> || D <- Delays >>);
                        _ -> skip
                    end,
                    {<<" -coalesce ">>,<<"-%04d.trim">>};
                _NoAnimation -> {<<" ">>,<<".trim">>}
            end,
                
            Cmd= <<"convert",Resize/binary,CmdOptions/binary,Original/binary,PngFormat/binary,Original/binary,CmdFileExt/binary>>,
            
            Port = erlang:open_port({spawn, Cmd},[exit_status]),
            wf:info(?M,"Cut Open (~p): ~p ~p",[Port,ImageInfo,Cmd]),
            Entry=#convert_entry{original=Original,port=Port,from=From,target=Target,group=Group,image_info=ImageInfo,stage=prepare},
            {reply,ok,H#handler{state=[ Entry | S ]}};
        Error ->
            wf:warning(?M,"Unsupperted file (~p): ~p",[Error,Original]),
            {reply,error,H}
    end;
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
        {value, #convert_entry{original=Original,image_info=ImageInfo,stage=prepare}=Entry, S2} ->
            file:delete(Original),
            Converted=path(Original),
            Quality=wf:to_binary(wf:config(erlach,bpg_quality,21)),
            CompLevel=wf:to_binary(wf:config(erlach,bpg_compression_level,6)),
            
            {CmdOptions,CmdFile}=case ImageInfo of
                #gif{animation=true,loop_count=LoopCount} ->
                    DelayFile=case filelib:is_regular(<<Original/binary,".delays">>) of
                        true -> <<"-delayfile ",Original/binary,".delays">>;
                        false -> <<"-fps ",(wf:to_binary(wf:config(erlach,bpg_default_fps,15)))/binary>>
                    end,
                    {<<" -m ",CompLevel/binary," -b 8 -q ",Quality/binary," -a ",
                        Original/binary,"-%4d.trim ",DelayFile/binary," -loop ",(wf:to_binary(LoopCount))/binary>>,<<>>};
                _NoAnimation -> {<<" -m ",CompLevel/binary," -b 8 -q ",Quality/binary>>,<<" ",Original/binary,".trim">>}
            end,
            Encoder= case is_alpha(ImageInfo) of true -> <<" -e jctvc">>; false -> <<" -e x265">> end,
            
            Cmd= <<"bpgenc",Encoder/binary,CmdOptions/binary," -o ",Converted/binary,CmdFile/binary>>,
                
            Port2 = erlang:open_port({spawn, Cmd},[exit_status]),
            wf:info(?M,"Converting to BPG (~p): ~p",[Port2,Cmd]),
            Entry2=Entry#convert_entry{converted=Converted,port=Port2,stage=convert},
            {reply,ok,H#handler{state=[ Entry2 | S2 ]}};
        {value, #convert_entry{original=Original,converted=Converted,from=From,target=Target,group=Group,image_info=ImageInfo=#jpeg{},stage=convert}, S2} ->
            wf:info(?M,"Jpeg converting (~p) finished",[Port]),
            file:delete(Original),
            reply(Target,From,Group,#pubsub{target=image,action=convert,element=Target,data={ok,Converted},from=From}),
            {reply,ok,H#handler{state=S2}};
        {value, #convert_entry{original=Original,converted=Converted,from=From,target=Target,group=Group,image_info=ImageInfo=#gif{},stage=convert}, S2} ->
            wf:info(?M,"Gif fake converting (~p) finished",[Port]),
            file:delete(Original),
            reply(Target,From,Group,#pubsub{target=image,action=convert,element=Target,data={ok,Converted},from=From}),
            {reply,ok,H#handler{state=S2}};
        {value, #convert_entry{original=Original,converted=Converted,from=From,target=Target,group=Group,image_info=_,stage=convert}, S2} ->
            wf:info(?M,"Converting to BPG (~p) finished",[Port]),
            [ file:delete(F) || F <- filelib:wildcard(wf:to_list(<<Original/binary,"*.trim">>))],
            file:delete(<<Original/binary,".delays">>),
            reply(Target,From,Group,#pubsub{target=image,action=convert,element=Target,data={ok,Converted},from=From}),
            {reply,ok,H#handler{state=S2}};
        _ -> {reply,error,H}
    end;
proc({Port,{exit_status,Status}}=E,#handler{state=S}=H) when is_port(Port) ->
    wf:error(?M,"Error event ~p",[E]),
    case lists:keytake(Port,#convert_entry.port,S) of
        {value, _, S2} -> {reply,error,H#handler{state=S2}};
        _ -> {reply,error,H}
    end;
% работа порта была прервана
% proc({'EXIT',Port,Reason},H) -> {reply,ok,H};

proc(Unknown,H) ->
    wf:warning(?M,"Unknown event ~p",[Unknown]),
    {reply,unknown,H}.

clear_temporary(#rst{image=Name}) ->
    wf:info(?M,"Clear temporary attachment",[]); % TODO:
clear_temporary(Rst) -> wf:warning(?M,"Wrong state for clear temporary ~p",[Rst]).

abort() -> clear.  % TODO:

image_info_file(FileName) ->
    case file:read_file(FileName) of
        {ok,Data} ->
            case image_info(Data) of
                {ok,Info} -> {ok,Info,Data};
                Error -> Error
            end;
        Error -> Error
    end.



% http://dev.exiv2.org/projects/exiv2/wiki/The_Metadata_in_JPEG_files
% http://stackoverflow.com/a/17848968
% https://en.wikipedia.org/wiki/JPEG_File_Interchange_Format#JFIF_APP0_marker_segment
% http://vip.sugovica.hu/Sardi/kepnezo/JPEG%20File%20Layout%20and%20Format.htm <--
image_info(<<255,216,Data/binary>>) ->
    Foreach=fun Foreach(Offset) ->
        case binary:part(Data,Offset,2) of
            <<16#ff,SOFn>> when SOFn =:= 16#c0 orelse SOFn =:= 16#c2 ->
                <<B:8,H:16/big,W:16/big,Comp:8>> =binary:part(Data,Offset+4,6),
                C=case Comp of 1 -> 'GreyScaled'; 3 -> 'YcbCr'; 4 -> 'CMYK' end,
                {ok,#jpeg{bit_depth=B,height=H,width=W,color_space=C}};
            <<16#ff,_>> ->
                <<L:16/big>> =binary:part(Data,Offset+2,2),
                Foreach(Offset+L+2)
        end
    end,
    Foreach(0);
image_info(<<137,80, 78, 71, 13, 10, 26, 10,IHDR:25/binary,_/binary>>) ->
    % Length= <<0,0,0,13>>,
    % Type= <<73,72,68,82>>,
    <<0,0,0,13,73,72,68,82,Data:13/binary,CRC:4/binary>>= IHDR,
    <<W:32/big,H:32/big,Depth:8,Color:8,_Compression:8,_Filter:8,_Interlace:8>>=Data,
    ColorType=case Color of 0 -> greyscale; 2 -> truecolor; 3 -> indexed; 4 -> 'grayscale-alpha'; 6 -> 'truecolor-alpha' end,
    {ok,#png{width=W,height=H,bit_depth=Depth,color_type=ColorType}};
% http://www.w3.org/Graphics/GIF/spec-gif89a.txt
% http://giflib.sourceforge.net/whatsinagif/bits_and_bytes.html
% http://www.onicos.com/staff/iz/formats/gif.html
image_info(<<$G,$I,$F,$8,N,$a,W:16/little,H:16/little,GCTF:1,Res:3,StCT:1,SzGCT:3,BCI:8,PAR:8,_/binary>>=Data) when N =:= $7 orelse N =:= $9 ->
    DataForeach=fun DataForeach(DataOffset) ->
        case binary:at(Data,DataOffset) of
            0 -> DataOffset+1;
            DataSize -> DataForeach(DataOffset+DataSize+1)
        end
    end,
    
    ExtensionForeach=fun ExtensionForeach({ExtOffset,#gif{delays=D,alpha=A}=G}) ->
        case binary:at(Data,ExtOffset) of
            255 -> % Application Extension Block
                DataOffset=ExtOffset+2+binary:at(Data,ExtOffset+1),
                case binary:part(Data,DataOffset,5) of
                    <<3,1,L:16/little,0>> -> {DataOffset+5,G#gif{loop_count=L}};
                    _ -> {DataForeach(DataOffset),G}
                end;
            254 -> {DataForeach(ExtOffset+1),G}; % Comment Extension Block
            249 ->
                <<_Reserved:3,_Disposal:3,_User:1,Transparent:1>>= binary:part(Data,ExtOffset+2,1),
                <<Delay:16/little>>= binary:part(Data,ExtOffset+3,2),
                {ExtOffset+2+binary:at(Data,ExtOffset+1)+1,G#gif{delays=[Delay|D],alpha=A+Transparent}}; % Graphic Control Extension Block
            1 -> {DataForeach(ExtOffset+14),G} % Plain Text Extension Block
        end
    end,
    
    BlockForeach=fun BlockForeach({BlockOffset,#gif{frames=F}=G}) ->
        case binary:at(Data,BlockOffset) of
            33 -> BlockForeach(ExtensionForeach({BlockOffset+1,G})); % Extension Block
            44 -> % Image Block
                SizeLCT=case <<(binary:at(Data,BlockOffset+9)):8>> of
                    <<0:1,_:7>> -> 0;
                    <<1:1,_:4,Size:3>> -> trunc(math:pow(2,Size+1)*3)
                end,
                BlockForeach({DataForeach(BlockOffset+10+SizeLCT+1),G#gif{frames=F+1}});
            59 -> {ok,G}
        end
    end,
    {ok,Gif}=BlockForeach({13+trunc(math:pow(2,SzGCT+1)*3),#gif{alpha=0,frames=0,loop_count=0,delays=[]}}),
    {ok,Gif#gif{animation=Gif#gif.frames > 1,delays=lists:reverse(Gif#gif.delays),width=W,height=H}};
image_info(<<16#425047fb:32,PF:3/big,A1:1,BD:4/big,CS:4/big,E:1,A2:1,LR:1,AN:1,D:8/binary,_/bitstring>>) ->
    
    ReadUE7 = fun ReadUE7(<<1:1,Byte7:7,Tail/binary>>,Number) ->
            ReadUE7(Tail,<<Number/bitstring,Byte7:7>>);
        ReadUE7(<<0:1,Byte7:7,Tail/binary>>,Number) ->
            Size=bit_size(Number)+7,
            <<UE7:Size>> = <<Number/bitstring,Byte7:7>>,
            {ok,UE7,Tail}
    end,
    ReadUE7Array = fun ReadUE7Array(<<>>,_,Acc) -> Acc;
        ReadUE7Array(_,0,Acc) -> Acc;
        ReadUE7Array(ByteList,Count,Acc) ->
            {ok,Num,Tail}=ReadUE7(ByteList,<<>>),
            ReadUE7Array(Tail,Count-1,[Num|Acc])
    end,
    
    case ReadUE7Array(D,2,[]) of
        [H,W] -> {ok,#bpg{
            pixel_format=case PF of
                0 -> grayscale;
                1 -> '4:2:0-jpeg';
                2 -> '4:2:2-jpeg';
                3 -> '4:4:4';
                4 -> '4:2:0-mpeg2';
                5 -> '4:2:2-mpeg2' end,
            alpha=case {A1,A2} of
                {0,0} -> no_alpha;
                {1,0} -> alpha_color_not_premultiplied;
                {1,1} -> alpha_color_premultiplied;
                {0,1} -> alpha_CMYK end,
            bit_depth=BD+8,
            color_space=case CS of
                0 -> {'YCbCr','BT 601', 5};
                1 -> {'RGB', undefined,unedfined};
                2 -> {'YCgCo', undefined, 8};
                3 -> {'YCbCr', 'BT 709', 1};
                4 -> {'YCbCr', 'BT 2020', 9};
                5 -> { reserved, 'BT 2020', undefined};
                _ -> { reserved, undefined, undefined} end,
            extension_present=case E of 0 -> false; 1 -> true end,
            limited_range=case LR of 0 -> false; 1 -> true end,
            animation=case AN of 0 -> false; 1 -> true end,
            width=W,
            height=H}};
        _ -> {error,bpg_wrong_data}
    end;
image_info(_) -> {error,unknown_image_format}.


t_l(L) -> lists:foldr(fun(A,Acc) -> [A|Acc] end, [], L).
    
    


