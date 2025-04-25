-module(erlach_image).
-author('Andy').
-compile(export_all).

% https://gist.github.com/m-2k/758d25266a444515b724
% apt-get install libjpeg-progs


%% %% %% %% %% %% %% %% %% %% [ TESTING ] %% %% %% %% %% %% %% %% %% %%
%%
%% Replace lines in ftp.js for slow loading:
%% 
%% ftp.send(item,data); // original
%% setTimeout(function() { ftp.send(item,data) }, 700); // slow mode
%% 
%% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %%

-include("erlach.hrl").
-include("erlach_image.hrl").

ext(#jpeg{}) -> <<".jpg">>;
ext(#png{}) ->  <<".png">>;
ext(#gif{}) ->  <<".gif">>;
ext(#bpg{}) ->  <<".bpg">>.

mime(#jpeg{}) -> <<"image/jpeg">>;
mime(#png{}) ->  <<"image/png">>;
mime(#gif{}) ->  <<"image/gif">>;
mime(#bpg{}) ->  <<"image/bpg">>.

type(#jpeg{}) -> jpeg;
type(#png{}) ->  png;
type(#gif{}) ->  gif;
type(#bpg{}) ->  bpg;
type(_) -> ?UNDEF.

w(Info) -> element(#image_info.width,Info).
h(Info) -> element(#image_info.height,Info).


filename(#ftp{filename=F}) -> spa_utils:hash({file,F,erlang:system_time(nano_seconds)}). % n2o upload

destination(Source,Storage,Path,Info) ->
    filename:join([Storage,Path,[filename:basename(Source),ext(Info)]]).

start(Group) -> restart(Group).
restart(Group) ->
    wf:warning(?M,"Convert supervisor starting...",[]),
    case n2o_async:stop(?SUP_CLASS,Group) of
        #handler{}=H -> n2o_async:start(H);
        _ -> n2o_async:start(#handler{module=?M,group=?APP,class=?SUP_CLASS,name=Group,state=#state{}})
    end.
convert(Key,Group,Source,Storage,Path,Meta,Target,FinallyFun,ErrorFun,AutoStart) ->
    wf:info(?M,"(~p) Call convert ~p ~p",[self(),Key,Source]),
    case n2o_async:pid({?SUP_CLASS,Group}) of Pid when is_pid(Pid) -> ok; _ -> restart(Group) end,
    E=#entry{id=Key,group=Group,
        source=Source,storage=Storage,path=Path,meta=Meta,target=Target,
        finally=FinallyFun,error=ErrorFun,autostart=AutoStart,from=self(),req=?REQ},
    n2o_async:send(?SUP_CLASS,Group,{insert_entry,E}).

run(Key,Group) ->
    wf:info(?M,"(~p) Run entry ~p ~p",[self(),Key,Group]),
    n2o_async:send(?SUP_CLASS,Group,{run_entry,Key}).
update(Key,Group,Fun) -> n2o_async:send(?SUP_CLASS,Group,{upadate_entry,Key,Fun}).
remove_pending(Pid,Group) when is_pid(Pid) ->
    wf:info(?M,"(~p) Remove entries by pid ~p ~p",[self(),Pid,Group]),
    n2o_async:send(?SUP_CLASS,Group,{remove_entries,Pid,pending}).
abort(Key,Group) -> abort(Key,Group,false).
abort(Key,Group,User) -> n2o_async:send(?SUP_CLASS,Group,{abort_entry,Key,User}).
dump(Group) -> n2o_async:send(?SUP_CLASS,Group,dump).
stop(Group) -> n2o_async:stop(?SUP_CLASS,Group).

check_file(Path) ->
    wf:info(?M,"(~p) Checking file info ~p",[self(),filename:basename(Path)]),
    case image_info_file(Path) of
        {ok,Info} ->
            W=w(Info),
            H=h(Info),
            {MaxW,MaxH}=wf:config(erlach,upload_max_dimensions,{8688,6192}),
            case W > MaxW orelse H > MaxH of
                true -> {error,{high_resolution,W,H}};
                false -> {ok,Info}
            end;
        Error -> Error
    end.

stage(#entry{infoA=#jpeg{},stage=starting,source=SP,destination=DP}=E) ->
    Mem=wf:to_binary(wf:config(erlach,jpegtran_maxmemory,524288)),
    Cmd=["jpegtran -copy none -optimize -maxmemory ",Mem," -outfile ",DP," ",SP],
    {ok, Cmd, E#entry{stage=preview}};
stage(#entry{infoA=#png{},stage=starting,source=SP,destination=DP}=E) ->
    Options=wf:config(erlach,optipng_opts,"-zc9 -zm8 -zs0 -f0 -nb -nc -np -nx -fix"),
    {ok, ["optipng ",Options," -out ",DP," ",SP], E#entry{stage=preview}};
stage(#entry{infoA=#gif{},stage=starting,source=SP,destination=DP}=E) -> {ok, ["mv ",SP," ",DP], E#entry{stage=preview}};
stage(#entry{infoA=#bpg{},stage=starting,source=SP,destination=DP}=E) -> {ok, ["mv ",SP," ",DP], E#entry{stage=finishing}};
stage(#entry{infoA=Info,stage=preview,destination=DP}=E) ->
    DLen=erlach_utils:char_length(DP),
    LExt=erlach_utils:char_length(ext(Info)),
    DPR=lists:sublist(erlach_utils:utf8_to_list(DP),DLen-LExt),
    Cmd=["convert ",DP," -background white -flatten ",
        "\\( -clone 0 -thumbnail 512x512 -unsharp 1x.3 -write ",DPR,"-R512.jpg +delete \\) ",
        "\\( -clone 0 -thumbnail 256x256 -unsharp 1x.4 -write ",DPR,"-R256.jpg \\) ",
        "+clone -delete 0-1 -thumbnail 128x128 -unsharp 1x.6 ",DPR,"-R128.jpg"],
    {ok, Cmd, E#entry{stage=finishing}}.

spawn_external(#entry{id=Key}=E,#state{entry_map=EM,port_map=PM}=S) ->
    wf:info(?M,"(~p) Spawning for ~p",[self(),Key]),
    case stage(E) of
        {ok,Cmd,#entry{destination=DP}=E2} ->
            Port=erlang:open_port({spawn, wf:to_binary(Cmd)},[exit_status]),
            wf:info(?M,"(~p) Process ~p ~p",[self(),Port,Cmd]),
            PM2=maps:put(Port,Key,PM),
            EM2=maps:put(Key,E2#entry{port=Port},EM),
            {ok,S#state{entry_map=EM2,port_map=PM2}};
        Error -> Error
    end.
    
reply_error(#entry{id=Key,target=T,meta=M,from=F}=E,Error,#state{}=S) ->
    wf:warning(?M,"(~p) Reply error ~p ~p",[self(),T,Error]),
    D=case Error of {error,_} -> Error; _ -> {error,Error} end,
    Msg=#pubsub{target=image,action=convert,element=Key,meta=M,data=D,from=F},
    wf:send(T,{server,Msg}).
reply_success(#entry{id=Key,target=T,meta=M,from=F,storage=Storage,path=Path,destination=DP,infoA=InfoA,infoB=InfoB}=E,#state{}=S) ->
    wf:info(?M,"(~p) Reply success ~p ~p",[self(),T,Key]),
    Msg=#pubsub{target=image,action=convert,element=Key,meta=M,data={ok,Storage,Path,filename:basename(DP),InfoA,InfoB},from=F},
    wf:send(T,{server,Msg}).

clear(#entry{id=Key,source=SP,port=Port}=E,#state{queue=Q,entry_map=EM,port_map=PM}=S) ->
    wf:info(?M,"(~p) Clearing for ~p",[self(),Key]),
    PM2=maps:remove(Port,PM),
    EM2=maps:remove(Key,EM),
    Q2=lists:delete(Key,Q),
    file:delete(SP),
    S#state{queue=Q2,entry_map=EM2,port_map=PM2}.

find_entry(Port,#state{entry_map=EM,port_map=PM}) when is_port(Port) ->
    wf:info(?M,"(~p) Find entry for ~p ~p ~p",[self(),Port,PM,EM]),
    case maps:find(Port,PM) of
        {ok,Key} -> maps:find(Key,EM);
        E -> wf:info(?M,"(~p) Find entry error ~p in ~p",[self(),Port,E]), error
    end.

finally(#entry{id=Key,finally=Fun,destination=DP}=E,#state{entry_map=EM}=S) ->
    wf:info(?M,"(~p) Exec finally fun for ~p",[self(),Key]),
    {ok,InfoB}=image_info_file(DP),
    E2=E#entry{infoB=InfoB},
    {E4,EM2}=case case is_function(Fun,1) of true -> Fun(E2); false -> false end of
        #entry{}=E3 -> {E3,maps:update(Key,E3,EM)};
        _ -> {E2,maps:remove(Key,EM)}
    end,
    reply_success(E4,S),
    S#state{entry_map=EM2}.

error(#entry{id=Key,error=Fun,destination=DP}=E,Error,#state{entry_map=EM}=S) ->
    wf:error(?M,"(~p) Exec error fun for ~p ~p",[self(),Key,Error]),
    {E3,S2}=case case is_function(Fun,1) of true -> Fun(E); false -> false end of
        #entry{}=E2 -> {E2,S#state{entry_map=maps:update(Key,E2,EM)}};
        _ -> {E,clear(E,S)}
    end,
    case Error of abort_by_user -> skip; _ -> reply_error(E3,Error,S) end,
    S2.

proc(init,#handler{group=G,class=C,name=N}=H) ->
    wf:info(?M,"(~p) Converter worker started: ~p:~p:~p",[self(),G,C,N]),
    {ok,H};
proc(dump,#handler{state=#state{queue=Q,entry_map=EM,port_map=PM}=S}=H) ->
    io:format("~p: (~p) Converter status queue count: ~p, entries count ~p~n",[?M,self(),length(Q),maps:size(EM)]),
    io:format("  Queue list: ~p~n",[Q]),
    io:format("  Entry list:~n",[]), [ io:format("    | #{~p => ~p}~n",[EMk,EMv]) || {EMk,EMv} <- maps:to_list(EM) ],
    io:format("  Port map list:~n",[]), [ io:format("    | #{~p => ~p}~n",[PMk,PMv]) || {PMk,PMv} <- maps:to_list(PM) ],
    {reply,ok,H};
proc({insert_entry,#entry{id=Key,autostart=AS}=E},#handler{state=#state{queue=Q,entry_map=EM}=S}=H) ->
    wf:info(?M,"(~p) Insert entry ~p",[self(),Key]),
    Q2=Q++[Key],
    EM2=maps:put(Key,E#entry{stage=pending},EM),
    case AS of true -> self() ! {run_entry,Key}; _ -> skip end,
    {reply,ok,H#handler{state=S#state{queue=Q2,entry_map=EM2}}};
proc({command,check_queue},#handler{state=#state{queue=[]}}=H) ->
    {reply,ok,H};
proc({command,check_queue},#handler{state=#state{queue=Q,entry_map=EM}=S}=H) ->
    wf:info(?M,"(~p) Check queue (~p items)",[self(), length(Q)]),
    S3=case maps:get(hd(Q),EM) of
        #entry{stage=starting}=E ->
            case spawn_external(E,S) of
                {ok,S2} -> wf:info(?M,"(~p) Check queue: SPAWN OK",[self()]), S2;
                Error -> wf:info(?M,"(~p) Check queue: SPAWN ERROR",[self()]), error(E,Error,S)
            end;
        _ -> wf:info(?M,"(~p) Check queue: PROCESS",[self()]), S
    end,
    {reply,ok,H#handler{state=S3}};
proc({run_entry,Key},#handler{state=#state{entry_map=EM}=S}=H) ->    
    wf:info(?M,"(~p) Run entry",[self()]),
    case maps:get(Key,EM,false) of
        #entry{stage=pending,source=SP,storage=Storage,path=Path}=E ->
            case check_file(SP) of
                {ok,InfoA} ->
                    DP=destination(SP,Storage,Path,InfoA),
                    filelib:ensure_dir(DP),
                    self() ! {command,check_queue},
                    EM2=maps:update(Key,E#entry{stage=starting,infoA=InfoA,destination=DP},EM),
                    {reply,ok,H#handler{state=S#state{entry_map=EM2}}};
                Error ->
                    S2=error(E,Error,S),
                    {reply,error,H#handler{state=S2}}
            end;
        false -> {reply,ok,H}
    end;
    
proc({upadate_entry,Key,Fun},#handler{state=#state{entry_map=EM}=S}=H) when is_function(Fun,1) ->
    wf:info(?M,"(~p) Update entry",[self()]),
    EM2=case maps:get(Key,EM,false) of
        #entry{}=E ->  case Fun(E) of #entry{}=E2 -> maps:update(Key,E2,EM); _ -> EM end;
        false -> EM
    end,
    {reply,ok,H#handler{state=S#state{entry_map=EM2}}};
proc({remove_entries,Pid,Stage},#handler{state=#state{entry_map=EM}=S}=H) when is_pid(Pid) ->
    wf:info(?M,"(~p) Remove entries by {pid:~p,stage:~p}",[self(),Pid,Stage]),
    Fold=fun(Key,E,Acc) -> case E of #entry{stage=Stage,from=Pid} -> [Key|Acc]; _ -> Acc end end,
    [ self() ! {abort_entry,Key,false} || Key <- maps:fold(Fold,[],EM) ],
    {reply,ok,H};
proc({abort_entry,Key,User},#handler{state=#state{entry_map=EM}=S}=H) -> % TODO:
    wf:warning(?M,"(~p) Aborting ~p",[self(),Key]),
    Kill=fun(#entry{port=P}) -> % not for error/3, ONLY for abort
        wf:info(?M,"Kill port: ~p",[P]),
        case erlang:port_info(P, os_pid) of
            {os_pid, OsPid} ->
                Cmd="kill -9 "++wf:to_list(OsPid),
                wf:warning(?M,"Kill cmd: ~p",[Cmd]),
                os:cmd(Cmd);
            _ -> skip
        end
    end,
    
    S2=case maps:find(Key,EM) of
        {ok,#entry{}=E} when User =:= true -> Kill(E), error(E,abort_by_user,S);
        {ok,#entry{}=E} -> Kill(E), error(E,abort,S);
        error -> S
    end,
    {reply,ok,H#handler{state=S2}};
proc({Port,{exit_status,0}},#handler{state=#state{queue=Q,entry_map=EM,port_map=PM}=S}=H) ->
    wf:info(?M,"(~p) Spawn finished ~p",[self(),Port]),
    case find_entry(Port,S) of
        {ok,#entry{stage=finishing,destination=DP,req=Req}=E} ->
            case erlach_ban:check({attachment,hash,DP},Req) of
                ok ->
                    S2=finally(E,S),
                    self() ! {command,check_queue},
                    S3 = clear(E,S2),
                    {reply,ok,H#handler{state=S3}};
                Error ->
                    wf:warning(?M,"(~p) Skiping finishing (BANNED) ~p",[self(),Port]),
                    S2 = error(E,{error,Error},S),
                    {reply,error,H#handler{state=S2}}
            end;
        {ok,#entry{}=E} ->
            S3 = case spawn_external(E,S) of
                {ok,S2} -> S2;
                Error -> error(E,Error,S)
            end,
            {reply,ok,H#handler{state=S3}};
        error ->
            wf:warning(?M,"(~p) Skiping finishing ~p",[self(),Port]),
            self() ! {command,check_queue},
            {reply,ok,H#handler{state=S}}
    end;
    
% получены данные Data от внешнего приложения
% proc({Port,{data,Data}},H) -> {reply,ok,H};
% ответ на {Port, close}
% proc({Port,closed},H) -> {reply,ok,H};
% ответ на {Port, {connect, NewPid}}
% proc({Port,connected},H) -> {reply,ok,H};
% передается если при создании порта использовалась опция exit_status и внешнее приложение завершило свою работу   
proc({Port,{exit_status,_Status}=Reason},#handler{state=S}=H) when is_port(Port) ->
    wf:error(?M,"(~p) Spawn failed with status ~p ~p",[self(),Reason,Port]),
    S2=case find_entry(Port,S) of
        {ok,#entry{}=E} -> wf:error(?M,"Error calling... ~p",[Reason]), error(E,{error,Reason},S);
        _ -> wf:warning(?M,"(~p) Skiping clearing (entry not found) ~p",[self(),Port]), S
    end,
    {reply,error,H#handler{state=S2}};
% работа порта была прервана
proc({'EXIT',Port,Reason},H) ->
    wf:error(?M,"(~p) Spawn aborted with status ~p ~p",[self(),Reason,Port]),
    proc({Port,{exit,Reason}},H);
proc(Unknown,H) ->
    wf:warning(?M,"Unknown event ~p",[Unknown]),
    {reply,unknown,H}.

image_info_file(FileName) ->
    case file:read_file(FileName) of
        {ok,Data} ->
            try image_info(Data) of
                {ok,Info} -> {ok,setelement(#image_info.size,Info,filelib:file_size(FileName))};
                Error -> Error
            catch E:R ->
                wf:error(?M,"image_info/1 EVALUATION ERROR: {~p:~p}",[E,R]),
                {error,image_unknown_structure}
            end;
        Error -> Error
    end.

% http://dev.exiv2.org/projects/exiv2/wiki/The_Metadata_in_JPEG_files
% http://stackoverflow.com/a/17848968
% https://en.wikipedia.org/wiki/JPEG_File_Interchange_Format#JFIF_APP0_marker_segment
% http://vip.sugovica.hu/Sardi/kepnezo/JPEG%20File%20Layout%20and%20Format.htm <--
image_info(<<255,216,Data/binary>>) -> % jpeg
    Foreach=fun Foreach(Offset) ->
        case binary:part(Data,Offset,2) of
            <<16#ff,SOFn>> when SOFn =:= 16#c0 orelse SOFn =:= 16#c2 ->
                wf:info(?M,"SOFn ~p",[SOFn]),
                <<B:8,H:16/big,W:16/big,Comp:8>> =binary:part(Data,Offset+4,6),
                C=case Comp of 1 -> 'GreyScaled'; 3 -> 'YcbCr'; 4 -> 'CMYK' end,
                {ok,#jpeg{bit_depth=B,height=H,width=W,color_space=C}};
            <<16#ff,_>> ->
                <<L:16/big>> =binary:part(Data,Offset+2,2),
                Foreach(Offset+L+2);
            _ -> {error,corrupt_jpeg_data}
        end
    end,
    Foreach(0);
image_info(<<137,80,78,71,13,10,26,10,IHDR:25/binary,ChunkList/binary>>) -> % png / apng
    <<0,0,0,13,73,72,68,82,Data:13/binary,_CRC:32>>= IHDR,
    <<W:32/big,H:32/big,Depth:8,Color:8,_Compression:8,_Filter:8,_Interlace:8>>=Data,
    {ColorType,Alpha}=case Color of
        0 -> {greyscale,false};
        2 -> {truecolor,false};
        3 -> {indexed,  false};
        4 -> {grayscale,true};
        6 -> {truecolor,true}
    end,
    <<Size:32,Type:4/binary,Payload:Size/binary,CRC:32,ChunkList2/binary>> = ChunkList,
    Animation = case {Type,Payload} of
        {<<"acTL">>,<<NumFrames:32,NumPlays:32>>} -> true;
        _ -> false
    end,
    {ok,#png{width=W,height=H,bit_depth=Depth,color_type=ColorType,alpha=Alpha,animation=Animation}};

% http://www.w3.org/Graphics/GIF/spec-gif89a.txt
% http://giflib.sourceforge.net/whatsinagif/bits_and_bytes.html
% http://www.onicos.com/staff/iz/formats/gif.html
image_info(<<$G,$I,$F,$8,N,$a,W:16/little,H:16/little,_GCTF:1,_Res:3,_StCT:1,SzGCT:3,_BCI:8,_PAR:8,_/binary>>=Data) when N =:= $7 orelse N =:= $9 -> % gif
    DataForeach=fun DataForeach(DataOffset) ->
        case binary:at(Data,DataOffset) of
            0 -> DataOffset+1;
            DataSize -> DataForeach(DataOffset+DataSize+1)
        end
    end,
    
    ExtensionForeach=fun({ExtOffset,#gif{delays=D,alpha_frames=A}=G}) ->
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
                {ExtOffset+2+binary:at(Data,ExtOffset+1)+1,G#gif{delays=[Delay|D],alpha_frames=A+Transparent}}; % Graphic Control Extension Block
            1 -> {DataForeach(ExtOffset+14),G} % Plain Text Extension Block
        end
    end,
    
    BlockForeach=fun BlockForeach({BlockOffset,#gif{frames=F}=G}) ->
        case binary:at(Data,BlockOffset) of
            33 ->
                BlockForeach(ExtensionForeach({BlockOffset+1,G})); % Extension Block
            44 -> % Image Block
                SizeLCT=case <<(binary:at(Data,BlockOffset+9)):8>> of
                    <<0:1,_:7>> -> 0;
                    <<1:1,_:4,Size:3>> -> trunc(math:pow(2,Size+1)*3)
                end,
                BlockForeach({DataForeach(BlockOffset+10+SizeLCT+1),G#gif{frames=F+1}});
            59 ->
                {ok,G}
        end
    end,
    OffsetGCT=case SzGCT of 0 -> 0; _ -> trunc(math:pow(2,SzGCT+1)*3) end,
    {ok,Gif}=BlockForeach({13+OffsetGCT,#gif{alpha_frames=0,frames=0,loop_count=0,delays=[]}}),
    {ok,Gif#gif{animation=Gif#gif.frames > 1,alpha=Gif#gif.alpha_frames > 0,delays=lists:reverse(Gif#gif.delays),width=W,height=H}};
image_info(<<16#425047fb:32,PF:3/big,A1:1,BD:4/big,CS:4/big,E:1,A2:1,LR:1,AN:1,D:8/binary,_/bitstring>>) -> % bpg
    
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
                {0,0} -> false;
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
            extension_present = E =/= 0,
            limited_range = LR =/= 0,
            animation = AN =/= 0,
            width=W,
            height=H}};
        _ -> {error,bpg_wrong_data}
    end;
image_info(_) -> {error,unknown_image_format}.

test_apng() ->
    Path= <<"/tmp/image-with-alpha-channel.png">>,
    image_info_file(Path).
