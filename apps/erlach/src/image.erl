-module(image).
-behaviour(gen_server).

-compile(export_all).

-include("erlach.hrl").

-define(EMAGICK_APP_ENV, [{magick_prefix, "gm"}]).

% static/attachments/cd25b27a050d2f06775143c816831fc18adc06ef1870e854ce8205272c253ea9.bpg

%% Limit RAM and HDD options / Monitoring of memory using
%% http://stackoverflow.com/questions/27458437/make-imagemagick-use-external-hdd-for-temporary-files


% Callback functions which should be exported
% -export([init/1]).
% -export([handle_cast/2, terminate/2]).

% user-defined interface functions
% -export([start_link/0]).

convert_async({Mime, Ext}, Data, Path, Quality, SuccFun) ->
    ?MODULE:start_link(),
    gen_server:cast(?MODULE, {convert, {{Mime, Ext}, Data, Path, Quality, SuccFun}}).

% gen_server:cast(image, calcbad).
% supervisor:start_child(web_sup, {image, {image, start_link, []}, temporary,10,worker,[]}).

start_link() -> gen_server:start_link({local,?MODULE},?MODULE,[],[]).
init(_Args) -> wf:info(?MODULE, "Started ~p", [self()]), {ok, []}.
handle_call(_Message,_From,State) -> {reply,invalid_call_command,State}.

% handle_cast(calc, State) ->
%      io:format("result 2+2=4~n"),
%      {noreply, State};
% handle_cast(calcbad, State) ->
%      io:format("result 1/0~n"),
%      1 / 0,
%      {noreply, State};

handle_cast({convert, {{Mime, Ext}, Data, Path, Quality, SuccFun}},State) ->
     % io:format("handle_cast :: convert~n"),
     Response = try
             {ok, Mime2, Converted} = convert({Mime, Ext}, Data, Quality),
             wf:info(?MODULE, "handle_cast ~p",[Path]),
             ok = file:write_file(Path, Converted),
             {ok, Mime2, size(Converted)}
         catch E:R -> {error, {E,R}} end,
     SuccFun(Response),
     {noreply, State}.

handle_info(_Message,State) -> {noreply,State}.
terminate(Reason,_State) -> wf:warning(?MODULE,"Terminated ~p",[Reason]), ok.
code_change(_OldVersion,State,_Extra) -> {ok,State}.

test() ->
    % {ok, Jpg} = file:read_file("/Users/m/Pictures/Girls/OBqKndqiqms.jpg"),
    % {ok, Jpg} = file:read_file("/Users/m/Pictures/Girls/OQst5Umr2s4.jpg"),
    % {ok, Jpg} = file:read_file("/Users/m/Pictures/Girls/peAQucKNmb0.jpg"),
    % {ok, Jpg} = file:read_file("/Users/m/Pictures/Girls/r5ijy39U1Jw.jpg"),
    {ok, Jpg} = file:read_file("/Users/m/Pictures/Girls/sj3OpNOTlyY.jpg"),
    S1 = size(Jpg),
    {ok, [Jpg2]} = emagick:convert(Jpg, jpg, jpg, [
            {strip}, % remove profile info & metadata
            {define, "jpeg:optimize-coding=true"}, % true by default for small images
            {define, "jpeg:dct-method=float"},
            {'sampling-factor', "4:2:2"}, % Industry-standard video subsampling notation
            {quality, "87"},
            % {define, "jpeg:preserve-settings"}, % To re-use the original sampling factors (and quality setting) when JPEG
                                                  % use for photo instead clipart
            {interlace, "Line"}, % Use Line to create an interlaced PNG or GIF or progressive JPEG image.
            % {'gaussian-blur', "0.05"}, % compress up to 2x
            % {unsharp, "2x0.5+0.7+0"},
            {filter, "Lanczos"}
        ], [{magick_prefix, "gm"}]),
    S2 = size(Jpg2),
    file:write_file("/Users/m/compressed.jpg",Jpg2),
    {S1,S2}.
    % {ok, Pdf} = file:read_file("/Users/m/Development/web/n2o-erlach-2/samples/deps/emagick/test/test.pdf"),
    % emagick:convert(Pdf, pdf, png, [{density, 200}], [{magick_prefix, "gm"}]).
    

mime_type(Binary) ->
    case <<Binary:64/bitstring>> of
        <<255,216,255, _,  _,  _,  _,  _>> -> ?MIME_IMAGE_JPEG;
        <<$G, $I, $F, $8, $7, $a,  _,  _>> -> ?MIME_IMAGE_GIF;
        <<$G, $I, $F, $8, $9, $a,  _,  _>> -> ?MIME_IMAGE_GIF;
        <<137,80, 78, 71, 13, 10, 26, 10>> -> ?MIME_IMAGE_PNG;
        <<66, 80, 71, 251, _,  _,  _,  _>> -> ?MIME_IMAGE_BPG;
        _ -> undefined
    end.


% TODO: convert(?MIME_IMAGE_JPEG, Binary)
convert(?MIME_IMAGE_JPEG=Mime, Binary, Quality) -> image:optim(Mime,Binary,Quality);
convert(?MIME_IMAGE_PNG=Mime,  Binary, Quality) -> image:optim(Mime,Binary,Quality);
convert(?MIME_IMAGE_GIF=Mime,  Binary, Quality) -> image:optim(Mime,Binary,Quality).
% convert(?MIME_IMAGE_PNG=Mime, Binary, _Quality) ->
%     {ok,GD} = gd:new(), % {ok,{gd,#Port<0.10123>}}
%     {ok,Ptr} = gd:image_create_from_png_ptr(GD, Binary),   % {ok,0}
%     {ok,Converted} = gd:image_png_ptr(GD, Ptr),
%     {ok, Mime, Converted};
% convert(?MIME_IMAGE_GIF=Mime, Binary, _Quality) ->
%     {ok,GD} = gd:new(), % {ok,{gd,#Port<0.10123>}}
%     {ok, Ptr} = gd:image_create_from_gif_ptr(GD, Binary),   % {ok,0}
%     {ok, Converted} = gd:image_gif_ptr(GD, Ptr),
%     {ok, Mime, Converted}.

optim({_, Ext}=Mime, Image, Quality) ->
    Opts = case {Mime,Quality} of
        {?MIME_IMAGE_JPEG, photo} -> [
                {strip},
                {define, "jpeg:optimize-coding=true"},
                {define, "jpeg:dct-method=float"},
                {define, "jpeg:preserve-settings"},
                {interlace, "Line"},
                {filter, "Lanczos"} ];
        {?MIME_IMAGE_JPEG, _} -> [
                {strip}, % remove profile info & metadata
                {define, "jpeg:optimize-coding=true"}, % true by default for small images
                {define, "jpeg:dct-method=float"},
                {'sampling-factor', "4:2:2"}, % Industry-standard video subsampling notation
                {quality, "87"},
                % {define, "jpeg:preserve-settings"}, % To re-use the original sampling factors (and quality setting) when JPEG
                                                      % use for photo instead clipart
                {interlace, "Line"}, % Use Line to create an interlaced PNG or GIF or progressive JPEG image.
                % {'gaussian-blur', "0.05"}, % compress up to 2x
                % {unsharp, "2x0.5+0.7+0"},
                {filter, "Lanczos"} ];
        {?MIME_IMAGE_GIF, _} -> [ {strip} ];
        {?MIME_IMAGE_PNG, _} -> [ {strip} ]
    end,
    
    wf:info(?MODULE, "Image converting  ~p ~p", [Opts,?EMAGICK_APP_ENV]),
    {ok, [Result]} = emagick:convert(Image, wf:to_atom(Ext), wf:to_atom(Ext), Opts, ?EMAGICK_APP_ENV),
    wf:info(?MODULE, "Image converting OK", []),
    {ok, Mime, Result}.
    

to_bpg({_, Ext}=Mime, Binary, Quality) ->
    Q = case {Mime,Quality} of
        {?MIME_IMAGE_JPEG, photo} -> 20;
        {?MIME_IMAGE_JPEG, _} -> 30
    end,

    Name = wf:temp_id(),
    TmpFname = wf:f("/tmp/~s.~s",[Name,Ext]),
    OutFname = wf:f("/tmp/~s.bpg",[Name]),
    Cmd = wf:f("bpgenc -q ~b -o ~s ~s",[Q, OutFname, TmpFname]),

    ok = file:write_file(TmpFname, Binary),

    wf:info(?MODULE, "Image converting to BPG ~p", [Cmd]),
    [] = os:cmd(Cmd), % successful
    wf:info(?MODULE, "Image converting to BPG.  Finish", []),
    ok = file:delete(TmpFname),

    {ok, Binary2} = file:read_file(OutFname),
    ok = file:delete(OutFname),
    {ok, ?MIME_IMAGE_BPG, Binary2}.




