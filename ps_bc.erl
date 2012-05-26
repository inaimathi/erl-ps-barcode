-module(ps_bc).

-behaviour(gen_server).

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([help/0, help/1, write/3, write/5, generate/2, generate/3, change/1, make_tempname/0]).

help() -> gen_server:call(?MODULE, help).
help(BarcodeType) -> gen_server:call(?MODULE, {help, BarcodeType}).
write(DestFolder, BarcodeType, Data) -> 
    write(DestFolder, BarcodeType, Data, 200, 200).
write(DestFolder, BarcodeType, Data, Width, Height) ->
    gen_server:call(?MODULE, {write, DestFolder, BarcodeType, Data, Width, Height}).
generate(BarcodeType, Data) -> generate("/tmp/", BarcodeType, Data).
generate(DestFolder, BarcodeType, Data) -> 
    NameOfTempFile = write(DestFolder, BarcodeType, Data),
    wand:process(NameOfTempFile),
    {NameOfTempFile, string:concat(NameOfTempFile, ".png"), string:concat(NameOfTempFile, ".ps")}.
change(TableId) -> gen_server:call(?MODULE, {change, TableId}).    

handle_call(help, _From, State) ->
    {reply, lists:append(ets:match(State, {'$1', encoder, '_', '_', '_', '_'})), State};
handle_call({help, BarcodeType}, _From, State) ->
    [[{example, Example}]] = ets:match(State, {BarcodeType, encoder, '_', '$1', '_', '_'}),
    {reply, Example, State};
handle_call({write, DestFolder, BarcodeType, Data, Width, Height}, _From, State) ->
    %% string:strip(os:cmd("mktemp -p /home/inaimathi/nitrogen/rel/nitrogen/site/static/images"), right, $\n),
    Fname = make_tempname(DestFolder),
    {ok, File} = file:open(Fname, [write, exclusive]),
    [[{requires, CompList}, {def_arg, ExArgs}]] = ets:match(State, {BarcodeType, encoder, '$1', '_', '$2', '_'}),
    file:write(File, io_lib:format("%!PS-Adobe-2.0\n%%BoundingBox: 0 0 ~w ~w\n%%LanguageLevel: 2\n", [Width, Height])),
    write_component(preamble, State, File),
    file:write(File, "\n/Helvetica findfont 10 scalefont setfont\n"),
    lists:map(fun (C) -> write_component(C, State, File) end, CompList),
    write_component(BarcodeType, State, File),
    write_barcode(File, BarcodeType, ExArgs, Data),
    file:close(File),
    {reply, Fname, State};
handle_call({change_table, Tab}, _From, _State) ->
    {reply, {watching_table, Tab}, Tab}.

make_tempname() ->
    {A, B, C} = now(),
    [D, E, F] = lists:map(fun integer_to_list/1, [A, B, C]),
    lists:append(["tmp.", D, ".", E, ".", F]).
make_tempname(TargetDir) ->
    filename:absname_join(TargetDir, make_tempname()).

write_component(preamble, Table, File) ->
    [[Pre]] = ets:match(Table, {preamble, '$1'}),
    file:write(File, Pre);
write_component(Name, Table, File) -> 
    file:write(File, lookup_component(Name, Table)).

write_barcode(File, datamatrix, _, Data)	-> format_barcode_string(File, datamatrix, "", Data);
write_barcode(File, BarcodeType, ExArgs, Data)	-> format_barcode_string(File, BarcodeType, string:join(ExArgs, " "), Data).

format_barcode_string(File, BarcodeType, ExArgString, DataString) ->
    io:format(File, "10 10 moveto (~s) (~s) /~s /uk.co.terryburton.bwipp findresource exec showpage",
	      [DataString, ExArgString, BarcodeType]).
		     
lookup_component(Name, Table) -> 
    Ren = ets:match(Table, {Name, renderer, '$1'}),
    Enc = ets:match(Table, {Name, encoder, '_', '_', '_', '$1'}),
    case {Ren, Enc} of
	{[], [[Res]]} -> Res;
	{[[Res]], []} -> Res
    end.

%%%%%%%%%%%%%%%%%%%% generic actions
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []). %% {local/global, Name}, Mod, InitArgs, Opts
stop() -> gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%% gen_server handlers
init([]) -> {ok, barcode_data:import_ets_file()}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
