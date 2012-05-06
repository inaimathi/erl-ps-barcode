-module(ps_bc).

-behaviour(gen_server).

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([help/0, help/1, write/2, change/1]).

help() -> gen_server:call(?MODULE, help).
help(BarcodeType) -> gen_server:call(?MODULE, {help, BarcodeType}).
write(BarcodeType, Data) -> gen_server:call(?MODULE, {write, BarcodeType, Data}).
change(TableId) -> gen_server:call(?MODULE, {change, TableId}).    


write_component(preamble, Table, File) ->
    [[Pre]] = ets:match(Table, {preamble, '$1'}),
    file:write(File, Pre);
write_component(Name, Table, File) -> 
    file:write(File, lookup_component(Name, Table)).
		     
lookup_component(Name, Table) -> 
    Ren = ets:match(Table, {Name, renderer, '$1'}),
    Enc = ets:match(Table, {Name, encoder, '_', '_', '_', '$1'}),
    case {Ren, Enc} of
	{[], [[Res]]} -> Res;
	{[[Res]], []} -> Res
    end.	    

handle_call(help, _From, State) ->
    {reply, ets:match(State, {'$1', encoder, '_', '_', '_', '_'}), State};
handle_call({help, BarcodeType}, _From, State) ->
    {reply, ets:match(State, {BarcodeType, encoder, '_', '$1', '_', '_'}), State};
handle_call({write, BarcodeType, Data}, _From, State) ->
    Fname = string:strip(os:cmd("mktemp"), right, $\n),
    {ok, File} = file:open(Fname, [append]),
    [[{requires, CompList}, {def_arg, ExArgs}]] = ets:match(State, {BarcodeType, encoder, '$1', '_', '$2', '_'}),
    file:write(File, "%!PS-Adobe-2.0\n%%BoundingBox: 0 0 200 200\n%%LanguageLevel: 2\n"),
    write_component(preamble, State, File),
    file:write(File, "\n/Helvetica findfont 10 scalefont setfont\n"),
    lists:map(fun (C) -> write_component(C, State, File) end, CompList),
    write_component(BarcodeType, State, File),
    io:format(File, "10 10 moveto (~s) (~s) /~s /uk.co.terryburton.bwipp findresource exec showpage",
	      [Data, string:join(ExArgs, " "), BarcodeType]),
    file:close(File),
    {reply, Fname, State};
handle_call({change_table, Tab}, _From, _State) ->
    {reply, {watching_table, Tab}, Tab}.

%%%%%%%%%%%%%%%%%%%% generic actions
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []). %% {local/global, Name}, Mod, InitArgs, Opts
stop() -> gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%% gen_server handlers
init([]) -> {ok, barcode_data:import_ets_file()}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
