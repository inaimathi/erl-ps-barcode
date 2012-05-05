-module(ps_bc).
-export([start/1, stop/0]).
-export([help/0, help/1, write/2, change/1]).

start(TableId) ->
    spawn(fun() ->
		  register(barcode, self()),
		  loop(TableId)
	  end).
stop() -> send(stop).

help() -> send(help).
help(BarcodeType) -> send({help, BarcodeType}).
write(BarcodeType, Data) -> send({write, BarcodeType, Data}).
change(TableId) -> send({change, TableId}).    

send(Msg) -> 
    barcode ! {self(), Msg},
    receive 
	Result -> Result
    end.

loop(TableId) ->
    receive
	{Pid, help} -> 
	    Pid ! ets:match(TableId, {'$1', encoder, '_', '_', '_', '_'}),
	    loop(TableId);
	{Pid, {help, BarcodeType}} -> 
	    Pid ! ets:match(TableId, {BarcodeType, encoder, '_', '$2', '_', '_'}), 
	    loop(TableId);
	{Pid, {write, BarcodeType, Data}} -> 
	    Fname = string:strip(os:cmd("mktemp"), right, $\n),
	    {ok, File} = file:open(Fname, [append]),
	    [[{def_arg, ExArgs}, Body]] = ets:match(TableId, {BarcodeType, encoder, '_', '_', '$1', '$2'}),
	    [[Pre]] = ets:match(TableId, {preamble, '$1'}),
	    file:write(File, "%!PS-Adobe-2.0\n%%BoundingBox: 0 0 200 200\n%%LanguageLevel: 2\n"),
	    file:write(File, Pre),
	    file:write(File, "\n/Helvetica findfont 10 scalefont setfont\n"),
	    file:write(File, Body),
	    io:format(File, "10 10 moveto (~s) (~s) /~s /uk.co.terryburton.bwipp findresource exec showpage",
		      [Data, string:join(ExArgs, " "), BarcodeType]),
	    file:close(File),
	    Pid ! Fname,
	    loop(TableId);
	
	{Pid, {change, Tab}} -> 
	    Pid ! Tab, 
	    loop(Tab);
	{Pid, stop} -> 
	    Pid ! stopping
    end.
