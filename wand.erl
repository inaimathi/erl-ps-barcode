-module(wand).
-export([start/0, stop/0, restart/0]).
-export([thumbnail/1]).

start() ->
    spawn(fun() ->
		  register(wand, self()),
		  process_flag(trap_exit, true),
		  Port = open_port({spawn, "./wand"}, [{packet, 2}]),
		  loop(Port)
	  end).

stop() -> wand ! stop.

restart() -> stop(), start().

thumbnail(Filename) ->
    call_port(Filename).

call_port(Msg) ->
    wand ! {call, self(), Msg},
    receive
	{wand, Result} ->
	    Result
    end.

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, Msg}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {wand, decode(Data)}    
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    exit({port_terminated, Reason})
    end.

decode([0]) -> {ok, 0};
decode([1]) -> {error, could_not_read};
decode([2]) -> {error, could_not_write}.
