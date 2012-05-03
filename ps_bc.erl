-module(ps_bc).
-export([read_until/2, match_groups/2]).
-export([read_block/1, open_bc_file/0, trim_flash/1, parse_block/1]).

%% -record(block, {name, renderer, requires, suggests, default_args, example_data, body}).

open_bc_file() -> 
    {ok, File} = file:open("barcode.ps", [read]), 
    trim_flash(File),
    File.

trim_flash(IoDevice) -> read_until(IoDevice, "% --BEGIN TEMPLATE").

read_block(IoDevice) -> read_until(IoDevice, "% --END ").

parse_block([["BEGIN", "PREAMBLE"] | Body]) ->
    {preamble, Body};
parse_block([["BEGIN", "RENDERER", Name] | Body]) ->
    {renderer, Name, Body};
parse_block([["BEGIN", "ENCODER", Name] | Body]) ->
    {encoder, Name, Body};
parse_block(_) -> {none}.

read_until(IoDevice, StartsWith) -> read_until(IoDevice, StartsWith, []).
read_until(IoDevice, StartsWith, Acc) ->
    case file:read_line(IoDevice) of
	{ok, "\n"} ->
	    read_until(IoDevice, StartsWith, Acc);
	{ok, Line} -> 
	    case lists:prefix(StartsWith, Line) of
		true -> 
		    lists:reverse(Acc);
		false -> 
		    read_until(IoDevice, StartsWith, 
			       [process_line(Line) | Acc])
	    end;
	{error, _} -> error;
	eof -> lists:reverse(Acc)
    end.

process_line(Line) ->
    case lists:prefix("% --", Line) of
	true -> split_directive_line(Line);
	false -> Line
    end.

split_directive_line(Line) ->
    [X || X <- re:split(Line, "( |--)", [{return, list}]),
	  X /= "%", X /= " ", X /= [], X /= "--", X /="\n"].

match_groups(Str, Regex) ->
    case re:run(Str, Regex) of
	{match, [_ | Groups]} ->
	    substrings(Str, Groups);
	nomatch ->
	    nomatch
    end.

substrings(Str, Coords) -> substrings(Str, Coords, []).
substrings(_, [], Acc) -> lists:reverse(Acc);
substrings(Str, [{Start, Len} | Rest], Acc) ->
    Sub = string:substr(Str, Start + 1, Len),
    substrings(Str, Rest, [Sub | Acc]).
