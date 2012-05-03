-module(ps_bc).
-export([read_until/2]).
-export([read_block/1, open_bc_file/0, trim_flash/1, parse_block/1]).
%% -export([sort_preamble/1]).
-export([test/0, parse_encoder_meta/1]).

%% -record(encoder, 
%%         {name, requires, default_args, example_data, body}).

test() ->
    F = open_bc_file(),
    Preamble = parse_block(read_block(F)),
    Renderer = parse_block(read_block(F)),
    read_block(F), read_block(F),
    Encoder = parse_block(read_block(F)),
    file:close(F),
    {Preamble, Renderer, Encoder}.

open_bc_file() -> 
    {ok, File} = file:open("barcode.ps", [read]), 
    trim_flash(File),
    File.

trim_flash(IoDevice) -> read_until(IoDevice, "% --BEGIN TEMPLATE").

read_block(IoDevice) -> read_until(IoDevice, "% --END ").

parse_block([["%", "BEGIN", "PREAMBLE"] | Body]) ->
    {preamble, lists:append(Body)};
parse_block([["%", "BEGIN", "RENDERER", Name] | Body]) ->
    {renderer, Name, lists:append(Body)};
parse_block([["%", "BEGIN", "ENCODER", Name] | Body]) ->
    {encoder, Name, parse_encoder_meta(Body)};
parse_block(_) -> {none}.

parse_encoder_meta (Encoder) -> parse_encoder_meta(Encoder, [], {[], [], []}).
parse_encoder_meta ([["%", "RNDR:" | Renderers] | Rest], Acc, {_, R, S}) ->
    parse_encoder_meta(Rest, Acc, {Renderers, R, S});
parse_encoder_meta ([["%", "REQUIRES" | Reqs] | Rest], Acc, {A, _, S}) ->
    parse_encoder_meta(Rest, Acc, {A, Reqs, S});
parse_encoder_meta ([["%", "SUGGESTS" | Suggs] | Rest], Acc, {A, R, _}) ->
    parse_encoder_meta(Rest, Acc, {A, R, Suggs});
parse_encoder_meta ([["%", "EXOP:" | Exop] | Rest], Acc, Cmp) ->
    parse_encoder_meta(Rest, [{default_args, strip_map(Exop)} | Acc], Cmp);
parse_encoder_meta ([["%", "EXAM:" | Exam] | Rest], Acc, Cmp) ->
    parse_encoder_meta(Rest, [{example_data, strip_map(Exam)} | Acc], Cmp);
parse_encoder_meta ([["%" | _] | Rest], Acc, Cmp) ->
    parse_encoder_meta(Rest, Acc, Cmp);
parse_encoder_meta (Body, Acc, {A, R, S}) ->
    Reqs = [strip_nl(X) || X <- lists:append([A, R, S])],
    [{requires, Reqs} | lists:reverse([lists:append(Body) | Acc])].

strip_nl(String) -> string:strip(String, right, $\n).
strip_map(List) -> lists:map(fun strip_nl/1, List).

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
	  X /= " ", X /= [], X /= "--", X /="\n"].
