-module(barcode_data).
-export([read_default_file/0, read_file/1]).
-export([export_ets_file/1, import_ets_file/0]).

export_ets_file(Table) -> ets:tab2file(Table, "ps-barcode-blocks").
import_ets_file() -> {ok, Tab} = ets:file2tab(filename:absname("ps-barcode-blocks")), Tab.

read_default_file() -> read_file("barcode.ps").
read_file(Filename) ->
    {ok, File} = file:open(Filename, [read]),
    TableId = ets:new(ps_barcode_blocks, [ordered_set]),
    trim_flash(File),
    {ok, Tab} = read_all_blocks(File, TableId),
    file:close(File),
    Tab.

trim_flash(IoDevice) -> read_until(IoDevice, "% --BEGIN TEMPLATE").

read_all_blocks(IoDevice, TableId) ->
    case Res = read_block(IoDevice) of
	[] -> {ok, TableId};
	_ -> ets:insert(TableId, parse_block(Res)),
	     read_all_blocks(IoDevice, TableId)
    end.

read_block(IoDevice) -> read_until(IoDevice, "% --END ").

parse_block([["%", "BEGIN", "PREAMBLE"] | Body]) ->
    {preamble, lists:append(Body)};
parse_block([["%", "BEGIN", "RENDERER", Name] | Body]) ->
    {list_to_atom(Name), renderer, lists:append(Body)};
parse_block([["%", "BEGIN", "ENCODER", Name] | Body]) ->
    parse_encoder_meta(Name, Body);
parse_block(_) -> {none}.

parse_encoder_meta (Name, Encoder) -> parse_encoder_meta(Name, Encoder, [], {[], [], []}).
parse_encoder_meta (Name, [["%", "RNDR:" | Renderers] | Rest], Acc, {_, R, S}) ->
    parse_encoder_meta(Name, Rest, Acc, {Renderers, R, S});
parse_encoder_meta (Name, [["%", "REQUIRES" | Reqs] | Rest], Acc, {A, _, S}) ->
    parse_encoder_meta(Name, Rest, Acc, {A, Reqs, S});
parse_encoder_meta (Name, [["%", "SUGGESTS" | Suggs] | Rest], Acc, {A, R, _}) ->
    parse_encoder_meta(Name, Rest, Acc, {A, R, Suggs});
parse_encoder_meta (Name, [["%", "EXOP:" | Exop] | Rest], Acc, Cmp) ->
    parse_encoder_meta(Name, Rest, [{def_arg, Exop} | Acc], Cmp);
parse_encoder_meta (Name, [["%", "EXAM:" | Exam] | Rest], Acc, Cmp) ->
    parse_encoder_meta(Name, Rest, [{example, string:join(Exam, " ")} | Acc], Cmp);
parse_encoder_meta (Name, [["%" | _] | Rest], Acc, Cmp) ->
    parse_encoder_meta(Name, Rest, Acc, Cmp);
parse_encoder_meta (Name, Body, [DefArgs, Example], {A, R, S}) ->
    Reqs = [list_to_atom(strip_nl(X)) || X <- lists:append([A, R, S])],
    {list_to_atom(Name), encoder, {requires, Reqs}, Example, DefArgs, lists:append(Body)}.

strip_nl(String) -> string:strip(String, right, $\n).

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
    [X || X <- re:split(strip_nl(Line), "( |--)", [{return, list}]),
	  X /= " ", X /= [], X /= "--", X /="\n"].
