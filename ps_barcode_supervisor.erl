-module(ps_barcode_supervisor).
-behavior(supervisor).

-export([start/0, start_for_testing/0, start_link/1, init/1]).

start() ->
    spawn(fun() -> supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []) end).

start_for_testing() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []),
    unlink(Pid).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).


init([]) ->
    case filelib:is_regular(filename:absname("ps-barcode-blocks")) of
	false -> barcode_data:export_ets_file(barcode_data:read_default_file());
	true -> ok
    end,
    {ok, {{one_for_one, 3, 10},
	  [{tag1, 
	    {wand, start, []},
	    permanent,
	    10000,
	    worker,
	    [wand]},
	   {tag2,
	    {ps_bc, start, []},
	    permanent,
	    10000,
	    worker,
	    [ps_bc]}]}}.
