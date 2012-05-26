-module(wand).

-behaviour(gen_server).

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([process/1]).

process(Filename) -> gen_server:call(?MODULE, {process_barcode, Filename}).

handle_call({process_barcode, Filename}, _From, State) ->
    State ! {self(), {command, Filename}},
    receive
	{State, {data, Data}} ->
	    {reply, decode(Data), State}
    after 3000 ->
	    exit(wand_timeout)
    end;
handle_call({'EXIT', _Port, Reason}, _From, _State) ->
    exit({port_terminated, Reason}).

decode([0]) -> {ok, 0};
decode([1]) -> {error, could_not_read};
decode([2]) -> {error, could_not_write}.

%%%%%%%%%%%%%%%%%%%% generic actions
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%% gen_server handlers
init([]) -> {ok, open_port({spawn, filename:absname("wand")}, [{packet, 2}])}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, State) -> State ! {self(), close}, ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
