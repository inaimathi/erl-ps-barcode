-module(ps_barcode_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) -> ps_barcode_supervisor:start_link(StartArgs).
stop(_State) -> ok.
