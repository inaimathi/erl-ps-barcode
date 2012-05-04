-module(ps_bc).

start() ->
    spawn(fun() ->
		  register(barcode, self()),
		  TableId = barcode_data:read_default_file(),
		  loop(TableId)
	  end).

stop() -> barcode ! stop.

loop(TableId) ->
    receive
	{help} -> list_barcode_types;
	{help, BarcodeType} -> example_data_for_BarcodeType;
	{write, BarcodeType, Data} -> write_goes_here;

	{change, TableId} -> use_the_new_table;
	stop -> stop
    end.
	 
	    
