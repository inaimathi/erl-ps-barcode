## erl-ps-barcode

Uses the [postscriptbarcode](http://code.google.com/p/postscriptbarcode/) Barcode Writer to generate various barcodes.

### Notes

Some barcodes still don't work. These include `isbn` and `ean13composite`. Probably the other composites too. **DO NOT USE THIS YET**

### Usage

1. Clone this git repo
2. Do `make` and `make run` in the project directory (you should get an error the first time you do this)
3. Evaluate `barcode_data:export_ets_file(barcode_data:read_default_file()).` (this only needs to be done the first time, the application loads the parsed file by itself in the future)
4. Exit the Erlang prompt and do `make run` again

You should now have a working barcode generator. 

`ps_bc:help/0` will list all available barcode types
`ps_bc:help/1` takes a barcode type and returns an example argument for that barcode type
`ps_bc:generate/2` takes a barcode type and a string and generates a barcode containing that string
`ps_bc:generate/3` takes `DestFolder`, `BarcodeType` and `Data` (a string) and generates the appropriate barcode in the `DestFolder` directory
