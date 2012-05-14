all: *.erl *.c
	make wand
	erlc -W *erl

run: 
	erl -name ps_barcode@127.0.1.1 -eval 'application:load(ps_barcode).' -eval 'application:start(ps_barcode).'

wand: wand.c erl_comm.c driver.c
	gcc -o wand `pkg-config --cflags --libs MagickWand` wand.c erl_comm.c driver.c

clean:
	rm wand	
	rm *beam