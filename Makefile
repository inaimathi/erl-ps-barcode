all: *.erl *.c
	make wand
	erlc -W *erl

wand: wand.c erl_comm.c driver.c
	gcc -o wand `pkg-config --cflags --libs MagickWand` wand.c erl_comm.c driver.c

clean:
	rm wand	
	rm *beam