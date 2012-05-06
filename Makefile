.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

MODS = wand

all: ${MODS:%=%.beam} wand

wand: wand.c erl_comm.c driver.c
	gcc -o wand `pkg-config --cflags --libs MagickWand` wand.c erl_comm.c driver.c

clean:
	rm wand