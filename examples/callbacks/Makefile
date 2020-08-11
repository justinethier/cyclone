all: basic-no-gc full-with-gc

basic.o: basic.c
	cc -g -c basic.c

basic-no-gc: basic-no-gc.scm basic.o
	cyclone -CLNK basic.o basic-no-gc.scm

full-with-gc: full-with-gc.scm basic.o
	cyclone -CLNK basic.o full-with-gc.scm

.PHONY: clean
clean:
	rm -f *.o basic-no-gc.c basic-no-gc full-with-gc.c full-with-gc
