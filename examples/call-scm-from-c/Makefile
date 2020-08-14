all: basic-no-gc full-with-gc

basic.o: basic.c
	cc -c basic.c

basic-no-gc: basic-no-gc.scm basic.o
	cyclone -COBJ "basic.o" basic-no-gc.scm

full.o: full.c
	cc -c full.c

full-with-gc: full-with-gc.scm full.o
	cyclone -COBJ full.o full-with-gc.scm

.PHONY: clean
clean:
	rm -f *.o basic-no-gc.c basic-no-gc full-with-gc.c full-with-gc
