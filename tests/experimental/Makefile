OPT=-g
#OPT=-O2

test-args: test-args.c
	cc -O2 test-args.c -o test-args

fac-test: fac-test.c
	cc fac-test.c  $(OPT) -fPIC -Wall -I/usr/local/include -L/usr/local/lib -Wl,--export-dynamic -c -o fac-test.o
	cc fac-test.o  /usr/local/share/cyclone/scheme/cyclone/common.o  /usr/local/share/cyclone/scheme/base.o  /usr/local/share/cyclone/scheme/write.o  -pthread -lcyclone -lck -lm -ltommath -ldl  $(OPT) -fPIC -Wall -I/usr/local/include -L/usr/local/lib -Wl,--export-dynamic -o fac-test
 
.PHONY: clean
clean:
	rm -f *.o fac-test test-args
