#OPT=-g
OPT=-O2

test-args: test-args.c
	cc $(OPT) test-args.c -o test-args

diviter: diviter.c
	cc diviter.c  $(OPT) -fPIC -Wall -I/usr/local/include -L/usr/local/lib -Wl,--export-dynamic -c -o diviter.o
	cc diviter.o  /usr/local/share/cyclone/scheme/cyclone/common.o  /usr/local/share/cyclone/scheme/time.o /usr/local/share/cyclone/scheme/read.o /usr/local/share/cyclone/scheme/char.o /usr/local/share/cyclone/scheme/base.o  /usr/local/share/cyclone/scheme/write.o  -pthread -lcyclone -lck -lm -ltommath -ldl  $(OPT) -fPIC -Wall -I/usr/local/include -L/usr/local/lib -Wl,--export-dynamic -o diviter

ack: ack.c
	cc ack.c  $(OPT) -fPIC -Wall -I/usr/local/include -L/usr/local/lib -Wl,--export-dynamic -c -o ack.o
	cc ack.o  /usr/local/share/cyclone/scheme/cyclone/common.o  /usr/local/share/cyclone/scheme/time.o /usr/local/share/cyclone/scheme/read.o /usr/local/share/cyclone/scheme/char.o /usr/local/share/cyclone/scheme/base.o  /usr/local/share/cyclone/scheme/write.o  -pthread -lcyclone -lck -lm -ltommath -ldl  $(OPT) -fPIC -Wall -I/usr/local/include -L/usr/local/lib -Wl,--export-dynamic -o ack

fac-test: fac-test.c
	cc fac-test.c  $(OPT) -fPIC -Wall -I/usr/local/include -L/usr/local/lib -Wl,--export-dynamic -c -o fac-test.o
	cc fac-test.o  /usr/local/share/cyclone/scheme/cyclone/common.o  /usr/local/share/cyclone/scheme/base.o  /usr/local/share/cyclone/scheme/write.o  -pthread -lcyclone -lck -lm -ltommath -ldl  $(OPT) -fPIC -Wall -I/usr/local/include -L/usr/local/lib -Wl,--export-dynamic -o fac-test
 
.PHONY: clean
clean:
	rm -f *.o fac-test test-args ack
