all: calls

sys.o: sys.c
	cc -c sys.c

calls.c: calls.scm
	cyclone -d calls.scm

calls: sys.o calls.c
	cc calls.c  -O2 -Wall -I/usr/local/include -c -o calls.o
	cc calls.o  /usr/local/share/cyclone/scheme/cyclone/common.o  /usr/local/share/cyclone/scheme/base.o  /usr/local/share/cyclone/srfi/18.o  /usr/local/share/cyclone/scheme/write.o  -pthread -lcyclone -lck -lm -lcyclonebn -ldl  -O2 -fPIC -Wall -Wno-shift-negative-value -Wno-unused-command-line-argument -I/usr/local/include -L/usr/local/lib -Wl,--export-dynamic -o calls



.PHONY: clean
clean:
	rm -f *.o calls.c calls
