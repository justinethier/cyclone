fac-test: fac-test.c
	cc fac-test.c  -O2 -fPIC -Wall -I/usr/local/include -L/usr/local/lib -Wl,--export-dynamic -c -o fac-test.o
	cc fac-test.o  /usr/local/share/cyclone/scheme/cyclone/common.o  /usr/local/share/cyclone/scheme/base.o  /usr/local/share/cyclone/scheme/write.o  -pthread -lcyclone -lck -lm -ltommath -ldl  -O2 -fPIC -Wall -I/usr/local/include -L/usr/local/lib -Wl,--export-dynamic -o fac-test
