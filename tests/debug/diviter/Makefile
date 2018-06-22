diviter: diviter.c
	cc diviter.c -O2 -fPIC -Wall -I/usr/local/include -L/usr/local/lib -Wl,--export-dynamic -c -o diviter.o
	cc diviter.o  /usr/local/share/cyclone/scheme/cyclone/common.o  /usr/local/share/cyclone/scheme/base.o  /usr/local/share/cyclone/scheme/time.o  /usr/local/share/cyclone/scheme/write.o  /usr/local/share/cyclone/scheme/char.o  /usr/local/share/cyclone/scheme/read.o  -pthread -lcyclone -lck -lm -ltommath -ldl -O2 -fPIC -Wall -I/usr/local/include -L/usr/local/lib -Wl,--export-dynamic -o diviter

.PHONY: clean

clean:
	rm -rf diviter *.o
