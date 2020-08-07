all: sample-app

sys.o: sys.c
	cc -g -c sys.c

sample-app: sample-app.scm sys.o
	cyclone -CLNK sys.o sample-app.scm

.PHONY: clean
clean:
	rm -f *.o sample-app.c sample-app
