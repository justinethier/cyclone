all: limited-scm-call-from-c sample-app2

sys.o: sys.c
	cc -g -c sys.c

limited-scm-call-from-c: limited-scm-call-from-c.scm sys.o
	cyclone -CLNK sys.o limited-scm-call-from-c.scm

sample-app2: sample-app2.scm sys.o
	cyclone -CLNK sys.o sample-app2.scm

.PHONY: clean
clean:
	rm -f *.o limited-scm-call-from-c.c limited-scm-call-from-c sample-app2.c sample-app2
