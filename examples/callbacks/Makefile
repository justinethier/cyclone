all: limited-scm-call-from-c complex-scm-call

sys.o: sys.c
	cc -g -c sys.c

limited-scm-call-from-c: limited-scm-call-from-c.scm sys.o
	cyclone -CLNK sys.o limited-scm-call-from-c.scm

complex-scm-call: complex-scm-call.scm sys.o
	cyclone -CLNK sys.o complex-scm-call.scm

.PHONY: clean
clean:
	rm -f *.o limited-scm-call-from-c.c limited-scm-call-from-c complex-scm-call.c complex-scm-call
