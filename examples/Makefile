# Build all example programs
TARGETS = \
  tail-call-optimization \
  begin-splicing \
  fac \
  long-running-process \
  networking/client \
  networking/server \
  threading/cv-broadcast \
  threading/many-writers \
  threading/producer-consumer \
  threading/thread-join \
  threading/sum-atomic \
  threading/sum-mutex \
  threading/sum-nosync \
  game-of-life/life \
  hello-library/hello \

SRCFILES = $(addsuffix .scm, $(TARGETS))

all: $(TARGETS)

tail-call-optimization     : tail-call-optimization.scm
	cyclone $^
begin-splicing             : begin-splicing.scm
	cyclone $^
fac                        : fac.scm
	cyclone $^
long-running-process       : long-running-process.scm
	cyclone $^
networking/client: networking/client.scm
	cyclone $^
networking/server: networking/server.scm
	cyclone $^
threading/cv-broadcast     : threading/cv-broadcast.scm
	cyclone $^
threading/many-writers     : threading/many-writers.scm
	cyclone $^
threading/producer-consumer: threading/producer-consumer.scm
	cyclone $^
threading/thread-join      : threading/thread-join.scm
	cyclone $^
threading/sum-atomic   : threading/sum-atomic.scm
	cyclone $^
threading/sum-mutex    :  threading/sum-mutex.scm
	cyclone $^
threading/sum-nosync   : threading/sum-nosync.scm
	cyclone $^
game-of-life/life:
	cd game-of-life ; make
hello-library/hello:
	cd hello-library ; make

.PHONY: clean
clean:
	rm -rf *.o *.c *.meta $(TARGETS)
	cd threading ; rm -rf *.o *.c *.meta
	cd game-of-life ; make clean
	cd hello-library ; make clean
	cd networking ; rm -rf client.c server.c

