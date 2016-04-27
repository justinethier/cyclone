# Build all example programs
TARGETS = \
  tail-call-optimization \
  begin-splicing \
  fac \
  long-running-process \
  threading/cv-broadcast \
  threading/many-writers \
  threading/producer-consumer \
  threading/thread-join \
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
threading/cv-broadcast     : threading/cv-broadcast.scm
	cyclone $^
threading/many-writers     : threading/many-writers.scm
	cyclone $^
threading/producer-consumer: threading/producer-consumer.scm
	cyclone $^
threading/thread-join      : threading/thread-join.scm
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
