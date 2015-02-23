TESTSCM = unit-tests
TESTFILES = $(addprefix tests/, $(addsuffix .scm, $(TESTSCM)))

all: cyclone icyc

trans.so: trans.scm
	csc -s trans.scm

cgen.so: cgen.scm
	csc -s cgen.scm

parser.so: parser.scm
	csc -s parser.scm

cyclone: cyclone.scm trans.so cgen.so parser.so
	csc cyclone.scm

.PHONY: test
test: $(TESTFILES) cyclone
	$(foreach f,$(TESTSCM), echo tests/$(f) ; ./cyclone tests/$(f).scm && tests/$(f) && rm -rf tests/$(f);)

icyc: cyclone icyc.scm eval.scm parser.scm runtime.h
	./cyclone icyc.scm

.PHONY: tags
tags:
	ctags -R *

.PHONY: clean
clean:
	rm -rf a.out *.o *.so *.c *.out tags cyclone icyc
	$(foreach f,$(TESTSCM), rm -rf $(f) $(f).c tests/$(f).c;)
