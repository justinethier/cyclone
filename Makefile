TESTSCM = unit-tests
TESTFILES = $(addprefix tests/, $(addsuffix .scm, $(TESTSCM)))

all: cyclone

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

repl: cyclone repl.scm eval.scm parser.scm
	./cyclone repl.scm

.PHONY: tags
tags:
	ctags -R *

.PHONY: clean
clean:
	rm -rf a.out *.o *.so *.c *.out tags cyclone repl
	$(foreach f,$(TESTSCM), rm -rf $(f) $(f).c tests/$(f).c;)
