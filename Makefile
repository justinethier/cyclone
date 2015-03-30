TESTSCM = unit-tests
TESTFILES = $(addprefix tests/, $(addsuffix .scm, $(TESTSCM)))

all: cyclone icyc

trans.so: trans.scm
	csc -s trans.scm

cgen.so: cgen.scm
	csc -s cgen.scm

parser.so: parser.scm
	csc -s parser.scm

libcyclone.a: runtime.c runtime.h
	gcc -c runtime.c -o runtime.o
	ar rcs libcyclone.a runtime.o
# Instructions from: http://www.adp-gmbh.ch/cpp/gcc/create_lib.html
# Note compiler will have to link to this, eg:
#Linking against static library
#gcc -static main.c -L. -lmean -o statically_linked
#Note: the first three letters (the lib) must not be specified, as well as the suffix (.a)

cyclone: cyclone.scm trans.so cgen.so parser.so libcyclone.a
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
	rm -rf a.out *.o *.so *.a *.out tags cyclone icyc
	$(foreach f,$(TESTSCM), rm -rf $(f) $(f).c tests/$(f).c;)
