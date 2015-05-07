TESTSCM = unit-tests
TESTFILES = $(addprefix tests/, $(addsuffix .scm, $(TESTSCM)))

all: cyclone icyc

trans.so: trans.scm
	csc -s trans.scm

cgen.so: cgen.scm
	csc -s cgen.scm

parser.so: parser.scm
	csc -s parser.scm

libcyclone.so.1: runtime.c runtime.h
	gcc -g -c -fPIC runtime.c -o runtime.o
	gcc -shared -Wl,-soname,libcyclone.so.1 -o libcyclone.so.1.0.1 runtime.o
libcyclone.a: runtime.c runtime.h
	gcc -g -c runtime.c -o runtime.o
	ar rcs libcyclone.a runtime.o
# Instructions from: http://www.adp-gmbh.ch/cpp/gcc/create_lib.html
# Note compiler will have to link to this, eg:
#Linking against static library
#gcc -static main.c -L. -lmean -o statically_linked
#Note: the first three letters (the lib) must not be specified, as well as the suffix (.a)

# debug compilation using static lib
.PHONY: debug
debug:
	gcc test.c -L. -lcyclone -I. -g -o test
.PHONY: debug2
debug2: libcyclone.so.1
	gcc test.c -L. -lcyclone -I. -g -o test

cyclone: cyclone.scm trans.so cgen.so parser.so libcyclone.a
	csc cyclone.scm

.PHONY: test
test: $(TESTFILES) cyclone
	$(foreach f,$(TESTSCM), echo tests/$(f) ; ./cyclone tests/$(f).scm && tests/$(f) && rm -rf tests/$(f);)

# A temporary testing directive
.PHONY: test2
test2: examples/hello-library/int-test/hello.c libcyclone.a
#	./cyclone -t examples/hello-library/hello.scm
#	./cyclone -t examples/hello-library/libs/lib2.sld
#	gcc examples/hello-library/int-test/lib2.c -I. -g -c -o lib2.o
#	gcc examples/hello-library/int-test/hello.c -L. -lcyclone -lm -I. -g -o hello
	gcc examples/hello-library/hello.c -L. -lcyclone -lm -I. -g -o hello

icyc: cyclone icyc.scm eval.scm parser.scm runtime.h
	./cyclone icyc.scm

.PHONY: tags
tags:
	ctags -R *

.PHONY: clean
clean:
	rm -rf a.out *.o *.so *.a *.out tags cyclone icyc
	$(foreach f,$(TESTSCM), rm -rf $(f) $(f).c tests/$(f).c;)
