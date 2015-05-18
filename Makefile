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

##############################
# Temporary testing directives
.PHONY: test2
test2: examples/hello-library/int-test/hello.c libcyclone.a
	./cyclone scheme/base.sld
	cd examples/hello-library ; ../../cyclone libs/lib1.sld
	cd examples/hello-library ; ../../cyclone libs/lib2.sld
	cd examples/hello-library ; ../../cyclone hello.scm
#	gcc examples/hello-library/int-test/lib2.c -I. -g -c -o lib2.o
#	gcc examples/hello-library/int-test/hello.c -I. -g -c -o hello.o
#	gcc hello.o lib2.o -L. -lcyclone -lm -o hello
#	gcc examples/hello-library/hello.c -L. -lcyclone -lm -I. -g -o hello
# TODO: will need to manually compile hello example. need to manually add the entry points to hello, and there is an issue in the libs with Cyc_global_vars not being assigned. but this still leads to a tag error so there must be other issues...
.PHONY: test3
test3:
	gcc examples/hello-library/int-test/scheme/base.c -I/home/justin/Documents/cyclone/ -g -c -o scheme/base.o
	cd examples/hello-library ; gcc int-test/libs/lib1.c -I/home/justin/Documents/cyclone/ -g -c -o libs/lib1.o
	cd examples/hello-library ; gcc int-test/libs/lib2.c -I/home/justin/Documents/cyclone/ -g -c -o libs/lib2.o
	cd examples/hello-library ; gcc int-test/hello.c -I/home/justin/Documents/cyclone/ -g -c -o hello.o
	cd examples/hello-library ; gcc hello.o  libs/lib1.o  /home/justin/Documents/cyclone/scheme/base.o  libs/lib2.o  -L/home/justin/Documents/cyclone/ -lcyclone -lm -I/home/justin/Documents/cyclone/ -g -o hello
# END temporary directives
##########################

icyc: cyclone icyc.scm eval.scm parser.scm runtime.h
	./cyclone icyc.scm

.PHONY: tags
tags:
	ctags -R *

.PHONY: clean
clean:
	rm -rf a.out *.o *.so *.a *.out tags cyclone icyc
	$(foreach f,$(TESTSCM), rm -rf $(f) $(f).c tests/$(f).c;)
