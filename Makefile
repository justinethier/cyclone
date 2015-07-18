# Cyclone Scheme
# Copyright (c) 2014, Justin Ethier
# All rights reserved.

include Makefile.config

TESTSCM = unit-tests
TESTFILES = $(addprefix tests/, $(addsuffix .scm, $(TESTSCM)))

all: cyclone icyc

scheme/base.o: cyclone scheme/base.sld
	./cyclone scheme/base.sld

scheme/char.o: cyclone scheme/char.sld
	./cyclone scheme/char.sld

scheme/eval.o: cyclone scheme/eval.sld eval.scm
	./cyclone scheme/eval.sld

scheme/file.o: cyclone scheme/file.sld
	./cyclone scheme/file.sld

scheme/read.o: cyclone scheme/read.sld parser.scm
	./cyclone scheme/read.sld

scheme/write.o: cyclone scheme/write.sld
	./cyclone scheme/write.sld

scheme/cyclone/common.o: scheme/cyclone/common.sld
	./cyclone scheme/cyclone/common.sld

scheme/cyclone/util.o: scheme/cyclone/util.sld
	./cyclone scheme/cyclone/util.sld

common.so: scheme/cyclone/common.scm
	csc -s scheme/cyclone/common.scm

util.so: util.scm
	csc -s util.scm

transforms.so: transforms.scm
	csc -s transforms.scm

cgen.so: cgen.scm
	csc -s cgen.scm

libraries.so: libraries.scm
	csc -s libraries.scm

parser.so: parser.scm
	csc -s parser.scm

libcyclone.so.1: runtime.c runtime.h
	gcc -g -c -fPIC runtime.c -o runtime.o
	gcc -shared -Wl,-soname,libcyclone.so.1 -o libcyclone.so.1.0.1 runtime.o
libcyclone.a: runtime.c runtime.h dispatch.c
	$(CC) -g -c dispatch.c -o dispatch.o
	$(CC) -g -c -DCYC_INSTALL_DIR=\"$(PREFIX)\" -DCYC_INSTALL_LIB=\"$(LIBDIR)\" -DCYC_INSTALL_INC=\"$(INCDIR)\" -DCYC_INSTALL_SLD=\"$(DATADIR)\" runtime.c -o runtime.o
	$(AR) rcs libcyclone.a runtime.o dispatch.o
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

cyclone: cyclone.scm transforms.so util.so cgen.so libraries.so parser.so common.so libcyclone.a
	csc cyclone.scm

#scheme/cyclone/util.o: cyclone scheme/cyclone/util.sld
.PHONY: self
self:
	./cyclone scheme/cyclone/common.sld
	./cyclone scheme/cyclone/libraries.sld
	./cyclone scheme/cyclone/transforms.sld
	./cyclone scheme/cyclone/cgen.sld
	./cyclone scheme/cyclone/util.sld
	./cyclone cyclone-self.scm

.PHONY: self2
self2:
	./cyclone-self scheme/base.sld
	./cyclone-self scheme/read.sld
	./cyclone-self scheme/write.sld
	./cyclone-self scheme/char.sld
	./cyclone-self scheme/eval.sld
	./cyclone-self scheme/file.sld
	./cyclone-self scheme/cyclone/common.sld
	./cyclone-self icyc.scm
	./cyclone-self scheme/cyclone/libraries.sld
	./cyclone-self scheme/cyclone/transforms.sld
	./cyclone-self scheme/cyclone/cgen.sld
	./cyclone-self scheme/cyclone/util.sld
	./cyclone-self cyclone-self.scm

# TODO: this is ugly and needs lots of work yet...
# would also need to call this after self2
# TODO: also need to take all the .sld files (??), and will need to configure.
#  or not, is it OK to assume bootstrap will only generate cyclone and (maybe) icyc??
# install location (although maybe that is a 'make install' issue rather than bootstrap,
# which can be done from a fixed location)
.PHONY: bootstrap
bootstrap:
#	$(MAKE) self2
	rm -rf tmp
	mkdir -p tmp/scheme/cyclone
	cp cyclone.h tmp
	cp runtime-main.h tmp
	cp runtime.h tmp
	cp runtime.c tmp
	cp dispatch.c tmp
	cp scheme/base.c tmp/scheme
	cp scheme/read.c tmp/scheme
	cp scheme/write.c tmp/scheme
	cp scheme/char.c tmp/scheme
	cp scheme/eval.c tmp/scheme
	cp scheme/file.c tmp/scheme
	cp scheme/cyclone/common.c tmp/scheme/cyclone
	cp icyc.scm tmp
	cp tests/unit-tests.scm tmp
	cp scheme/cyclone/libraries.c tmp/scheme/cyclone
	cp scheme/cyclone/transforms.c tmp/scheme/cyclone
	cp scheme/cyclone/cgen.c tmp/scheme/cyclone
	cp scheme/cyclone/util.c tmp/scheme/cyclone
	cp cyclone-self.c tmp/cyclone.c
	cp Makefile-bootstrap tmp/Makefile
	cp Makefile.config tmp/Makefile.config


.PHONY: test
test: $(TESTFILES) cyclone
	$(foreach f,$(TESTSCM), echo tests/$(f) ; ./cyclone tests/$(f).scm && tests/$(f) && rm -rf tests/$(f);)

###############################
## Temporary testing directives
.PHONY: test2
test2: examples/hello-library/int-test/hello.c libcyclone.a
	cd examples/hello-library ; ../../cyclone libs/lib1.sld
	cd examples/hello-library ; ../../cyclone libs/lib2.sld
	cd examples/hello-library ; ../../cyclone hello.scm
##	gcc examples/hello-library/int-test/lib2.c -I. -g -c -o lib2.o
##	gcc examples/hello-library/int-test/hello.c -I. -g -c -o hello.o
##	gcc hello.o lib2.o -L. -lcyclone -lm -o hello
##	gcc examples/hello-library/hello.c -L. -lcyclone -lm -I. -g -o hello
## TODO: will need to manually compile hello example. need to manually add the entry points to hello, and there is an issue in the libs with Cyc_global_vars not being assigned. but this still leads to a tag error so there must be other issues...
#.PHONY: test3
#test3:
#	gcc examples/hello-library/int-test/scheme/base.c -I/home/justin/Documents/cyclone/ -g -c -o scheme/base.o
#	cd examples/hello-library ; gcc int-test/libs/lib1.c -I/home/justin/Documents/cyclone/ -g -c -o libs/lib1.o
#	cd examples/hello-library ; gcc int-test/libs/lib2.c -I/home/justin/Documents/cyclone/ -g -c -o libs/lib2.o
#	cd examples/hello-library ; gcc int-test/hello.c -I/home/justin/Documents/cyclone/ -g -c -o hello.o
#	cd examples/hello-library ; gcc hello.o  libs/lib1.o  /home/justin/Documents/cyclone/scheme/base.o  libs/lib2.o  -L/home/justin/Documents/cyclone/ -lcyclone -lm -I/home/justin/Documents/cyclone/ -g -o hello
## END temporary directives
###########################

icyc: cyclone icyc.scm eval.scm libraries.scm parser.scm runtime.h scheme/base.o scheme/read.o scheme/write.o scheme/char.o scheme/eval.o scheme/file.o scheme/cyclone/util.o scheme/cyclone/common.o scheme/cyclone/util.o
	./cyclone icyc.scm

.PHONY: tags
tags:
	ctags -R *

.PHONY: clean
clean:
	rm -rf a.out *.o *.so *.a *.out tags cyclone icyc scheme/*.o scheme/*.c
	$(foreach f,$(TESTSCM), rm -rf $(f) $(f).c tests/$(f).c;)

install:
	$(MKDIR) $(DESTDIR)$(BINDIR)
	$(MKDIR) $(DESTDIR)$(LIBDIR)
	$(MKDIR) $(DESTDIR)$(INCDIR)
	$(MKDIR) $(DESTDIR)$(DATADIR)
	$(MKDIR) $(DESTDIR)$(DATADIR)/scheme/cyclone
	$(INSTALL) -m0755 cyclone $(DESTDIR)$(BINDIR)/
	$(INSTALL) -m0755 icyc $(DESTDIR)$(BINDIR)/
	$(INSTALL) -m0644 libcyclone.a $(DESTDIR)$(LIBDIR)/
	$(INSTALL) -m0644 *.h $(DESTDIR)$(INCDIR)/
	$(INSTALL) -m0644 scheme/*.sld $(DESTDIR)$(DATADIR)/scheme
	$(INSTALL) -m0644 scheme/*.o $(DESTDIR)$(DATADIR)/scheme
	$(INSTALL) -m0644 scheme/cyclone/*.sld $(DESTDIR)$(DATADIR)/scheme/cyclone
	$(INSTALL) -m0644 scheme/cyclone/*.o $(DESTDIR)$(DATADIR)/scheme/cyclone

uninstall:
	$(RM) $(DESTDIR)$(BINDIR)/cyclone
	$(RM) $(DESTDIR)$(BINDIR)/icyc
	$(RM) $(DESTDIR)$(LIBDIR)/*.*
	$(RMDIR) $(DESTDIR)$(LIBDIR)
	$(RM) $(DESTDIR)$(INCDIR)/*.*
	$(RMDIR) $(DESTDIR)$(INCDIR)
	$(RM) $(DESTDIR)$(DATADIR)/scheme/cyclone/*.*
	$(RMDIR) $(DESTDIR)$(DATADIR)/scheme/cyclone
	$(RM) $(DESTDIR)$(DATADIR)/scheme/*.*
	$(RMDIR) $(DESTDIR)$(DATADIR)/scheme
	$(RMDIR) $(DESTDIR)$(DATADIR)

testing:
	make PREFIX="." libcyclone.a && make && make test && make self && make self2 && make bootstrap

