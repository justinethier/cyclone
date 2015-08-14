# Cyclone Scheme
# Copyright (c) 2014, Justin Ethier
# All rights reserved.

include Makefile.config

CYCLONE = cyclone
TESTSCM = unit-tests
TESTFILES = $(addprefix tests/, $(addsuffix .scm, $(TESTSCM)))
BOOTSTRAP_DIR = ../cyclone-bootstrap

SMODULES = \
  scheme/base \
  scheme/char \
  scheme/eval \
  scheme/file \
  scheme/read \
  scheme/write \
  scheme/cyclone/cgen \
  scheme/cyclone/common \
  scheme/cyclone/libraries \
  scheme/cyclone/macros \
  scheme/cyclone/transforms \
  scheme/cyclone/util 
SLDFILES = $(addsuffix .sld, $(SMODULES))
COBJECTS=$(SLDFILES:.sld=.o)

all: cyclone icyc

%.o: %.sld
	$(CYCLONE) $<

cyclone: $(COBJECTS) libcyclone.a cyclone.scm
	$(CYCLONE) cyclone.scm

icyc: $(COBJECTS) libcyclone.a icyc.scm
	$(CYCLONE) icyc.scm

dispatch.c: generate-c.scm
# TODO: could call from icyc, eg: icyc generate-c.scm
	$(CYCLONE) generate-c.scm
	./generate-c

libcyclone.so.1: runtime.c include/cyclone/runtime.h
	gcc -g -c -fPIC runtime.c -o runtime.o
	gcc -shared -Wl,-soname,libcyclone.so.1 -o libcyclone.so.1.0.1 runtime.o

libcyclone.a: runtime.c include/cyclone/runtime.h include/cyclone/types.h dispatch.c
	$(CC) -g -c -Iinclude dispatch.c -o dispatch.o
	$(CC) -g -c -Iinclude -DCYC_INSTALL_DIR=\"$(PREFIX)\" -DCYC_INSTALL_LIB=\"$(LIBDIR)\" -DCYC_INSTALL_INC=\"$(INCDIR)\" -DCYC_INSTALL_SLD=\"$(DATADIR)\" runtime.c -o runtime.o
	$(AR) rcs libcyclone.a runtime.o dispatch.o
# Instructions from: http://www.adp-gmbh.ch/cpp/gcc/create_lib.html
# Note compiler will have to link to this, eg:
#Linking against static library
#gcc -static main.c -L. -lmean -o statically_linked
#Note: the first three letters (the lib) must not be specified, as well as the suffix (.a)

.PHONY: bootstrap
bootstrap: icyc
#	rm -rf $(BOOTSTRAP_DIR)
	mkdir -p $(BOOTSTRAP_DIR)/scheme/cyclone
	mkdir -p $(BOOTSTRAP_DIR)/include/cyclone
	cp include/cyclone/types.h $(BOOTSTRAP_DIR)/include/cyclone
	cp include/cyclone/runtime-main.h $(BOOTSTRAP_DIR)/include/cyclone
	cp include/cyclone/runtime.h $(BOOTSTRAP_DIR)/include/cyclone
	cp scheme/*.sld $(BOOTSTRAP_DIR)/scheme
	cp scheme/cyclone/*.sld $(BOOTSTRAP_DIR)/scheme/cyclone
	cp runtime.c $(BOOTSTRAP_DIR)
	cp dispatch.c $(BOOTSTRAP_DIR)
	cp scheme/base.c $(BOOTSTRAP_DIR)/scheme
	cp scheme/read.c $(BOOTSTRAP_DIR)/scheme
	cp scheme/write.c $(BOOTSTRAP_DIR)/scheme
	cp scheme/char.c $(BOOTSTRAP_DIR)/scheme
	cp scheme/eval.c $(BOOTSTRAP_DIR)/scheme
	cp scheme/file.c $(BOOTSTRAP_DIR)/scheme
	cp scheme/cyclone/common.c $(BOOTSTRAP_DIR)/scheme/cyclone
	cp icyc.scm $(BOOTSTRAP_DIR)
	cp tests/unit-tests.scm $(BOOTSTRAP_DIR)
	cp scheme/cyclone/libraries.c $(BOOTSTRAP_DIR)/scheme/cyclone
	cp scheme/cyclone/macros.c $(BOOTSTRAP_DIR)/scheme/cyclone
	cp scheme/cyclone/transforms.c $(BOOTSTRAP_DIR)/scheme/cyclone
	cp scheme/cyclone/cgen.c $(BOOTSTRAP_DIR)/scheme/cyclone
	cp scheme/cyclone/util.c $(BOOTSTRAP_DIR)/scheme/cyclone
	cp cyclone.c $(BOOTSTRAP_DIR)/cyclone.c
	cp Makefile.config $(BOOTSTRAP_DIR)/Makefile.config


.PHONY: test
test: $(TESTFILES) $(CYCLONE)
	$(foreach f,$(TESTSCM), echo tests/$(f) ; ./cyclone tests/$(f).scm && tests/$(f) && rm -rf tests/$(f);)

.PHONY: tags
tags:
	ctags -R *

.PHONY: clean
clean:
	rm -rf a.out *.o *.so *.a *.out tags cyclone icyc scheme/*.o scheme/*.c scheme/cyclone/*.o scheme/cyclone/*.c dispatch.c icyc.c generate-c.c generate-c
	$(foreach f,$(TESTSCM), rm -rf $(f) $(f).c tests/$(f).c;)

install-includes:
	$(MKDIR) $(DESTDIR)$(INCDIR)
	$(INSTALL) -m0644 include/cyclone/*.h $(DESTDIR)$(INCDIR)/

install-libs:
	$(MKDIR) $(DESTDIR)$(LIBDIR)
	$(INSTALL) -m0644 libcyclone.a $(DESTDIR)$(LIBDIR)/

install-bin:
	$(MKDIR) $(DESTDIR)$(BINDIR)
	$(INSTALL) -m0755 cyclone $(DESTDIR)$(BINDIR)/
	$(INSTALL) -m0755 icyc $(DESTDIR)$(BINDIR)/

# TODO: rewrite install to be in terms of incremental steps above.
#       also want to propagate this change to cyclone-bootstrap
install:
	$(MKDIR) $(DESTDIR)$(BINDIR)
	$(MKDIR) $(DESTDIR)$(LIBDIR)
	$(MKDIR) $(DESTDIR)$(INCDIR)
	$(MKDIR) $(DESTDIR)$(DATADIR)
	$(MKDIR) $(DESTDIR)$(DATADIR)/scheme/cyclone
	$(INSTALL) -m0644 libcyclone.a $(DESTDIR)$(LIBDIR)/
	$(INSTALL) -m0644 include/cyclone/*.h $(DESTDIR)$(INCDIR)/
	$(INSTALL) -m0644 scheme/*.sld $(DESTDIR)$(DATADIR)/scheme
	$(INSTALL) -m0644 scheme/*.o $(DESTDIR)$(DATADIR)/scheme
	$(INSTALL) -m0644 scheme/cyclone/*.sld $(DESTDIR)$(DATADIR)/scheme/cyclone
	$(INSTALL) -m0644 scheme/cyclone/*.o $(DESTDIR)$(DATADIR)/scheme/cyclone
	$(INSTALL) -m0755 cyclone $(DESTDIR)$(BINDIR)/
	$(INSTALL) -m0755 icyc $(DESTDIR)$(BINDIR)/

uninstall:
	$(RM) $(DESTDIR)$(BINDIR)/cyclone
	$(RM) $(DESTDIR)$(BINDIR)/icyc
	$(RM) $(DESTDIR)$(LIBDIR)/libcyclone.a
	$(RM) $(DESTDIR)$(INCDIR)/*.*
	$(RMDIR) $(DESTDIR)$(INCDIR)
	$(RM) $(DESTDIR)$(DATADIR)/scheme/cyclone/*.*
	$(RMDIR) $(DESTDIR)$(DATADIR)/scheme/cyclone
	$(RM) $(DESTDIR)$(DATADIR)/scheme/*.*
	$(RMDIR) $(DESTDIR)$(DATADIR)/scheme
	$(RMDIR) $(DESTDIR)$(DATADIR)

# This is an example of how to build/test changes to the compiler.
# With the way everything is setup now, you need to rebuild the module(s),
# install them, and then rebuild the compiler executable to run them.
#
# It may be possible to use dynamic linking to reduce some of this overhead
# in the future (should really look into that!)
trans:
	cyclone scheme/cyclone/transforms.sld && sudo cp scheme/cyclone/transforms.* /usr/local/share/cyclone/scheme/cyclone/ && cyclone cyclone.scm && sudo cp cyclone /usr/local/bin
#	cyclone scheme/cyclone/transforms.sld
#	sudo cp scheme/cyclone/transforms.* /usr/local/share/cyclone/scheme/cyclone/
#	cyclone cyclone.scm
#	sudo cp cyclone /usr/local/bin/cyclone

eval:
	cyclone scheme/eval.sld && sudo cp scheme/eval.* /usr/local/share/cyclone/scheme/ && cyclone cyclone.scm && cyclone icyc.scm && sudo make install-bin && cyclone test2.scm && ./test2
