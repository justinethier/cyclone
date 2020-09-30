# Cyclone Scheme
# Copyright (c) 2014-2020, Justin Ethier
# All rights reserved.

include Makefile.config

# Commands
CYCLONE = cyclone -A .
CCOMP = $(CC) $(CFLAGS)
INDENT_CMD = indent -linux -l80 -i2 -nut

# Libraries
CYC_RT_LIB = libcyclone.a
CYC_BN_LIB = libcyclonebn.a
CYC_BN_LIB_SUBDIR = third-party/libtommath-1.2.0
CYC_LIBS = $(CYC_RT_LIB) $(CYC_BN_LIB)

# Directories
BOOTSTRAP_DIR = ../cyclone-bootstrap
SCHEME_DIR = scheme
EXAMPLE_DIR = examples
HEADER_DIR = include/cyclone
TEST_DIR = tests

# Source files
SLDFILES = $(wildcard $(SCHEME_DIR)/*.sld) \
					 $(wildcard srfi/*.sld) \
					 $(wildcard libs/cyclone/*.sld) \
					 $(wildcard $(SCHEME_DIR)/cyclone/*.sld)
COBJECTS = $(SLDFILES:.sld=.o)
HEADERS = $(HEADER_DIR)/runtime.h $(HEADER_DIR)/types.h
TEST_SRC = $(TEST_DIR)/unit-tests.scm \
					 $(TEST_DIR)/test-shared-queue.scm \
					 $(TEST_DIR)/macro-hygiene.scm \
					 $(TEST_DIR)/match-tests.scm \
					 $(TEST_DIR)/srfi-28-tests.scm \
					 $(TEST_DIR)/srfi-60-tests.scm \
					 $(TEST_DIR)/srfi-121-tests.scm \
					 $(TEST_DIR)/srfi-128-162-tests.scm \
					 $(TEST_DIR)/srfi-143-tests.scm
TESTS = $(basename $(TEST_SRC))

# Primary rules (of interest to an end user)

all : cyclone icyc libs

test : libs $(TESTS)

example :
	cd $(EXAMPLE_DIR) ; $(MAKE)
	cd $(EXAMPLE_DIR)/call-scm-from-c ; $(MAKE)

clean :
	rm -rf test.txt a.out *.so *.o *.a *.out tags cyclone icyc scheme/*.o scheme/*.so scheme/*.c scheme/*.meta srfi/*.c srfi/*.meta srfi/*.o srfi/*.so scheme/cyclone/*.o scheme/cyclone/*.so scheme/cyclone/*.c scheme/cyclone/*.meta libs/cyclone/*.o libs/cyclone/*.so libs/cyclone/*.c libs/cyclone/*.meta cyclone.c dispatch.c icyc.c generate-c.c generate-c
	cd $(EXAMPLE_DIR) ; $(MAKE) clean
	rm -rf html tests/*.o tests/*.c
	rm -f tests/srfi-28-tests
	rm -f tests/srfi-60-tests
	rm -f tests/srfi-121-tests
	rm -f tests/srfi-143-tests
	rm -f tests/macro-hygiene
	rm -f tests/match-tests
	cd $(CYC_BN_LIB_SUBDIR) ; $(MAKE) clean

install : libs install-libs install-includes install-bin
	$(MKDIR) $(DESTDIR)$(DATADIR)
	$(MKDIR) $(DESTDIR)$(DATADIR)/scheme/cyclone
	$(MKDIR) $(DESTDIR)$(DATADIR)/cyclone
	$(MKDIR) $(DESTDIR)$(DATADIR)/srfi
	$(MKDIR) $(DESTDIR)$(DATADIR)/srfi/list-queues
	$(MKDIR) $(DESTDIR)$(DATADIR)/srfi/sets
	$(MKDIR) $(DESTDIR)$(DATADIR)/srfi/sorting
	$(INSTALL) -m0644 scheme/*.sld $(DESTDIR)$(DATADIR)/scheme
	$(INSTALL) -m0644 scheme/*.o $(DESTDIR)$(DATADIR)/scheme
	$(INSTALL) -m0755 scheme/*.so $(DESTDIR)$(DATADIR)/scheme
	$(INSTALL) -m0644 scheme/cyclone/*.sld $(DESTDIR)$(DATADIR)/scheme/cyclone
	$(INSTALL) -m0644 scheme/cyclone/*.scm $(DESTDIR)$(DATADIR)/scheme/cyclone
	$(INSTALL) -m0644 libs/cyclone/test.meta $(DESTDIR)$(DATADIR)/cyclone
	$(INSTALL) -m0644 libs/cyclone/match.meta $(DESTDIR)$(DATADIR)/cyclone
	$(INSTALL) -m0644 libs/cyclone/foreign.meta $(DESTDIR)$(DATADIR)/cyclone
	$(INSTALL) -m0644 scheme/cyclone/*.o $(DESTDIR)$(DATADIR)/scheme/cyclone
	$(INSTALL) -m0755 scheme/cyclone/*.so $(DESTDIR)$(DATADIR)/scheme/cyclone
	$(INSTALL) -m0644 libs/cyclone/*.sld $(DESTDIR)$(DATADIR)/cyclone
	$(INSTALL) -m0644 libs/cyclone/*.scm $(DESTDIR)$(DATADIR)/cyclone
	$(INSTALL) -m0644 libs/cyclone/*.o $(DESTDIR)$(DATADIR)/cyclone
	$(INSTALL) -m0755 libs/cyclone/*.so $(DESTDIR)$(DATADIR)/cyclone
	$(INSTALL) -m0644 srfi/*.sld $(DESTDIR)$(DATADIR)/srfi
	$(INSTALL) -m0644 srfi/*.o $(DESTDIR)$(DATADIR)/srfi
	$(INSTALL) -m0755 srfi/*.so $(DESTDIR)$(DATADIR)/srfi
	$(INSTALL) -m0644 srfi/*.meta $(DESTDIR)$(DATADIR)/srfi
	$(INSTALL) -m0644 srfi/list-queues/*.scm $(DESTDIR)$(DATADIR)/srfi/list-queues
	$(INSTALL) -m0644 srfi/sets/*.scm $(DESTDIR)$(DATADIR)/srfi/sets
	$(INSTALL) -m0644 srfi/sorting/*.scm $(DESTDIR)$(DATADIR)/srfi/sorting

uninstall :
	$(RM) $(DESTDIR)$(BINDIR)/cyclone
	$(RM) $(DESTDIR)$(BINDIR)/icyc
	$(RM) $(DESTDIR)$(LIBDIR)/$(CYC_RT_LIB)
	$(RM) $(DESTDIR)$(LIBDIR)/$(CYC_BN_LIB)
	$(RM) $(DESTDIR)$(INCDIR)/*.*
	$(RMDIR) $(DESTDIR)$(INCDIR)
	$(RM) $(DESTDIR)$(DATADIR)/scheme/cyclone/*.*
	$(RMDIR) $(DESTDIR)$(DATADIR)/scheme/cyclone
	$(RM) $(DESTDIR)$(DATADIR)/cyclone/*.*
	$(RMDIR) $(DESTDIR)$(DATADIR)/cyclone
	$(RM) $(DESTDIR)$(DATADIR)/srfi/list-queues/*.*
	$(RMDIR) $(DESTDIR)$(DATADIR)/srfi/list-queues
	$(RM) $(DESTDIR)$(DATADIR)/srfi/sets/*.*
	$(RMDIR) $(DESTDIR)$(DATADIR)/srfi/sets
	$(RM) $(DESTDIR)$(DATADIR)/srfi/sorting/*.*
	$(RMDIR) $(DESTDIR)$(DATADIR)/srfi/sorting
	$(RM) $(DESTDIR)$(DATADIR)/srfi/*.*
	$(RMDIR) $(DESTDIR)$(DATADIR)/srfi
	$(RM) $(DESTDIR)$(DATADIR)/scheme/*.*
	$(RMDIR) $(DESTDIR)$(DATADIR)/scheme
	$(RMDIR) $(DESTDIR)$(DATADIR)

# Dev rules (of interest to people hacking on Cyclone's core)

tags :
	ctags -R *

indent : gc.c runtime.c ffi.c mstreams.c $(HEADER_DIR)/*.h
	$(INDENT_CMD) gc.c
	$(INDENT_CMD) runtime.c
	$(INDENT_CMD) ffi.c
	$(INDENT_CMD) mstreams.c
	$(INDENT_CMD) $(HEADER_DIR)/*.h

# This is a test directive used to test changes to a SLD file
# EG: make sld SLDPATH=scheme/cyclone SLD=macros
sld :
	cyclone $(SLDPATH)/$(SLD).sld && sudo cp $(SLDPATH)/$(SLD).c /usr/local/share/cyclone/$(SLDPATH)/  && sudo cp $(SLDPATH)/$(SLD).sld /usr/local/share/cyclone/$(SLDPATH)/ && sudo cp $(SLDPATH)/$(SLD).o /usr/local/share/cyclone/$(SLDPATH)/ && cyclone cyclone.scm && cyclone icyc.scm && sudo $(MAKE) install-bin

debug :
	sudo ls; cyclone scheme/cyclone/cgen.sld && sudo cp scheme/cyclone/cgen.* /usr/local/share/cyclone/scheme/cyclone/ && cyclone cyclone.scm && sudo $(MAKE) install-includes && sudo $(MAKE) install-libs && ./cyclone generate-c.scm

doc :
	doxygen Doxyfile

# Helper rules (of interest to people hacking on this makefile)

.PHONY: clean full bench bootstrap tags indent debug test doc

$(TESTS) : %: %.scm
	$(CYCLONE) -I . $<
	./$@
	rm -rf $@

$(EXAMPLES) : %: %.scm
	$(CYCLONE) $<

game-of-life :
	cd $(EXAMPLE_DIR)/game-of-life ; $(MAKE)

hello-library/hello : 
	cd $(EXAMPLE_DIR)/hello-library ; $(MAKE)

libs : $(COBJECTS)

$(COBJECTS) : %.o: %.sld
	$(CYCLONE) $<

cyclone : cyclone.scm $(CYC_RT_LIB) $(CYC_BN_LIB)
	$(CYCLONE) cyclone.scm

icyc : icyc.scm $(CYC_RT_LIB) $(CYC_BN_LIB)
	$(CYCLONE) $<

dispatch.c : generate-c.scm
	$(CYCLONE) $<
	./generate-c

$(CYC_RT_LIB) : $(CFILES) $(HEADERS) $(CYC_BN_LIB)

$(CYC_BN_LIB) : $(CYC_BN_LIB_SUBDIR)/*.c
	cd $(CYC_BN_LIB_SUBDIR) ; $(MAKE) LIBNAME=$(CYC_BN_LIB) && cp $(CYC_BN_LIB) ../..

hashset.o : hashset.c $(HEADERS)
	$(CCOMP) -c $< -o $@

dispatch.o : dispatch.c $(HEADERS)
	$(CCOMP) -c $< -o $@

gc.o : gc.c $(HEADERS)
	$(CCOMP) -std=gnu99 -c $< -o $@

ffi.o : ffi.c $(HEADERS)
	$(CCOMP) -c $< -o $@

mstreams.o : mstreams.c $(HEADERS)
	$(CCOMP) -c \
					-DCYC_HAVE_OPEN_MEMSTREAM=$(CYC_PLATFORM_HAS_MEMSTREAM) \
					-DCYC_HAVE_FMEMOPEN=$(CYC_PLATFORM_HAS_FMEMOPEN) \
					$< -o $@

runtime.o : runtime.c $(HEADERS)
	$(CCOMP) -c \
					-DCYC_INSTALL_DIR=\"$(PREFIX)\" \
					-DCYC_INSTALL_LIB=\"$(LIBDIR)\" \
					-DCYC_INSTALL_BIN=\"$(BINDIR)\" \
					-DCYC_INSTALL_INC=\"$(INCDIR)\" \
					-DCYC_INSTALL_SLD=\"$(DATADIR)\" \
					-DCYC_CC_PROG=\"$(CC_PROG)\" \
					-DCYC_CC_EXEC=\"$(CC_EXEC)\" \
					-DCYC_CC_LIB=\"$(CC_LIB)\" \
					-DCYC_CC_SO=\"$(CC_SO)\" \
					-DCYC_PLATFORM=\"$(PLATFORM)\" \
					$< -o $@

libcyclone.a : runtime.o gc.o dispatch.o ffi.o mstreams.o hashset.o
	$(CREATE_LIBRARY_COMMAND) $(CREATE_LIBRARY_FLAGS) $@ $&
	$(RANLIB_COMMAND)
# Instructions from: http://www.adp-gmbh.ch/cpp/gcc/create_lib.html
# Note compiler will have to link to this, eg:
#Linking against static library
#gcc -static main.c -L. -lmean -o statically_linked
#Note: the first three letters (the lib) must not be specified, as well as the suffix (.a)

full : 
	$(MAKE) clean ; $(MAKE) && $(MAKE) test && $(MAKE) bootstrap && cd ../cyclone-bootstrap && $(MAKE) clean && ./install.sh

bench :
	cd ../r7rs-benchmarks && rm results.Cyclone && ./bench cyclone all && grep Elapsed results.Cyclone >out.txt ; grep Elapsed results.Cyclone |wc ; grep -i -e error -e limit -e crash results.Cyclone ; grep Elapsed results.Cyclone | cut -d" " -f 3 ; true

bootstrap : icyc libs
	mkdir -p $(BOOTSTRAP_DIR)/libs/cyclone
	mkdir -p $(BOOTSTRAP_DIR)/scheme/cyclone
	mkdir -p $(BOOTSTRAP_DIR)/srfi
	mkdir -p $(BOOTSTRAP_DIR)/$(HEADER_DIR)
	cp $(HEADER_DIR)/types.h $(BOOTSTRAP_DIR)/include/cyclone
	cp $(HEADER_DIR)/runtime-main.h $(BOOTSTRAP_DIR)/include/cyclone
	cp $(HEADER_DIR)/runtime.h $(BOOTSTRAP_DIR)/include/cyclone
	cp $(HEADER_DIR)/ck_ht_hash.h $(BOOTSTRAP_DIR)/include/cyclone
	cp $(HEADER_DIR)/hashset.h $(BOOTSTRAP_DIR)/include/cyclone
	cp $(HEADER_DIR)/bignum.h $(BOOTSTRAP_DIR)/include/cyclone
	cp scheme/*.sld $(BOOTSTRAP_DIR)/scheme
	cp libs/cyclone/*.sld $(BOOTSTRAP_DIR)/libs/cyclone
	cp scheme/cyclone/*.sld $(BOOTSTRAP_DIR)/scheme/cyclone
	cp libs/cyclone/*.c $(BOOTSTRAP_DIR)/libs/cyclone
	cp srfi/*.sld $(BOOTSTRAP_DIR)/srfi
	cp srfi/*.scm $(BOOTSTRAP_DIR)/srfi
	cp runtime.c $(BOOTSTRAP_DIR)
	cp ffi.c $(BOOTSTRAP_DIR)
	cp mstreams.c $(BOOTSTRAP_DIR)
	cp hashset.c $(BOOTSTRAP_DIR)
	cp gc.c $(BOOTSTRAP_DIR)
	cp dispatch.c $(BOOTSTRAP_DIR)
	cp scheme/base.c $(BOOTSTRAP_DIR)/scheme
	cp scheme/case-lambda.c $(BOOTSTRAP_DIR)/scheme
	cp scheme/cxr.c $(BOOTSTRAP_DIR)/scheme
	cp scheme/read.c $(BOOTSTRAP_DIR)/scheme
	cp scheme/write.c $(BOOTSTRAP_DIR)/scheme
	cp scheme/char.c $(BOOTSTRAP_DIR)/scheme
	cp scheme/complex.c $(BOOTSTRAP_DIR)/scheme
	cp scheme/eval.c $(BOOTSTRAP_DIR)/scheme
	cp scheme/repl.c $(BOOTSTRAP_DIR)/scheme
	cp scheme/file.c $(BOOTSTRAP_DIR)/scheme
	cp scheme/inexact.c $(BOOTSTRAP_DIR)/scheme
	cp scheme/lazy.c $(BOOTSTRAP_DIR)/scheme
	cp scheme/load.c $(BOOTSTRAP_DIR)/scheme
	cp scheme/process-context.c $(BOOTSTRAP_DIR)/scheme
	cp scheme/time.c $(BOOTSTRAP_DIR)/scheme
	cp scheme/cyclone/common.c $(BOOTSTRAP_DIR)/scheme/cyclone
	cp icyc.scm $(BOOTSTRAP_DIR)
	cp icyc.c $(BOOTSTRAP_DIR)
	cp tests/unit-tests.scm $(BOOTSTRAP_DIR)
	cp scheme/cyclone/ast.c $(BOOTSTRAP_DIR)/scheme/cyclone
	cp scheme/cyclone/cps-optimizations.c $(BOOTSTRAP_DIR)/scheme/cyclone
	cp scheme/cyclone/cps-opt-local-var-redux.scm $(BOOTSTRAP_DIR)/scheme/cyclone
	cp scheme/cyclone/cps-opt-analyze-call-graph.scm $(BOOTSTRAP_DIR)/scheme/cyclone
	cp scheme/cyclone/cps-opt-memoize-pure-fncs.scm $(BOOTSTRAP_DIR)/scheme/cyclone
	cp scheme/cyclone/pass-validate-syntax.scm $(BOOTSTRAP_DIR)/scheme/cyclone
	cp scheme/cyclone/hashset.c $(BOOTSTRAP_DIR)/scheme/cyclone
	cp scheme/cyclone/libraries.c $(BOOTSTRAP_DIR)/scheme/cyclone
	cp scheme/cyclone/macros.c $(BOOTSTRAP_DIR)/scheme/cyclone
	cp libs/cyclone/match.c $(BOOTSTRAP_DIR)/cyclone
	cp libs/cyclone/match.meta $(BOOTSTRAP_DIR)/cyclone
	cp libs/cyclone/foreign.meta $(BOOTSTRAP_DIR)/cyclone
	cp scheme/cyclone/pretty-print.c $(BOOTSTRAP_DIR)/scheme/cyclone
	cp scheme/cyclone/primitives.c $(BOOTSTRAP_DIR)/scheme/cyclone
	cp scheme/cyclone/transforms.c $(BOOTSTRAP_DIR)/scheme/cyclone
	cp scheme/cyclone/cgen.c $(BOOTSTRAP_DIR)/scheme/cyclone
	cp scheme/cyclone/util.c $(BOOTSTRAP_DIR)/scheme/cyclone
	cp libs/cyclone/test.c $(BOOTSTRAP_DIR)/cyclone
	cp libs/cyclone/test.meta $(BOOTSTRAP_DIR)/cyclone
	cp libs/cyclone/test.scm $(BOOTSTRAP_DIR)/cyclone
	cp srfi/1.c $(BOOTSTRAP_DIR)/srfi
	cp srfi/2.c $(BOOTSTRAP_DIR)/srfi
	cp srfi/2.meta $(BOOTSTRAP_DIR)/srfi
	cp srfi/9.c $(BOOTSTRAP_DIR)/srfi
	cp srfi/9.meta $(BOOTSTRAP_DIR)/srfi
	cp srfi/18.c $(BOOTSTRAP_DIR)/srfi
	cp srfi/27.c $(BOOTSTRAP_DIR)/srfi
	cp srfi/28.c $(BOOTSTRAP_DIR)/srfi
	cp srfi/60.c $(BOOTSTRAP_DIR)/srfi
	cp srfi/69.c $(BOOTSTRAP_DIR)/srfi
	cp srfi/106.c $(BOOTSTRAP_DIR)/srfi
	cp srfi/111.c $(BOOTSTRAP_DIR)/srfi
	cp srfi/113.c $(BOOTSTRAP_DIR)/srfi
	cp srfi/117.c $(BOOTSTRAP_DIR)/srfi
	cp srfi/121.c $(BOOTSTRAP_DIR)/srfi
	cp srfi/128.c $(BOOTSTRAP_DIR)/srfi
	cp srfi/128.meta $(BOOTSTRAP_DIR)/srfi
	cp srfi/132.c $(BOOTSTRAP_DIR)/srfi
	cp srfi/133.c $(BOOTSTRAP_DIR)/srfi
	cp srfi/143.c $(BOOTSTRAP_DIR)/srfi
	cp srfi/list-queues/*.scm $(BOOTSTRAP_DIR)/srfi/list-queues
	cp srfi/sets/*.scm $(BOOTSTRAP_DIR)/srfi/sets
	cp srfi/sorting/*.scm $(BOOTSTRAP_DIR)/srfi/sorting
	cp cyclone.c $(BOOTSTRAP_DIR)/cyclone.c
	cp Makefile.config $(BOOTSTRAP_DIR)/Makefile.config

install-includes : $(HEADER_DIR)/*.h 
	$(MKDIR) $(DESTDIR)$(INCDIR)
	$(INSTALL) -m0644 $(HEADER_DIR)/*.h $(DESTDIR)$(INCDIR)/

install-libs : $(CYC_LIBS)
	$(MKDIR) $(DESTDIR)$(LIBDIR)
	$(INSTALL) -m0644 $(CYC_RT_LIB) $(DESTDIR)$(LIBDIR)/
	$(INSTALL) -m0644 $(CYC_BN_LIB) $(DESTDIR)$(LIBDIR)/

install-bin : cyclone icyc
	$(MKDIR) $(DESTDIR)$(BINDIR)
	$(INSTALL) -m0755 cyclone $(DESTDIR)$(BINDIR)/
	$(INSTALL) -m0755 icyc $(DESTDIR)$(BINDIR)/
