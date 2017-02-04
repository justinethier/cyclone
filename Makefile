# Cyclone Scheme
# Copyright (c) 2014, Justin Ethier
# Copyright (c) 2017, Koz Ross
# All rights reserved.

include Makefile.config

CYCLONE = cyclone
CCOMP = $(CC) $(CFLAGS)
SLDFILES = scheme/*.sld srfi/*.sld
COBJECTS = $(SLDFILES:.sld=.o)
HEADER_DIR = include/cyclone
HEADERS = $(HEADER_DIR)/runtime.h $(HEADER_DIR)/types.h

all : cyclone icyc

$(COBJECTS) : $(SLDFILES)
	$(CYCLONE) $<

cyclone : cyclone.scm $(COBJECTS) libcyclone.a
	$(CYCLONE) $<

icyc : icyc.scm $(COBJECTS) libcyclone.a
	$(CYCLONE) $<

dispatch.c : generate-c.scm
# TODO: could call from icyc, eg: icyc generate-c.scm
	$(CYCLONE) generate-c.scm
	./generate-c

libcyclone.so.1 : runtime.c include/cyclone/runtime.h
	gcc $(CFLAGS) -c -fPIC runtime.c -o runtime.o
	gcc -shared -Wl,-soname,libcyclone.so.1 -o libcyclone.so.1.0.1 runtime.o

libcyclone.a : $(CFILES) include/cyclone/runtime.h include/cyclone/types.h

dispatch.o : dispatch.c $(HEADERS)
	$(CCOMP) -c $< -o $@

gc.o : gc.c $(HEADERS)
	$(CCOMP) -std=gnu99 -c $< -o $@

mstreams.o : mstreams.c $(HEADERS)
	$(CCOMP) -c \
					-DCYC_HAVE_OPEN_MEMSTREAM=$(CYC_PLATFORM_HAS_MEMSTREAM) \
					-DCYC_HAVE_FMEMOPEN=$(CYC_PLATFORM_HAS_FMEMOPEN) \
					$< -o $@

runtime.o : runtime.c $(HEADERS)
	$(CCOMP) -c \
					-DCYC_INSTALL_DIR=\"$(PREFIX)\" \
					-DCYC_INSTALL_LIB=\"$(LIBDIR)\" \
					-DCYC_INSTALL_INC=\"$(INCDIR)\" \
					-DCYC_INSTALL_SLD=\"$(DATADIR)\" \
					-DCYC_CC_PROG=\"$(CC_PROG)\" \
					-DCYC_CC_EXEC=\"$(CC_EXEC)\" \
					-DCYC_CC_LIB=\"$(CC_LIB)\" \
					$< -o $@

libcyclone.a : runtime.o gc.o dispatch.o mstreams.o 
	$(AR) rcs $@ $^ 
# Instructions from: http://www.adp-gmbh.ch/cpp/gcc/create_lib.html
# Note compiler will have to link to this, eg:
#Linking against static library
#gcc -static main.c -L. -lmean -o statically_linked
#Note: the first three letters (the lib) must not be specified, as well as the suffix (.a)

.PHONY: clean
clean:
	rm -rf a.out *.o *.so *.a *.out tags cyclone icyc scheme/*.o scheme/*.c scheme/*.meta srfi/*.c srfi/*.meta srfi/*.o scheme/cyclone/*.o scheme/cyclone/*.c scheme/cyclone/*.meta cyclone.c dispatch.c icyc.c generate-c.c generate-c
	$(foreach f,$(TESTSCM), rm -rf $(f) $(f).c $(f).o tests/$(f).c tests/$(f).o;)
	cd examples ; make clean
