# Automated building of a program
#
# Just put the main program and libraries at the top here
# without extensions, and the makefile takes care of
# everything else:
SCM_PROGRAM = hello
SCM_LIBS = libs/lib1 libs/lib2

SLD_FILES = $(addsuffix .sld, $(SCM_LIBS))
SCM_FILE = $(addsuffix .scm, $(SCM_PROGRAM))
META_FILES = $(addsuffix .meta, $(SCM_LIBS))
GENC_FILES = $(addsuffix .c, $(SCM_LIBS))
COBJECTS=$(SLD_FILES:.sld=.o)

all: $(SCM_PROGRAM)

%.o: %.sld
	cyclone $<

$(SCM_PROGRAM): $(SCM_FILE) $(COBJECTS)
	cyclone $<

clean:
	rm -f *.o *.c $(SCM_PROGRAM) $(META_FILES) $(GENC_FILES) $(COBJECTS)
