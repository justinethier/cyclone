SCM_PROGRAM = life
SCM_LIBS = example/grid example/life

SLD_FILES = $(addsuffix .sld, $(SCM_LIBS))
SCM_FILE = $(addsuffix .scm, $(SCM_PROGRAM))
META_FILES = $(addsuffix .meta, $(SCM_LIBS))
GENC_FILES = $(addsuffix .c, $(SCM_LIBS))
COBJECTS=$(SLD_FILES:.sld=.o)

all: $(SCM_PROGRAM) #write-png

%.o: %.sld
	cyclone $<

$(SCM_PROGRAM): $(SCM_FILE) $(COBJECTS) write-png.o
	cyclone $<

#write-png: write-png.o
#	gcc -o write-png write-png.o -lpng

write-png.o: write-png.c write-png.h
	gcc -c write-png.c

clean:
	rm -f *.o $(SCM_PROGRAM).c $(SCM_PROGRAM) $(META_FILES) $(GENC_FILES) $(COBJECTS) write-png *.png *.gif
	rm -rf tmp

convert:
	./convert.sh

# A temporary directive to build the life program using GCC directly
# For now, need to manually add "#include "../write-png.h"" to example/life.c to get it to build
debug:
	gcc example/life.c -g -c -o example/life.o
	gcc life.c -g -c -o life.o
	gcc life.o  /usr/local/share/cyclone/scheme/base.o  example/grid.o  /usr/local/share/cyclone/scheme/write.o  example/life.o write-png.o -pthread -lcyclone -lck -lm -lpng -g -o life
