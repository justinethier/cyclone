#
# Need to install ImageMagick, libpng, and libpng headers to build this project.
# To install these packages on Ubuntu: sudo apt-get install imagemagick libpng-dev
#
# To create PNG outputs and convert to an animation, run:
# make && ./life && ./convert.sh
#
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
#	cyclone $<
	cyclone -d $<
# For now, modify -d output manually to compile-in necessary objects/libs for PNG
	gcc life.c -g -c -o life.o
	gcc life.o  /usr/local/share/cyclone/scheme/base.o  example/grid.o  /usr/local/share/cyclone/scheme/write.o  example/life.o write-png.o -pthread -lcyclone -lck -lm -lpng -g -o life

#write-png: write-png.o
#	gcc -o write-png write-png.o -lpng

write-png.o: write-png.c write-png.h
	gcc -c write-png.c

clean:
	rm -f *.o $(SCM_PROGRAM).c $(SCM_PROGRAM) $(META_FILES) $(GENC_FILES) $(COBJECTS) write-png *.png *.gif
	rm -rf tmp

convert:
	./convert.sh
