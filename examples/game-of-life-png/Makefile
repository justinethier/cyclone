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
	rm -f *.o $(SCM_PROGRAM).c $(SCM_PROGRAM) $(META_FILES) $(GENC_FILES) $(COBJECTS) write-png *.png

convert:
	convert -resize 400x test.png out.png
#	convert -delay 120 -loop 0 *.png animated.gif
