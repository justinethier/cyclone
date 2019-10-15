ifeq ($V,1)
silent_stdout=
else
silent_stdout= > /dev/null
endif

PLATFORM := $(shell uname | sed -e 's/_.*//')
ifeq ($(PLATFORM), Darwin)
err:
	$(error Docs can't be built on Mac)

docdvi poster docs mandvi manual: err
endif

# makes the LTM book DVI file, requires tetex, perl and makeindex [part of tetex I think]
docdvi: tommath.src
	${MAKE} -C pics/ MAKE=${MAKE}
	echo "hello" ${silent_stdout}
	perl booker.pl
	touch tommath.ind
	latex tommath ${silent_stdout}
	latex tommath ${silent_stdout}
	makeindex tommath
	latex tommath ${silent_stdout}

# poster, makes the single page PDF poster
poster: poster.tex
	cp poster.tex poster.bak
	touch --reference=poster.tex poster.bak
	(printf "%s" "\def\fixedpdfdate{"; date +'D:%Y%m%d%H%M%S%:z' -d @$$(stat --format=%Y poster.tex) | sed "s/:\([0-9][0-9]\)$$/'\1'}/g") > poster-deterministic.tex
	printf "%s\n" "\pdfinfo{" >> poster-deterministic.tex
	printf "%s\n" "  /CreationDate (\fixedpdfdate)" >> poster-deterministic.tex
	printf "%s\n}\n" "  /ModDate (\fixedpdfdate)" >> poster-deterministic.tex
	cat poster.tex >> poster-deterministic.tex
	mv poster-deterministic.tex poster.tex
	touch --reference=poster.bak poster.tex
	pdflatex poster
	sed -b -i 's,^/ID \[.*\]$$,/ID [<0> <0>],g' poster.pdf
	mv poster.bak poster.tex
	rm -f poster.aux poster.log poster.out

# makes the LTM book PDF file, requires tetex, cleans up the LaTeX temp files
docs: docdvi
	dvipdf tommath
	rm -f tommath.log tommath.aux tommath.dvi tommath.idx tommath.toc tommath.lof tommath.ind tommath.ilg
	${MAKE} -C pics/ clean MAKE=${MAKE}

#LTM user manual
mandvi: bn.tex
	cp bn.tex bn.bak
	touch --reference=bn.tex bn.bak
	(printf "%s" "\def\fixedpdfdate{"; date +'D:%Y%m%d%H%M%S%:z' -d @$$(stat --format=%Y bn.tex) | sed "s/:\([0-9][0-9]\)$$/'\1'}/g") > bn-deterministic.tex
	printf "%s\n" "\pdfinfo{" >> bn-deterministic.tex
	printf "%s\n" "  /CreationDate (\fixedpdfdate)" >> bn-deterministic.tex
	printf "%s\n}\n" "  /ModDate (\fixedpdfdate)" >> bn-deterministic.tex
	cat bn.tex >> bn-deterministic.tex
	mv bn-deterministic.tex bn.tex
	touch --reference=bn.bak bn.tex
	echo "hello" > bn.ind
	latex bn ${silent_stdout}
	latex bn ${silent_stdout}
	makeindex bn
	latex bn ${silent_stdout}

#LTM user manual [pdf]
manual:	mandvi
	pdflatex bn >/dev/null
	sed -b -i 's,^/ID \[.*\]$$,/ID [<0> <0>],g' bn.pdf
	mv bn.bak bn.tex
	rm -f bn.aux bn.dvi bn.log bn.idx bn.lof bn.out bn.toc

clean:
	${MAKE} -C pics/ clean MAKE=${MAKE}
	rm -f *.idx *.toc *.log *.aux *.dvi *.lof *.ind *.ilg *.ps *.log tommath.tex
