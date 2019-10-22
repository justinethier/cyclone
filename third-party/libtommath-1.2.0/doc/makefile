ifeq ($V,1)
silent_stdout=
else
silent_stdout= > /dev/null
endif

PLATFORM := $(shell uname | sed -e 's/_.*//')
ifeq ($(PLATFORM), Darwin)
err:
	$(error Docs can't be built on Mac)

docs mandvi manual: err
endif

docs: manual

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
	rm -f *.idx *.toc *.log *.aux *.dvi *.lof *.ind *.ilg *.ps *.log tommath.tex
