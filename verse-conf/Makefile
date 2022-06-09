LATEX=pdflatex -halt-on-error
verse-conf.pdf: verse-conf.ltx ../calculus.fmt
	lhs2TeX verse-conf.ltx > verse-conf.tex
	$(LATEX) verse-conf
	$(LATEX) verse-conf
	$(LATEX) verse-conf

clean:
	rm -f verse-conf.aux verse-conf.log verse-conf.out verse-conf.pdf
