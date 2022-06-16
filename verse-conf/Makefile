LATEX=pdflatex -halt-on-error
verse-conf.pdf: verse-conf.ltx rewrites.ltx ../calculus.fmt ../verse.bib
	lhs2TeX verse-conf.ltx > verse-conf.tex
	lhs2TeX rewrites.ltx > rewrites.tex
	$(LATEX) verse-conf
	bibtex verse-conf
	$(LATEX) verse-conf
	$(LATEX) verse-conf

clean:
	rm -f verse-conf.aux verse-conf.log verse-conf.out verse-conf.pdf
