LATEX=pdflatex -halt-on-error
verse.pdf:	verse.tex
	$(LATEX) verse.tex
	$(LATEX) verse.tex
	$(LATEX) verse.tex

calculus.pdf: calculus.ltx calculus.fmt
	lhs2TeX calculus.ltx > calculus.tex
	pdflatex calculus

clean:
	rm -f verse.aux verse.log verse.out verse.pdf
