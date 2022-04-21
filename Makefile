LATEX=pdflatex -halt-on-error
calculus.pdf: calculus.ltx calculus.fmt
	lhs2TeX calculus.ltx > calculus.tex
	$(LATEX) calculus
	$(LATEX) calculus
	$(LATEX) calculus

verse.pdf:	verse.tex
	$(LATEX) verse.tex
	$(LATEX) verse.tex
	$(LATEX) verse.tex

clean:
	rm -f verse.aux verse.log verse.out verse.pdf
	rm -f calculus.tex calculus.aux calculus.log calculus.out calculus.pdf
