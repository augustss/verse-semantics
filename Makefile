LATEX=pdflatex -halt-on-error
verse.pdf:	verse.tex
	$(LATEX) verse.tex
	$(LATEX) verse.tex
	$(LATEX) verse.tex

clean:
	rm -f verse.aux verse.log verse.out verse.pdf
