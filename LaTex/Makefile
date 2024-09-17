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

ZIPFILES=verse-icfp23/*.tex confluence/03-preliminaries.tex confluence/04-confluence.tex confluence/08-skew.tex verse.bib verse-icfp23/icfp23.bbl acmart.cls

icfp23.zip: $(ZIPFILES)
	rm -f icfp23.zip
	zip icfp23.zip $(ZIPFILES)
