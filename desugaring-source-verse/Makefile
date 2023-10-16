LATEX=pdflatex -halt-on-error

desugaring.pdf: desugaring.ltx ../calculus.fmt core.ltx
	lhs2TeX core.ltx > core.tex
	lhs2TeX desugaring.ltx > desugaring.tex
	$(LATEX) desugaring
	$(LATEX) desugaring
	$(LATEX) desugaring

clean:
	rm -f desugaring.aux desugaring.log desugaring.out desugaring.pdf desugaring.tex
