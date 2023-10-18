LATEX=pdflatex -halt-on-error

# verifier_overview.ltx verifier_rules.ltx verifier_examples.ltx 

desugaring.pdf: desugaring.ltx ../calculus.fmt
#	lhs2TeX core.ltx > core.tex
	lhs2TeX desugaring.ltx > desugaring.tex
	$(LATEX) desugaring
	$(LATEX) desugaring
	$(LATEX) desugaring

clean:
	rm -f desugaring.aux desugaring.log desugaring.out desugaring.pdf desugaring.tex
