LATEX=pdflatex -halt-on-error

PAPER=desugaring
PAPERTEX = $(PAPER).tex
PAPERSRC = $(PAPERTEX) verifier.tex ../trsverifier/overview.tex ../trsverifier/rules.tex ../trsverifier/examples.tex ../calculus.fmt

$(PAPER).pdf: $(PAPERSRC)
	$(LATEX) $(PAPER)

%.tex:   %.ltx   ../calculus.fmt
	lhs2TeX $< > $@

clean:
	rm -f desugaring.aux desugaring.log desugaring.out desugaring.pdf desugaring.tex
