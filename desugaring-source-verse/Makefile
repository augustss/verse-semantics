LATEX=pdflatex -halt-on-error

PAPER=desugaring
PAPERTEX = $(PAPER).tex
PAPERSRC = $(PAPERTEX) ../trsverifier/overview.tex ../trsverifier/rules.tex ../trsverifier/examples.tex ../calculus.fmt core.tex verse-macros.tex

$(PAPER).pdf: $(PAPERSRC)
	$(LATEX) $(PAPER)

%.tex:   %.ltx   ../calculus.fmt
	lhs2TeX $< > $@
	lhs2TeX $< > $@

clean:
	rm -f desugaring.aux desugaring.log desugaring.out desugaring.pdf desugaring.tex
