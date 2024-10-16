LATEX=pdflatex -halt-on-error

PAPER=desugaring
PAPERTEX = $(PAPER).tex
PAPERSRC = $(PAPERTEX) verifier_overview.tex verifier_examples.tex ../LaTex/calculus.fmt core.tex verse-macros.tex

$(PAPER).pdf: $(PAPERSRC)
	$(LATEX) $(PAPER)
	$(LATEX) $(PAPER)

%.tex:   %.ltx   ../LaTex/calculus.fmt
	lhs2TeX $< > $@

clean:
	rm -f desugaring.aux desugaring.log desugaring.out desugaring.pdf desugaring.tex
