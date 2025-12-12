LATEX=pdflatex -halt-on-error

PAPER=verse-conf26
PAPERTEX = $(PAPER).tex
PAPERSRC = $(PAPERTEX) ../LaTex/calculus.fmt ../LaTex/verse-macros.tex \
           verse-conf26.tex \
           macros.tex \
           syntax.tex \
           densem.tex

$(PAPER).pdf: $(PAPERSRC)
	$(LATEX) $(PAPER)
	$(LATEX) $(PAPER)

%.tex:   %.ltx   ../LaTex/calculus.fmt
	lhs2TeX $< > $@

clean:
	-rm -f verse-*.tex syntax.tex densem.tex
