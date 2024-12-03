LATEX=pdflatex -halt-on-error

PAPER=verse-spec
PAPERTEX = $(PAPER).tex
PAPERSRC = $(PAPERTEX) ../LaTex/calculus.fmt ../LaTex/verse-macros.tex verse-spec.tex \
           verse-source.tex verse-densem-planA.tex verse-desugar-to-icfp.tex verse-rules.tex

$(PAPER).pdf: $(PAPERSRC)
	$(LATEX) $(PAPER)
	$(LATEX) $(PAPER)

%.tex:   %.ltx   ../LaTex/calculus.fmt
	lhs2TeX $< > $@

