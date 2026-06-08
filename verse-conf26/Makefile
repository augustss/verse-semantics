LATEX=pdflatex -halt-on-error

PAPER=verse-conf26
PAPERTEX = $(PAPER).tex
PAPERSRC = $(PAPERTEX) ../LaTex/calculus.fmt ../LaTex/verse-macros.tex \
           verse-conf26.tex \
           rewrite-rules.tex \
           macros.tex \
           syntax.tex \
           densem.tex

$(PAPER).pdf: $(PAPERSRC)
	$(LATEX) $(PAPER)
	$(LATEX) $(PAPER)

%.tex:   %.ltx   ../LaTex/calculus.fmt
	lhs2TeX $< > $@

clean:
	-rm -f verse-*.tex syntax.tex densem.tex  *.log *.out *.aux

# ----------------------------

REWRITE=verse-rewriting
REWRITETEX = $(REWRITE).tex
REWRITESRC = $(REWRITETEX) ../LaTex/calculus.fmt ../LaTex/verse-macros.tex \
           verse-rewriting.tex \
           macros.tex \
           syntax.tex \
           runtime-rules.tex \
           verification-rules.tex

$(REWRITE).pdf: $(REWRITESRC)
	$(LATEX) $(REWRITE)
	$(LATEX) $(REWRITE)
