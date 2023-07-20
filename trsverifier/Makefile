.SUFFIXES: .pdf .ps .ltx $(.SUFFIXES)
.PHONY: all clean realclean

PAPER=main

# Top-level target
all: main.pdf


LATEX=pdflatex -halt-on-error

PAPERTEX = $(PAPER).tex rewrites.tex
PAPERSRC = $(PAPERTEX) overview.tex syntax.tex rules.tex examples.tex ../verse.bib ../calculus.fmt

rewrites.tex: ../verse-icfp23/rewrites.ltx
	lhs2TeX ../verse-icfp23/rewrites.ltx > rewrites.tex

%.tex:   %.ltx   ../calculus.fmt
	lhs2TeX $< > $@

$(PAPER).pdf: $(PAPERSRC)
	$(LATEX) $(PAPER)
	-bibtex $(PAPER)
	$(LATEX) $(PAPER)
	$(LATEX) $(PAPER)

# Fake targets
##############
clean:
	-rm -f $(PAPER).tex
	-rm -f $(PAPER).aux $(PAPER).log $(PAPER).out \
               $(PAPER).pdf $(PAPER).bbl $(PAPER).blg

# Total reset -- delete intermediates *and* final derives
realclean:
	-rm -f $(PAPER).tex
	-rm -f $(PAPER).aux $(PAPER).log $(PAPER).out \
               $(PAPER).pdf $(PAPER).bbl $(PAPER).blg
	-rm -f $(PAPER).pdf

# Implicit rules
################

.ltx.tex:
	lhs2TeX $< > $@

.tex.pdf:
	$(LATEX) $<
	bibtex $<
	$(LATEX) $<
	$(LATEX) $<
