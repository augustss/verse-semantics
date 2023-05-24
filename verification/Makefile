.SUFFIXES: .pdf .ps .ltx $(.SUFFIXES)
.PHONY: all clean realclean

PAPER=verification

# Top-level target
all: verification.pdf


LATEX=pdflatex -halt-on-error

PAPERTEX = $(PAPER).tex rewrites.tex rewriting.tex
PAPERSRC = $(PAPERTEX) ../verse.bib ../calculus.fmt

rewrites.tex: ../verse-pldi23/rewrites.ltx
	lhs2TeX ../verse-pldi23/rewrites.ltx > rewrites.tex

$(PAPER).tex: $(PAPER).ltx

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
