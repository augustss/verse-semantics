.SUFFIXES: .pdf .ps .ltx $(.SUFFIXES)
.PHONY: all clean realclean

# Top-level target
all: verse-conf.pdf

LATEX=pdflatex -halt-on-error

PAPERTEX = verse-conf.tex rewrites.tex
PAPERSRC = $(PAPERTEX) ../verse.bib

rewrites.tex:   rewrites.ltx   ../calculus.fmt
verse-conf.tex: verse-conf.ltx ../calculus.fmt

talk-figures.pdf: talk-figures.tex
	$(LATEX) talk-figures
	$(LATEX) talk-figures

verse-conf.pdf: $(PAPERSRC)
	$(LATEX) verse-conf
	bibtex verse-conf
	$(LATEX) verse-conf
	$(LATEX) verse-conf

# Fake targets
##############
clean:
	-rm -f verse-conf.tex rewrites.tex
	-rm -f verse-conf.aux verse-conf.log verse-conf.out \
               verse-conf.pdf verse-conf.bbl verse-conf.blg

# Total reset -- delete intermediates *and* final derives
realclean:
	-rm -f verse-conf.tex rewrites.tex
	-rm -f verse-conf.aux verse-conf.log verse-conf.out \
               verse-conf.pdf verse-conf.bbl verse-conf.blg
	-rm -f verse-conf.pdf

# Implicit rules
################

.ltx.tex:
	lhs2TeX $< > $@

.tex.pdf:
	$(LATEX) $<
	bibtex $<
	$(LATEX) $<
	$(LATEX) $<

