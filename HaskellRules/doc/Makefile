.SUFFIXES: .pdf .ps .ltx $(.SUFFIXES)
.PHONY: all clean realclean

# Top-level target
all: rules.pdf

LATEX=pdflatex -halt-on-error

PAPERSRC = rules.tex ../../verse.bib

rules.tex:   rules.ltx   ../../calculus.fmt

rules.pdf: $(PAPERSRC)
	$(LATEX) rules
	bibtex rules
	$(LATEX) rules
	$(LATEX) rules


.ltx.tex:
	lhs2TeX $< > $@

.tex.pdf:
	$(LATEX) $<
	bibtex $<
	$(LATEX) $<
	$(LATEX) $<

