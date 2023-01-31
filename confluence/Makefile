.SUFFIXES: .pdf .ps .ltx $(.SUFFIXES)
.PHONY: all clean realclean

LATEX=pdflatex -halt-on-error
PAPERTEX=confluence.tex
PAPERSRC=$(PAPERTEX) rewrites.tex 01-language.tex 03-preliminaries.tex 04-confluence.tex ../verse.bib

# Top-level target
rules: rules-eqn.tex rewrites.tex
	latexmk -pdf rules-eqn.tex

conf: $(PAPERSRC)
	latexmk -pdf $(PAPERTEX)

%.tex:   %.ltx   ../calculus.fmt
	lhs2TeX $< > $@

# Fake targets
##############
clean:
	-rm -f *.tex *.aux *.fdb_latexmk *.log *.out *.ptb *.bbl *.fls *.blg

# Total reset -- delete intermediates *and* final derives
realclean:
	-rm -f *.tex *.aux *.fdb_latexmk *.log *.out *.ptb *.bbl *.fls *.blg *.pdf
