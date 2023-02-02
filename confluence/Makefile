.SUFFIXES: .pdf .ps .ltx $(.SUFFIXES)
.PHONY: all clean realclean

REWRITES=../verse-icfp23/rewrites.tex
LATEX=pdflatex -halt-on-error
PAPERTEX=confluence.tex
PAPERSRC=$(PAPERTEX) $(REWRITES) 01-language.tex 03-preliminaries.tex 04-confluence.tex ../verse.bib

# Top-level target
pdf: $(PAPERSRC)
	latexmk -pdf $(PAPERTEX)

%.tex:   %.ltx   ../calculus.fmt
	lhs2TeX $< > $@

../verse-icfp23/rewrites.tex: ../verse-icfp23/rewrites.ltx
	(cd ../verse-icfp23; make rewrites.tex)

# Fake targets
##############
clean:
	-rm -f *.tex *.aux *.fdb_latexmk *.log *.out *.ptb *.bbl *.fls *.blg

# Total reset -- delete intermediates *and* final derives
realclean:
	-rm -f *.tex *.aux *.fdb_latexmk *.log *.out *.ptb *.bbl *.fls *.blg *.pdf
