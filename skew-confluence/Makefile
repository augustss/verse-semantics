.SUFFIXES: .pdf .ps .ltx $(.SUFFIXES)
.PHONY: all clean realclean

REWRITES=../verse-icfp23/rewrites.tex skew-rewrites.tex
LATEX=pdflatex -halt-on-error
PAPERTEX=skew.tex
PAPERSRC=$(PAPERTEX) $(REWRITES) 01-introduction.tex 02-languages.tex 03-preliminaries.tex 04-organization.tex 05-lemmas.tex 06-lambda-proof.tex 07-letrec-proof.tex 08-verse-proof.tex 09-wrapup.tex ../verse.bib

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


