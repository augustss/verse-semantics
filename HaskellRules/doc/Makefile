.SUFFIXES: .pdf .ps .ltx $(.SUFFIXES)
.PHONY: all clean realclean
COMMON=../../calculus.fmt ../../verse.bib

# Top-level target
all: rules.pdf block.pdf rules-eqn.pdf lfblock.pdf

LATEX=pdflatex -halt-on-error

rules.tex: $(COMMON) rules.ltx
rules-eqn.tex: $(COMMON) ../../verse-icfp23/rewrites.tex rules-eqn.ltx
block.tex: $(COMMON) block.ltx
lfblock.tex: $(COMMON) lfblock.ltx

../../verse-icfp23/rewrites.tex: ../../verse-icfp23/rewrites.ltx
	(cd ../../verse-icfp23; make rewrites.tex)

.ltx.tex:
	lhs2TeX $< > $@

.tex.pdf:
	$(LATEX) $<
	bibtex `basename -s .tex $<`
	$(LATEX) $<
	$(LATEX) $<

clean:
	rm -f *.aux *.log *.out *.pdf *.tex *.bbl *.blg
