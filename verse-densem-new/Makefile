.SUFFIXES: .pdf .ps .ltx $(.SUFFIXES)
.PHONY: all clean realclean

# Top-level target
all: verse-densem.pdf

LATEX=pdflatex -halt-on-error

PAPERTEX = verse-densem.tex
PAPERSRC = $(PAPERTEX) ../LaTex/verse.bib

verse-densem.tex: verse-densem.ltx ../LaTex/calculus.fmt

verse-densem.pdf: $(PAPERSRC)
	$(LATEX) verse-densem
	-bibtex verse-densem
	$(LATEX) verse-densem
	$(LATEX) verse-densem

# Fake targets
##############
clean:
	-rm -f verse-densem.tex
	-rm -f verse-densem.aux verse-densem.log verse-densem.out \
               verse-densem.pdf verse-densem.bbl verse-densem.blg
	-rm -f Sem SemDo SemM

# Total reset -- delete intermediates *and* final derives
realclean:
	-rm -f verse-densem.tex
	-rm -f verse-densem.aux verse-densem.log verse-densem.out \
               verse-densem.pdf verse-densem.bbl verse-densem.blg
	-rm -f verse-densem.pdf
	-rm -f Sem SemDo SemM

# Implicit rules
################

.ltx.tex:
	lhs2TeX $< > $@

.tex.pdf:
	$(LATEX) $<
	bibtex $<
	$(LATEX) $<
	$(LATEX) $<

testSem:	Sem
	./Sem

Sem:	Sem.hs
	ghc -O2 Sem.hs

testSemDo:	SemDo
	./SemDo

SemDo:	SemDo.hs
	ghc -O2 SemDo.hs

testSemM:	SemM
	./SemM

SemM:	SemM.hs
	ghc -O2 SemM.hs

testSemC:	SemC
	./SemC

SemC:	SemC.hs
	ghc -O2 SemC.hs

testall:	testSem testSemDo testSemM testSemC
