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
	-rm -f Sem SemDo SemM SemC *.o *.hi

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

HS=Env.hs Examples.hs Exp.hs Set.hs Val.hs

Sem:	Sem.hs $(HS)
	ghc -O2 --make Sem.hs

testSemDo:	SemDo
	./SemDo

SemDo:	SemDo.hs $(HS)
	ghc -O2 --make SemDo.hs

testSemM:	SemM
	./SemM

SemM:	SemM.hs $(HS)
	ghc -O2 --make SemM.hs

testSemC:	SemC
	./SemC

SemC:	SemC.hs $(HS)
	ghc -O2 --make SemC.hs

testPlanC:	PlanC
	./PlanC

PlanC:	PlanC.hs $(HS)
	ghc -O2 --make PlanC.hs



testall:	testSem testSemDo testSemM testSemC testPlanC
