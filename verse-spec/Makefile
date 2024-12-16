LATEX=pdflatex -halt-on-error

PAPER=verse-spec
PAPERTEX = $(PAPER).tex
PAPERSRC = $(PAPERTEX) ../LaTex/calculus.fmt ../LaTex/verse-macros.tex verse-spec.tex \
           verse-source.tex verse-densem-planA.tex verse-desugar-to-icfp.tex verse-rules.tex \
           verse-densem-planC.tex

$(PAPER).pdf: $(PAPERSRC)
	$(LATEX) $(PAPER)
	$(LATEX) $(PAPER)

%.tex:   %.ltx   ../LaTex/calculus.fmt
	lhs2TeX $< > $@

clean:
	-rm -f verse-densem.tex
	-rm -f verse-densem.aux verse-densem.log verse-densem.out \
               verse-densem.pdf verse-densem.bbl verse-densem.blg
	-rm -f verse-densem.pdf
	-rm -f PlanA PlanADo PlanAM PlanC PlanAC PlanCC *.o *.hi

#####################

HS=Env.hs Examples.hs Exp.hs Set.hs Val.hs

testPlanA:	PlanA
	./PlanA

PlanA:	PlanA.hs $(HS)
	ghc -O2 --make PlanA.hs

testPlanADo:	PlanADo
	./PlanADo

PlanADo:	PlanADo.hs $(HS)
	ghc -O2 --make PlanADo.hs

testPlanAM:	PlanAM
	./PlanAM

PlanAM:	PlanAM.hs $(HS)
	ghc -O2 --make PlanAM.hs

testPlanC:	PlanC
	./PlanC

PlanC:	PlanC.hs $(HS)
	ghc -O2 --make PlanC.hs

testall:	testPlanA testPlanADo testPlanAM testPlanAC testPlanC
