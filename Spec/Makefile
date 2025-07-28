LATEX=pdflatex -halt-on-error

PAPER=verse-spec
PAPERTEX = $(PAPER).tex
PAPERSRC = $(PAPERTEX) ../LaTex/calculus.fmt ../LaTex/verse-macros.tex verse-spec.tex \
           verse-source.tex ess-to-mini.tex mini-to-core.tex verse-rules.tex \
           densem-expr-style.tex densem-dps-style.tex densem-koen.tex \
           verse-densem-planA.tex densem-notation.tex \
#           densem-set-seq-set.tex

$(PAPER).pdf: $(PAPERSRC)
	$(LATEX) $(PAPER)
	$(LATEX) $(PAPER)

%.tex:   %.ltx   ../LaTex/calculus.fmt
	lhs2TeX $< > $@

clean:
	-rm -f verse-*.tex densem-expr*tex ess-*tex mini-*tex *koen*tex
	-rm -f verse-spec.aux verse-spec.log verse-spec.out \
	-rm -f *.log \
               verse-spec.pdf verse-spec.bbl verse-spec.blg
	-rm -f verse-spec.pdf
	-rm -f PlanA PlanADo PlanAM PlanC PlanAC PlanCC *.o *.hi *~

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
