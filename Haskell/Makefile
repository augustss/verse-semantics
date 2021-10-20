FWTMP=tmp
FWVERSE=$(FWTMP)/fwverse
PACKAGES= -package megaparsec -package parser-combinators -package mtl -package uniplate -package optparse-applicative

.PHONY:	all test clean

all:
	@echo Pick a target

$(FWVERSE):	*.hs
	@mkdir -p $(FWTMP)
	ghc --make -odir $(FWTMP) -o $(FWVERSE) $(PACKAGES) Main.hs

test:	$(FWVERSE) tests/*.verse
	$(FWVERSE) --dump-parse tests/allExprs.verse > $(FWTMP)/allExprs.dump-parse && diff tests/allExprs.dump-parse $(FWTMP)//allExpr.dump-parse

clean:
	rm -rf $(FWTMP)
