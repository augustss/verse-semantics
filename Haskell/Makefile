FWTMP=tmp
FWVERSE=$(FWTMP)/fwverse
PACKAGES= -package megaparsec -package parser-combinators -package mtl -package uniplate -package optparse-applicative

.PHONY:	all test clean

all:	$(FWVERSE)

$(FWVERSE):	*.hs
	@mkdir -p $(FWTMP)
	ghc -O2 --make -odir $(FWTMP) -o $(FWVERSE) $(PACKAGES) Main.hs

test:	$(FWVERSE) runtest.sh tests/*.verse
	@sh runtest.sh $(FWVERSE) dump-parse allExprs
	@sh runtest.sh $(FWVERSE) dump-desugar allDesugar
	@sh runtest.sh $(FWVERSE) dump-extrude allExtrude
	@sh runtest.sh $(FWVERSE) dump-scope allScope
	@sh runtest.sh $(FWVERSE) dump-cleanup ex1
	@sh runtest.sh $(FWVERSE) dump-cleanup ex2
	@sh runtest.sh $(FWVERSE) dump-cleanup ex3
	@sh runtest.sh $(FWVERSE) dump-cleanup ex4
	@sh runtest.sh $(FWVERSE) dump-cleanup ex5

clean:
	rm -rf $(FWTMP)
