FWTMP=tmp
FWVERSE=$(FWTMP)/fwverse
PACKAGES= -package megaparsec -package parser-combinators -package mtl -package uniplate -package optparse-applicative

.PHONY:	all test clean

all:	$(FWVERSE)

$(FWVERSE):	*.hs
	@mkdir -p $(FWTMP)
	ghc --make -odir $(FWTMP) -o $(FWVERSE) $(PACKAGES) Main.hs

test:	$(FWVERSE) runtest.sh tests/*.verse
	@sh runtest.sh $(FWVERSE) dump-parse allExprs
	@sh runtest.sh $(FWVERSE) dump-desugar allDesugar

clean:
	rm -rf $(FWTMP)
