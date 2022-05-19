PKGS = -package megaparsec -package parser-combinators -package haskeline
.PHONY: clean

parser: *.hs
	ghc -Wall --make $(PKGS) Main.hs -o parser

tests: *.hs
	ghc -Wall --make $(PKGS) Tests.hs -o tests

clean:
	rm -f *.o *.hi parser parser.exe tests tests.exe
