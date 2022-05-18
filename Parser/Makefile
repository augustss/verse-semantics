PKGS = -package megaparsec -package parser-combinators -package haskeline
.PHONY: clean

parser: *.hs
	ghc -Wall --make $(PKGS) Main.hs -o parser

clean:
	rm -f *.o *.hi parser parser.exe
