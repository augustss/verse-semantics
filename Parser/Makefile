PKGS = -package megaparsec -package parser-combinators -package haskeline
.PHONY: clean all testing

all:	parser tests

parser: *.hs
	ghc -Wall --make $(PKGS) Main.hs -o parser

tests: *.hs
	ghc -Wall --make $(PKGS) Testing.hs -main-is Testing -o tests

testing: tests
	./tests

clean:
	rm -f *.o *.hi parser parser.exe tests tests.exe
