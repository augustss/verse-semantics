PKGS = -package megaparsec -package parser-combinators -package haskeline
.PHONY: clean all testing

all:	verse tests

parser: *.hs
	ghc -Wall --make $(PKGS) Main.hs -o verse

tests: *.hs
	ghc -Wall --make $(PKGS) Testing.hs -main-is Testing -o tests

testing: tests
	./tests

clean:
	rm -f *.o *.hi verse verse.exe tests tests.exe
