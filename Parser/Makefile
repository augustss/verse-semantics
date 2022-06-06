PKGS = -package megaparsec -package parser-combinators -package haskeline -package QuickCheck -i../TRS
.PHONY: clean all testing

all:	verse tests

verse: *.hs ../TRS/*.hs
	ghc -Wall --make $(PKGS) Main.hs -o verse

tests: *.hs ../TRS/*.hs
	ghc -Wall --make $(PKGS) Testing.hs -main-is Testing -o tests

testing: tests
	./tests

clean:
	rm -f *.o *.hi verse verse.exe tests tests.exe
