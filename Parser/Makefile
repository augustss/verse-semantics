PKGS = -package megaparsec -package parser-combinators -package haskeline -package QuickCheck -i../TRS
FLAGS = -Wall --make -odir objs
.PHONY: clean all test testr

all:	verse tests

verse: *.hs ../TRS/*.hs
	ghc $(FLAGS) $(PKGS) Main.hs -o verse

tests: *.hs ../TRS/*.hs
	ghc $(FLAGS) $(PKGS) Testing.hs -main-is Testing -o tests

test: tests
	./tests

testr: tests
	./tests -

clean:
	rm -f *.o *.hi verse verse.exe tests tests.exe
