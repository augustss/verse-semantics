PKGS = -package megaparsec -package parser-combinators -package haskeline -package QuickCheck -i../TRS
FLAGS = -Wall --make
.PHONY: clean all test testr timtests

all:	verse tests timparse

verse: *.hs ../TRS/*.hs
	ghc $(FLAGS) $(PKGS) -odir objs-verse Main.hs -o verse

tests: *.hs ../TRS/*.hs
	ghc $(FLAGS) $(PKGS) -odir objs-tests Testing.hs -main-is Testing -o tests

timparse: *.hs
	ghc $(FLAGS) $(PKGS)  -odir objs-timparse TimParse.hs -main-is TimParse -o timparse

test: tests
	./tests

testr: tests
	./tests -

testt: timparse
	./timparse

clean:
	rm -f *.o *.hi verse verse.exe tests tests.exe timparse timparse.exe
	rm -rf objs-*
