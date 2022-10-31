PKGS = -package megaparsec -package parser-combinators -package haskeline -package QuickCheck -package optparse-applicative -i../TRS
FLAGS = -O -Wall --make
TESTSFILE=tests.versetest
.PHONY: clean all test testfast timtests

all:	verse tests timparse

verse: *.hs ../TRS/*.hs
	ghc $(FLAGS) $(PKGS) -outputdir objs-verse Main.hs -o verse

tests: *.hs ../TRS/*.hs
	ghc $(FLAGS) $(PKGS) -outputdir objs-tests Testing.hs -main-is Testing -o tests

timparse: *.hs
	ghc $(FLAGS) $(PKGS)  -outputdir objs-timparse TimParse.hs -main-is TimParse -o timparse

test: tests testfast
	./tests --quiet                  $(TESTFILE)
	./tests --quiet --split          $(TESTFILE)
	@echo All tests succeeded

testfast: tests
	./tests --quiet --parse          test1.verse
	./tests --quiet --dfs            $(TESTFILE)
	./tests --quiet --dfs --split    $(TESTFILE)
	./tests --quiet --dfs --simplify $(TESTFILE)
	./tests --quiet --dfs --popl     $(TESTFILE)
	./tests --quiet --eval           $(TESTFILE)

testt: timparse
	./timparse

clean:
	rm -f *.o *.hi verse verse.exe tests tests.exe timparse timparse.exe
	rm -rf objs-*
