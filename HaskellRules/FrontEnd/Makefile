PKGS = -package megaparsec -package parser-combinators -package haskeline -package QuickCheck -package optparse-applicative -i.. -i../epicgames/src
FLAGS = -O -Wall --make -DNO_STRUCT_RULES=1
TESTSFILE=tests.versetest
.PHONY: clean all test testfast timtests

all:	verse tests timparse

verse: *.hs ../TRS/*.hs
	ghc $(FLAGS) $(PKGS) -outputdir objs-verse Main.hs -o verse

tests: *.hs ../TRS/*.hs
	ghc $(FLAGS) $(PKGS) -outputdir objs-tests Testing.hs -main-is Parser.Testing -o tests

timparse: *.hs
	ghc $(FLAGS) $(PKGS)  -outputdir objs-timparse TimParse.hs -main-is Parser.TimParse -o timparse

test: tests testfast
	./tests --quiet                  $(TESTFILE)
	./tests --quiet --split          $(TESTFILE)
	@echo All tests succeeded

testfast: tests
	./tests --quiet --parse          test1.verse
	./tests --quiet --dfs            $(TESTFILE)
	./tests --quiet --dfs --split    $(TESTFILE)
	./tests --quiet --dfs --simplify $(TESTFILE)
#	./tests --quiet --dfs --popl     $(TESTFILE)
	./tests --quiet --eval           $(TESTFILE)

testtricky: tests
	./tests tricky.versetest

testt: timparse
	./timparse

timing:	tests
	time ./tests

clean:
	rm -f *.o *.hi verse verse.exe tests tests.exe timparse timparse.exe
	rm -rf objs-*
