# Temporary Makefile to run tests
TESTER=./dist-newstyle/build/*/*/HaskellRules-0.1.0.0/x/tester/build/tester/tester
TESTFILE=versetests/tests.versetest
TESTPARSEFILE=versetests/test1.verse
.PHONY: all test testfast clean

all:
	echo Specify a target

test: testfast
	$(TESTER) --quiet --rules=popl           $(TESTFILE)

testfast:
	$(TESTER) --quiet --parse                $(TESTPARSEFILE)
	$(TESTER) --quiet --eval                 $(TESTFILE)
	$(TESTER) --quiet --dfs --rules=popl     $(TESTFILE)
	$(TESTER) --quiet --dfs --rules=pldi     $(TESTFILE)

clean:
	cabal clean
	rm -f *~

#######

PKGS = -package megaparsec -package parser-combinators -package haskeline -package QuickCheck -package optparse-applicative
SRCS = -iepic -ifrontend -irules -itrs -iverse
FLAGS = -O -Wall --make

verse: */*.hs */*/*.hs
	ghc $(FLAGS) $(PKGS) $(SRCS) -outputdir objs-verse verse/Main.hs -o bin/verse

tester: */*.hs */*/*.hs
	ghc $(FLAGS) $(PKGS) $(SRCS) -outputdir objs-tester tester/Main.hs -o bin/tester
