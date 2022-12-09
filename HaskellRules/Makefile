# Temporary Makefile to run tests
TESTFILE=versetests/tests.versetest
TRICKY=versetests/tricky.versetest
TESTPARSEFILE=versetests/test1.verse
.PHONY: all test testfast clean

TESTER=./dist-newstyle/build/*/*/HaskellRules-0.1.0.0/x/tester/build/tester/tester
TESTER=cabal run tester --

all:
	@echo Specify a target

build:
	cabal build

test: testfast
	$(TESTER) --quiet --rules=popl           $(TESTFILE)

testfast:
	$(TESTER) --quiet --parse                                                $(TESTPARSEFILE)
	$(TESTER) --quiet       --eval       --no-under-lambda                   $(TESTFILE)
#	$(TESTER) --quiet --eval --split                                         $(TESTFILE)
	$(TESTER) --quiet --dfs --rules=POPL                --max-steps=25000    $(TESTFILE)
	$(TESTER) --quiet --dfs --rules=PLDI --final-inline --max-steps=25000    $(TESTFILE)

trickytest: testtricky

testtricky:
	$(TESTER) --quiet --summary --no-error --all-rules $(TRICKY)

clean:
	cabal clean
	rm -rf *~ objs-*

#######

PKGS = -package megaparsec -package parser-combinators -package haskeline -package QuickCheck -package optparse-applicative
SRCS = -iepic -ifrontend -irules -itrs -iverse
FLAGS = -O -Wall --make
PROF =
#PROF = -prof -fprof-auto

bin/verse: */*.hs */*/*.hs
	ghc $(FLAGS) $(PROF) $(PKGS) $(SRCS) -outputdir objs-verse verse/Main.hs -o bin/verse

# Put a compiled `tester` in ./bin/tester
bin/tester: */*.hs */*/*.hs
	mkdir -p bin
	ghc $(FLAGS) $(PROF) $(PKGS) $(SRCS) -outputdir objs-tester tester/Main.hs -o bin/tester

bin/qctest: */*.hs */*/*.hs
	ghc $(FLAGS) $(PROF) $(PKGS) $(SRCS) -outputdir objs-qctest qctest/Main.hs -o bin/qctest

v1install:
	cabal v1-install -p megaparsec
	cabal v1-install -p QuickCheck
	cabal v1-install -p uniplate
	cabal v1-install -p optparse-applicative
