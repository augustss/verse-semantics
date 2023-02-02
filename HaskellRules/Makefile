# Temporary Makefile to run tests
TESTFILE=versetests/tests.versetest
TRICKY=versetests/tricky.versetest
TESTPARSEFILE=versetests/test1.verse
.PHONY: all test testfast clean

TESTER=./dist-newstyle/build/*/*/HaskellRules-0.1.0.0/x/tester/build/tester/tester
TESTER=cabal run tester --
QCTEST=cabal run qctest --

all:
	@echo Specify a target

build:
	cabal build

testicfp:
	$(TESTER) --quiet --dfs --rules=ICFP                    --max-steps=25000       $(TESTFILE)
	$(TESTER) --quiet --dfs --rules=ICFPR                   --max-steps=25000       $(TESTFILE)
	$(TESTER) --quiet       --rules=ICFP --max-steps=500000 --max-norm-steps=100000 $(TRICKY)
	$(QCTEST)               --rules=ICFP --ignore-fuel-stop --max-success=1000 --ignore-recursive

testslow:
	$(TESTER)               --rules=popl --no-under-lambda --max-steps=25000    $(TESTFILE)

testfast:
	$(TESTER) --quiet --parse                                                   $(TESTPARSEFILE)
	$(TESTER) --quiet       --rules=eval --no-under-lambda                      $(TESTFILE)
#	$(TESTER) --quiet --eval --split                                            $(TESTFILE)
	$(TESTER) --quiet --dfs --rules=POPL                   --max-steps=25000    $(TESTFILE)
	$(TESTER) --quiet --dfs --rules=POPLF                  --max-steps=25000    $(TESTFILE)
	$(TESTER) --quiet --dfs --rules=Block                  --max-steps=25000    $(TESTFILE)
	$(TESTER) --quiet --dfs --rules=POPLL                  --max-steps=25000    $(TESTFILE)
	$(TESTER) --quiet --dfs --rules=PLDI --final-inline    --max-steps=25000    $(TESTFILE)

testnorec:
	$(TESTER) --quiet --dfs --rules=POPLR                  --max-steps=25000    $(TESTFILE)

testblock:
	$(TESTER) --quiet --dfs --rules=Block                  --max-steps=1000     $(TESTFILE)
	$(TESTER) --quiet       --rules=Block                  --max-steps=25000    $(TESTFILE)

testblockqc:
	$(QCTEST) --rules=BlockA --ignore-fuel-stop --max-success=1000000

testpoplaqc:
	$(QCTEST) --rules=POPLA --ignore-fuel-stop --max-success=1000000

testpoplxqc:
	$(QCTEST) --rules=POPLX --ignore-fuel-stop --max-success=1000000

testicfpqc:
	$(QCTEST) --rules=ICFP --ignore-fuel-stop --max-success=1000000 --ignore-recursive

testpopll:
	$(TESTER) --quiet --dfs --rules=POPLL --max-steps=1000                          $(TESTFILE)
	$(TESTER)               --rules=POPLL --max-steps=25000 --max-norm-steps=100000 $(TRICKY)
	$(TESTER) --quiet       --rules=POPLL --max-steps=25000                         $(TESTFILE)

testpopllqc:
	$(QCTEST) --rules=POPLL --ignore-fuel-stop --max-success=1000000

testnorecqc:
	$(QCTEST) --rules=POPLR --ignore-fuel-stop --max-success=10000

trickytest: testtricky

testtricky:
	$(TESTER) --quiet --summary --no-error --all-rules --max-norm-steps=100000 --max-steps=4000 $(TRICKY)

testqcconf:
	$(QCTEST) --rules=POPLV --ignore-fuel-stop
	$(QCTEST) --rules=POPLV --max-success=30

teststore:
	$(TESTER) --rules=eval --split --no-under-lambda versetests/storetests.versetest

teststorepopl:
	$(TESTER) --rules=popls --split --final-inline --max-steps=200000 --no-under-lambda versetests/storetests.versetest

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
	cabal v1-install -p logict
	cabal v1-install -p prettyprinter
	cabal v1-install -p comonad
	cabal v1-install -p semigroupoids
	cabal v1-install -p optparse-applicative
