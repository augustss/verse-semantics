# Temporary Makefile to run tests
TESTFILE=versetests/tests.versetest
TESTSTOREFILE=versetests/storetests.versetest
TRICKY=versetests/tricky.versetest
VERIFY=versetests/verify.versetest
TRACEVERIFY=versetests/verify1.versetest
TESTPARSEFILE=versetests/test1.verse
.PHONY: all test testfast clean

TESTER=./dist-newstyle/build/*/*/HaskellRules-0.1.0.0/x/tester/build/tester/tester
TESTER=cabal run tester --
QCTEST=cabal run qctest --

RULES=ICFP
VRULES=SPLITverify

all: build testiblock testrulesfast

allv: build testiblock testverify testrules

build:
	cabal build

testrulesfast:
	$(TESTER) --quiet --dfs --rules=$(RULES) $(EXTRA)                    --assume-verified --max-steps=250000       $(TESTFILE)

testrules:
	$(TESTER) --quiet --dfs --rules=$(RULES) $(EXTRA)                    --assume-verified --max-steps=250000       $(TESTFILE)
	$(QCTEST)               --rules=$(RULES) $(EXTRA) --ignore-fuel-stop --max-success=1000 --ignore-recursive --koen
	$(TESTER) --quiet       --rules=$(RULES) $(EXTRA) --max-steps=500000 --assume-verified --max-norm-steps=100000 $(TRICKY)

testiblock:
	$(TESTER) --quiet --split --rules=iblock --assume-verified --max-steps=25000    $(TESTFILE)
	$(TESTER) --quiet --split --rules=iblock --assume-verified --max-steps=25000    $(TRICKY)
	$(TESTER) --quiet --split --rules=iblock --assume-verified --max-steps=25000    $(TESTSTOREFILE)

testverify:
	$(TESTER) --quiet         --prelude=verifyprelude --rules=$(VRULES) --max-norm-steps=100000 $(VERIFY)
	cabal run trsverifier

traceverify:
	$(TESTER) --quiet --prelude=verifyprelude --rules=$(VRULES) --max-norm-steps=100000 $(TRACEVERIFY)

testicfpqc:
	$(QCTEST) --rules=ICFP --ignore-fuel-stop --max-success=1000000 --ignore-recursive

testicfpstore:
	$(TESTER) --quiet --dfs --split --rules=ICFPS --post-process --assume-verified --max-steps=25000 $(TESTSTOREFILE)

testicfpstoreqc:
	$(QCTEST) --rules=ICFPS --ignore-fuel-stop --max-success=100000 --ignore-recursive

testtricky:
	$(TESTER) --quiet --summary --no-error --all-rules --assume-verified --max-norm-steps=100000 --max-steps=4000 $(TRICKY)

testtimverify:
	$(TESTER) --tim-verify --prelude=verifyprelude --rules=$(VRULES) versetests/TimTests.verse

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
