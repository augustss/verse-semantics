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
# PLDI rules no longer terminate with structural rules.
#	$(TESTER) --quiet --dfs            $(TESTFILE)
#	$(TESTER) --quiet --dfs --split    $(TESTFILE)
#	$(TESTER) --quiet --dfs --simplify $(TESTFILE)
#	$(TESTER) --quiet --dfs --popl     $(TESTFILE)

clean:
	cabal clean
	rm -f *~
