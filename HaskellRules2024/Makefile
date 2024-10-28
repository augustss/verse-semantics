.PHONY: test

# Run various tests to make sure everything works before a checking
test:
	cabal build
	cabal run tester -- versetests/tests.versetest
	cabal run tester -- versetests/tests.versetest --all-as-iter
	cabal run tester -- versetests/verify.versetest
	cabal run tester -- versetests/verify.versetest --all-as-iter
	cabal run tester -- versetests/meetings.versetest
	cabal run tester -- versetests/meetings.versetest --all-as-iter
	cabal run tester -- versetests/arrays.versetest
	cabal run refimpltester

