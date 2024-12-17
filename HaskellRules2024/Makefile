.PHONY: test

# Run various tests to make sure everything works before a checking
test:
	cabal build
	cabal run tester -- versetests/tests.versetest -m 1500
	#cabal run tester -- versetests/tests.versetest --all-as-iter -m 1500
	cabal run tester -- versetests/verify.versetest -m 1500
	#cabal run tester -- versetests/verify.versetest --all-as-iter -m 1500
	cabal run tester -- versetests/meetings.versetest -m 1500
	#cabal run tester -- versetests/meetings.versetest --all-as-iter -m 1500
	cabal run tester -- versetests/arrays.versetest -m 1500
	cabal run refimpltester

