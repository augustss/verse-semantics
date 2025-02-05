.PHONY: test

# Run various tests to make sure everything works before a checking
test:
	cabal build
	cabal run tester -- versetests/tests.versetest
	cabal run tester -- versetests/verify.versetest -m 1600
	cabal run tester -- versetests/meetings.versetest
	# cabal run tester -- versetests/arrays.versetest
	#    Omit arrays till we get them working again
	# cabal run refimpltester -- commented out for now

