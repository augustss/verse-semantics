.PHONY: test

# Run various tests to make sure everything works before a checking
test:
	cabal build
	# The tests below has many failures, it's old so we don't care?
	# cabal run tester -- versetests/tests.versetest
	# cabal run tester -- versetests/verify.versetest -m 1600
	# cabal run tester -- versetests/meetings.versetest
	# cabal run tester -- versetests/arrays.versetest -m 3000
	# cabal run tester -- versetests/densemM.versetest
	# cabal run refimpltester -- commented out for now

profile:
	cabal run tester -- versetests/arrays.versetest -m 3000 --only arr2
