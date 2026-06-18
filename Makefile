.PHONY: test

# Run various tests to make sure everything works before a checking
test:
	cabal build
	cabal run tester -- versetests/tests.versetest --evaluator=essential --assume-verified
