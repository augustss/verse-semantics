RUN=cabal run ebnf -- ../TimGrammar.txt

test: ../TimGrammar.txt ../TimAbout.verse all.verse
	$(RUN) all.verse
	$(RUN) ../TimAbout.verse all.verse

clean:
	cabal clean
	rm -f *~
