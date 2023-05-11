HAPPYFILE=../VerseReferenceImpl/src/Language/Verse/Parse.y
top: grammar.ebnf tim.ebnf tim.xbnf

grammar.ebnf:	$(HAPPYFILE) convert.sh non-term.ebnf
	sh convert.sh < $(HAPPYFILE) > grammar.ebnf

tim.ebnf:	TimGrammar.txt timconvert.sh
	sh timconvert.sh > tim.ebnf

tim.xbnf:	TimGrammar.txt Makefile
	sed < TimGrammar.txt -n -e '/^Alpha/,/^File/p' | sed -e '/ := /s/^/;;/' > tim.xbnf

# Use https://bottlecaps.de/rr/ui to convert to SVG
