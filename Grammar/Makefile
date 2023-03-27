HAPPYFILE=../VerseReferenceImpl/src/Language/Verse/Parse.y
grammar.ebnf:	$(HAPPYFILE) convert.sh non-term.ebnf
	sh convert.sh < $(HAPPYFILE) > grammar.ebnf

# Use https://bottlecaps.de/rr/ui to convert to SVG
