HAPPYFILE=../VerseReferenceImpl/src/Language/Verse/Parse.y convert.sh non-term.ebnf
grammar.ebnf:	$(HAPPYFILE)
	sh convert.sh < $(HAPPYFILE) > grammar.ebnf
