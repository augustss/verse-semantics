PREL=Prelude.hs
Prelude.hs: quote.sh noprelude.verse miniprelude.verse verifyprelude.verse verifyabsprelude.verse mediumprelude.verse
	mv $(PREL) $(PREL).bak
	sed -e '/-- snip -- snip --/,$$d' $(PREL).bak > $(PREL)
	echo '-- snip -- snip --' >> $(PREL)
	sh quote.sh noprelude >> $(PREL)
	sh quote.sh miniprelude >> $(PREL)
	sh quote.sh verifyprelude >> $(PREL)
	sh quote.sh verifyabsprelude >> $(PREL)
	sh quote.sh mediumprelude >> $(PREL)
