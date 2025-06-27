COQMAKEFILE=CoqMakefile
LIBNAME=verse
SYNTAX=$(shell ls syntax/*.sig)
SYNTAXV=$(patsubst %.sig,%.v,$(SYNTAX))

.PHONY: coq syntax clean fullclean


# compile all .v files in _CoqProject
coq: CoqSrc.mk $(VFILES)
	make -f CoqSrc.mk

CoqSrc.mk: _CoqProject 
	 coq_makefile -arg '-w -variable-collision,-meta-collision,-require-in-module,-notation-overridden' -f _CoqProject -o CoqSrc.mk

# Remove compiled files
clean:
	make -f CoqSrc.mk clean

%.vo: %.v CoqSrc.mk
	make -f CoqSrc.mk $*.vo

syntax/%.v: syntax/%.sig Makefile syntax/imports.v
	autosubst -no-static -f -s rocq -v ge813 -o $@ $< -p syntax/imports.v

realclean: clean
	rm -f syntax/mini.v
	rm -f syntax/core.v
	rm CoqSrc.mk
