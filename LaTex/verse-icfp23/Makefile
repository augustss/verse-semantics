.SUFFIXES: .pdf .ps .ltx .tex $(.SUFFIXES)
.PHONY: all clean realclean

NAME=icfp23

# Top-level target
all: $(NAME).pdf

LATEX=pdflatex -halt-on-error

PAPERSRC = $(NAME).tex rewrites.tex densem-icfp23.tex appendixA.tex store-icfp23.tex ../confluence/03-preliminaries.tex ../confluence/04-confluence.tex ../confluence/08-skew.tex ../verse.bib

rewrites.tex:   rewrites.ltx   ../calculus.fmt
store-icfp23.tex: store-icfp23.ltx ../calculus.fmt
densem-icfp23.tex: densem-icfp23.ltx ../calculus.fmt
appendixA.tex: appendixA.ltx ../calculus.fmt

$(NAME).tex: $(NAME).ltx ../calculus.fmt

$(NAME).pdf: $(PAPERSRC)
	$(LATEX) $(NAME)
	bibtex $(NAME)
	$(LATEX) $(NAME)
	$(LATEX) $(NAME)

# Fake targets
##############
clean:
	-rm -f $(NAME).tex rewrites.tex store-icfp23.tex appendixA.tex
	-rm -f $(NAME).aux $(NAME).log $(NAME).out \
               $(NAME).pdf $(NAME).bbl $(NAME).blg \
               $(NAME).ptb comment.cut

# Total reset -- delete intermediates *and* final derives
realclean: clean
	-rm -f $(NAME).pdf

# Implicit rules
################

.ltx.tex:
	lhs2TeX $< > $@

.tex.pdf:
	$(LATEX) $<
	bibtex $<
	$(LATEX) $<
	$(LATEX) $<
