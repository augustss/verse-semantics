.SUFFIXES: .pdf .ps .ltx .tex $(.SUFFIXES)
.PHONY: all clean realclean

NAME=simplified

# Top-level target
all: $(NAME).pdf

LATEX=pdflatex -halt-on-error

PAPERSRC = $(NAME).tex rewrites.tex

rewrites.tex:   rewrites.ltx   ../calculus.fmt

$(NAME).tex: $(NAME).ltx ../calculus.fmt

$(NAME).pdf: $(PAPERSRC)
	$(LATEX) $(NAME)
	bibtex $(NAME)
	$(LATEX) $(NAME)
	$(LATEX) $(NAME)

# Fake targets
##############
clean:
	-rm -f $(NAME).tex rewrites.tex
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
