.PHONY: all clean

all: thesis.pdf

clean:
	rm -f *.aux *.bbl *.blg *.eps *.log *.out *.pdf

%.pdf: %.tex references.bib
	pdflatex $<
	# bibtex $(patsubst %.tex,%.aux,$<)
	# pdflatex $<
	# pdflatex $<   # *sigh*

%.eps: %.dia
	dia --export=$@ $<
