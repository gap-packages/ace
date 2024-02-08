
# Makefile for ACE 4.1xx doco, Colin Ramsay, 24 Sep 03, 25 Apr 14
# adapted by Dima Pasechnik to use xelatex 20 Mar 23

textargets := $(wildcard *.tex)

all: guide

styles: styles.mp
	mpost styles

bib: guide_one
	bibtex ace41

guide_one: styles $(textargets)
	xelatex ace41

guide: guide_one bib
	bibtex ace41
	xelatex ace41
	xelatex ace41

clean:
	rm -f *.aux *.bbl *.blg *.dvi *.lof *.log *.lot *.pdf *.ps *.toc *.mpx
	rm -f styles*.eps
	rm -f *~


