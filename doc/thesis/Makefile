# This is the Makefile for the demonstration dissertation
# written by Martin Richards
# modified by J Brown
#
# Note that continuation lines require '\'
# and that TAB is used after ':' and before unix commands.

# USAGE:
#
# make                     -- to paginate the dissertation
#
# make proposal.dvi        -- to paginate  proproposal
#
# make xdvi                -- view the dissertation with xdvi
#
# make diss.ps             -- make a postscript version of the dissertation
#
# make gv                  -- view the dissertation with ghostview
#
# make all                 -- construct proposal.dvi, diss.dvi and diss.ps
#
# make clean               -- remove all remakeable files

DISS =	abstract.tex diss.tex ch1.tex ch2.tex ch3.tex ch4.tex ch5.tex \
	ch6.tex ch7.tex ch8.tex apformat.tex

run:	pass2

again:	pass3
	latex diss

pass3:	pass2
	latex diss
	touch pass3

pass2:	pass1
	latex diss
	touch pass2

pass1:	$(DISS)
	latex diss
#	bibtex diss
	touch pass1

abstract.dvi: abstract.tex
	latex abstract

all:	abstract.dvi

clean:
	rm -f abstract.ps *.dvi *.aux *.log *.err
	rm -f core *~ *.lof *.toc *.blg *.bbl
	rm -f *.lof *.toc *.blg *.bbl
	rm -f pass1 pass2 pass3

xdvi:	pass3
	xdvi abstract.dvi

gv:	diss.ps
	ghostview abstract.ps

diss.ps:	pass3
	dvips -j1 diss.dvi -o diss.ps

diss-odd.ps:	pass3
	dvips -A abstract.dvi -o abstract-odd.ps

diss-even.ps:	pass3
	dvips -B abstract.dvi -o abstract-even.ps

ch3.aux: aluform.eps
