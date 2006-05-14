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

DISS =	paper.tex

run:	pass2

again:	pass3
	latex paper

pass3:	pass2
	latex paper
	touch pass3

pass2:	pass1
	latex paper
	touch pass2

pass1:	$(DISS)
	latex paper
	bibtex paper
	touch pass1

paper.dvi: paper.tex
	latex paper

all:	paper.ps

clean:
	rm -f abstract.ps *.dvi *.aux *.log *.err
	rm -f core *~ *.lof *.toc *.blg *.bbl
	rm -f *.lof *.toc *.blg *.bbl
	rm -f pass1 pass2 pass3

xdvi:	pass3
	xdvi abstract.dvi

gv:	diss.ps
	ghostview abstract.ps

paper.ps:	pass3
	dvips -j1 paper.dvi -o paper.ps

paper.pdf:	paper.ps
	dvipdf paper.dvi

