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

DISS =	status.tex ch1.tex ch2.tex ch3.tex ch4.tex ch5.tex aluform.eps fdo.eps \
	feedback.eps runtime1.eps runtime2.eps runtime3.eps runtime4.eps \
	toolchain.eps

run:	pass2

again:	pass3
	latex status

pass3:	pass2
	latex status
	touch pass3

pass2:	pass1
	latex status
	touch pass2

pass1:	$(DISS)
	latex status
#	bibtex status
	touch pass1

clean:
	rm -f abstract.ps *.dvi *.aux *.log *.err
	rm -f core *~ *.lof *.toc *.blg *.bbl
	rm -f *.lof *.toc *.blg *.bbl
	rm -f pass1 pass2 pass3

xdvi:	pass3
	xdvi abstract.dvi

gv:	status.ps
	ghostview abstract.ps

status.ps:	pass3
	dvips -j1 status.dvi -o status.ps

status.pdf:	pass3
	dvipdf status.dvi status.pdf

status-odd.ps:	pass3
	dvips -A abstract.dvi -o abstract-odd.ps

status-even.ps:	pass3
	dvips -B abstract.dvi -o abstract-even.ps

ch3.aux: aluform.eps
