
all:	slides.ps

clean:
	rm slides.aux slides.dvi slides.log slides.ps

slides.dvi:	slides.tex
	latex slides.tex

slides.ps:	slides.dvi
	dvips slides.dvi -o slides.ps
