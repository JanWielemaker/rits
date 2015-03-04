slides.pdf: slides.ps
	ps2pdf -dNOSAFER $<

slides.ps: slides.dvi
	dvips $<

slides.dvi: slides.tex
	latex $<
