rits_architecture.pdf: rits_architecture.ps
	ps2pdf rits_architecture.ps

rits_architecture.ps: rits_architecture.dvi
	dvips rits_architecture.dvi

rits_architecture.dvi: rits_architecture.tex utrits.ps
	latex rits_architecture.tex
