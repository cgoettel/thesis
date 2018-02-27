master.pdf: *.tex Makefile *.sty
	pdflatex -halt-on-error master
	biber master
	pdflatex master
	pdflatex master
	pdflatex master

clean:
	rm -f *.aux *.log *.toc *.bbl *.lof *.lot *.bst *.brf *.blg *.out *.bcf *.xml
