main.pdf: *.tex Makefile
	pdflatex -halt-on-error main
	bibtex main
	pdflatex main
	pdflatex main
	pdflatex main

clean:
	rm -f *.aux *.bbl *.blg *.brf *.lof *.log *.lot *.out *.toc
