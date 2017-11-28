thesis.pdf: *.tex Makefile
	pdflatex -halt-on-error master.tex

clean:
	rm -f *.aux *.log *.toc
