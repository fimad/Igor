
all: annbib.pdf
	

annbib.pdf : annbib.tex annbib.bib
	pdflatex annbib
	bibtex annbib
	pdflatex annbib
	pdflatex annbib

clean-tmp:
	rm -f *.aux *.bbl *.blg *.log

clean:
	rm -f *.aux *.bbl *.blg *.log *.pdf
