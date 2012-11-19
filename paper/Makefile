
all: annbib.pdf lit-review.pdf ext-abstract.pdf
	
lit-review.pdf : lit-review.tex annbib.bib
	pdflatex lit-review
	bibtex lit-review
	pdflatex lit-review
	pdflatex lit-review
	
ext-abstract.pdf : ext-abstract.tex annbib.bib
	pdflatex ext-abstract
	bibtex ext-abstract
	pdflatex ext-abstract
	pdflatex ext-abstract

annbib.pdf : annbib.tex annbib.bib
	pdflatex annbib
	bibtex annbib
	pdflatex annbib
	pdflatex annbib

clean-tmp:
	rm -f *.aux *.bbl *.blg *.log

clean:
	rm -f *.aux *.bbl *.blg *.log *.pdf
