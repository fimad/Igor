################################################################################
# Run `make` to build the latest version of the paper. To build drafts of
# individual chapters, run `make draft_chapter.pdf` where chapter is the title
# of the chapter to be created.
################################################################################

paper.pdf: \
	date.tex \
	chapter_intro.tex \
	chapter_related.tex \
	chapter_method.tex \
	chapter_results.tex \
	chapter_conclusion.tex

all: paper.pdf

#Hack to get the current date in latex, to make sure that the date is up to date
#run `make clean` before building
date.tex:
	date "+%B %d, %Y" | sed -r 's/^(.+)$$/\\date{\1}/g' > date.tex

#Generic pdf generator
%.pdf: %.tex
	pdflatex $*
	bibtex $*
	pdflatex $*
	pdflatex $*

#For generating individual chapter drafts
draft_%.pdf: draft.tex chapter_%.tex date.tex
	echo $* | python -c "print raw_input().capitalize()" > draft_title.tex
	cp chapter_$*.tex draft_input.tex
	pdflatex draft
	bibtex draft
	pdflatex draft
	pdflatex draft
	cp draft.pdf draft_$*.pdf

clean-tmp:
	rm -f *.toc *.lot *.lof *.aux *.bbl *.blg *.log draft_title.* draft_input.*

clean: clean-tmp
	rm -f *.pdf date.tex
