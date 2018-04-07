all:pdf

pdf: 
	Rscript -e "library(knitr);knit('mwe.Rnw')"
	pdflatex mwe.tex
	pdflatex mwe.tex
	pdflatex mwe.tex

