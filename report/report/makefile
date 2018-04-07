all:pdf

pdf: 
	Rscript -e "library(knitr);knit('pbs-gf-synopsis.Rnw')"
	pdflatex pbs-gf-synopsis.tex
	pdflatex pbs-gf-synopsis.tex
