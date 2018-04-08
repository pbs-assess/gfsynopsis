all:pdf

pdf:
	Rscript -e "library(knitr);knit('pbs-gf-synopsis.Rnw')"
	latexmk -pdf pbs-gf-synopsis.tex
