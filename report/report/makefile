all:pdf

pdf:
	Rscript -e "library(knitr);knit('pbs-gf-synopsis.Rnw')"
	latexmk -pdf pbs-gf-synopsis.tex

clean:
	latexmk -c pbs-gf-synopsis.tex
	rm *.synctex.gz
	rm *.upa
	rm pbs-gf-synopsis.tex
	rm -rf knitr-cache/
	rm -rf knitr-figs/
