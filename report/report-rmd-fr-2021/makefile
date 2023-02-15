all:pdf

pdf:
	Rscript -e "bookdown::render_book('index.Rmd')"

compressfigs:
	cd knitr-figs
	find -X . -name '*.png' -print0 | xargs -0 -n 7 -P 4 optipng -strip all
	
compresslogos:
	cd csas-style/images/
	find -X . -name '*.png' -print0 | xargs -0 optipng -strip all

bibclean:
	cp bib/spp-refs.bib bib/spp-refs-backup.bib
	bibtool --print.use.tab=Off --print.line.length=80 -i bib/spp-refs.bib -o bib/spp-refs.bib
