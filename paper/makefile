TEXT = paper
REFS = refs

all: $(TEXT).pdf

%.pdf: %.Rmd $(REFS).bib
	Rscript -e "source('compile.R')"
continuous:
	while true; do make --silent; sleep 3; done

response:
	Rscript -e "rmarkdown::render('response.md')"

