Rscript -e "library(knitr);knit('./pbs-gf-synopsis.rnw')" 1> knitr-output.log 2>&1

(@pdflatex -synctex=1 "pbs-gf-synopsis.tex" && pdflatex "pbs-gf-synopsis.tex" && bibtex "pbs-gf-synopsis" && pdflatex "pbs-gf-synopsis.tex" && pdflatex "pbs-gf-synopsis.tex") 1> latex-output.log 2>&1
