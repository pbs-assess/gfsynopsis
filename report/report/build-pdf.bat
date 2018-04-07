Rscript -e "library(knitr);knit('./res-doc.rnw')" 1> knitr-output.log 2>&1

(@pdflatex -synctex=1 "res-doc.tex" && pdflatex "res-doc.tex" && bibtex "res-doc" && pdflatex "res-doc.tex" && pdflatex "res-doc.tex") 1> latex-output.log 2>&1
