# Clean up the directory after a LaTeX build. Windows version
del *.aux
del *.bbl
del *.blg
del *.dvi
del *.log
del *.lof
del *.lot
del *.ps
del *.toc
del *.txt
del *.out
del *.snm
del *.nav
del *.upa
del *.vrb
del *-concordance.tex
del *.synctex.gz*
del *.tex
del *.pdf
rmdir out-csv /S /Q
rmdir /S /Q knitr-cache
