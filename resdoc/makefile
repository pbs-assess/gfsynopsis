all:pdf  
MAINFILE  := gf-synopsis-wp
RNWFILES  := methods.Rnw, introduction.Rnw spatial-appendix.Rnw
RFILES    := 
TEXFILES  := 
CACHEDIR  := cache
FIGUREDIR := fig
LATEXMK_FLAGS := 
BIBFILES  := refs.bib

# -------------
RNWTEX = $(RNWFILES:.Rnw=.tex)
ROUTFILES = $(RFILES:.R=.Rout)
RDATAFILES= $(RFILES:.R=.Rdata)
MAINTEX = $(MAINFILE:=.tex)
MAINPDF = $(MAINFILE:=.pdf)
ALLTEX = $(MAINTEX) $(RNWTEX) $(TEXFILES)
KNITCMD="library('knitr'); \
knitr::opts_chunk[['set']](prefix.string='$(FIGUREDIR)/$*-');\
knitr::opts_chunk[['set']](prefix.cache='$(CACHEDIR)/$*-');\
knit('$<','$@')"

# Dependencies
$(RNWTEX): $(RDATAFILES)
$(MAINTEX): $(RNWTEX) $(TEXFILES)
$(MAINPDF): $(MAINTEX) $(ALLTEX) 

.PHONY:pdf tex clean clearcache cleanall
pdf: $(MAINPDF)
tex: $(RDATAFILES) $(ALLTEX) 

$(CACHEDIR):
	mkdir $(CACHEDIR)
	
$(FIGUREDIR):
	mkdir $(FIGUREDIR)

%.tex:%.Rnw | $(CACHEDIR) $(FIGUREDIR)
	Rscript -e $(KNITCMD)

%.R:%.Rnw
	Rscript -e "Sweave('$^', driver=Rtangle())"

%.Rout:%.R
	R CMD BATCH "$^" "$@"

%.pdf: %.tex 
	latexmk -pdf $<
	
clean:
	-latexmk -c -quiet $(MAINFILE).tex
	-rm -f Rplots.pdf $(MAINTEX) $(RNWTEX)
	-rm -rf $(FIGUREDIR)
	-rm *tikzDictionary
	-rm $(MAINPDF)
	-rm -f $(MAINFILE).bbl
	-rm -f $(MAINFILE).upa
	-rm -f $(MAINFILE).pdf
	
clearcache:
	-rm -rf cache *.map
  
cleanall: clean clearcache

rtf: $(TEXT).pdf
	latex2rtf -f0 -M3 -E0 $(TEXT).tex

continuous:
	while true; do make --silent; sleep 1; done
