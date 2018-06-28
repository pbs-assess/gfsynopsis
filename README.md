# gfsynopsis: An R package for generating an annual data synopsis report for groundfish

The combination of fishery dependent data, such as catch and effort, and fishery independent survey data, such as biomass indices and age compositions, forms the backbone of most fisheries stock assessments. For British Columbia groundfish fisheries, we collect vast quantities of such data with 100% at-sea observer coverage, 100% dockside monitoring of landings, and multiple trawl, trap, and hook-and-line surveys deployed annually. However, we lack the capacity to conduct formal stock assessments for most stocks annually, and therefore, much of this data is not regularly published or readily accessible. We are developing a reproducible report to give a snapshot of long-term and recent population and fishing trends and data availability for all British Columbia groundfish stocks of commercial and conservation interest. The report generation is fully automated --- pulling data from databases, generating visualizations, and stitching the document together to facilitate annual publication. Our goals are (1) to facilitate groundfish scientists and managers regularly reviewing trends in survey indices and stock composition to potentially flag stocks for prioritized assessment; (2) to generate standardized datasets and visualizations that will help assessment scientists develop operating models and select candidate management procedures as part of a planned management-procedure framework for data-limited groundfish stocks; and (3) to increase data transparency between Fisheries and Oceans Canada, the fishing industry, non-governmental organizations, and the public. We have developed an early version of the report and will be refining it based on consultation with other groundfish scientists, fisheries managers, industry representatives, and other interested parties.

This package uses the data extraction, data tidying, model fitting, and plotting functions from [gfplot](https://github.com/pbs-assess/gfplot).

The gfsynopsis package is *not* ready for use yet. However, it can be installed with:

# Building the document

1. Install gfsynopsis:

```r
# install.packages("devtools")
devtools::install_github("pbs-assess/gfsynopsis")
library(gfsynopsis)
```

2. Clone or download the gfsynopsis repository.

3. With the R working directory set to the root folder of the project (e.g. open the RStudio `gfsynopsis.Rproj` file), run:

```r
source("report/make.R")
```

4. Wait for a very long time for all the data to download, all the models to fit, and all the plots to generate (maybe a few hours if starting from scratch).

5. On Unix, open a Terminal window, `cd` to the `gfsynopsis/report/report/` folder, and run `make` (after installing `latexmk`, perhaps with `homebrew`). Alternatively, or if not on Unix, run the following in R after changing the working directory to the `gfsynopsis/report/report/` folder:

```r
knitr::knit("pbs-gf-synopsis.Rnw")
```

Then install `latexmk` and on a command line outside of R:

```sh
latexmk -pdf pbs-gf-synopsis.tex
```

Or, if you don't have `latexmk` installed, then run:

```sh
pdflatex pbs-gf-synopsis.tex
bibtex pbs-gf-synopsis.tex
pdflatex pbs-gf-synopsis.tex
pdflatex pbs-gf-synopsis.tex
pdflatex pbs-gf-synopsis.tex
pdflatex pbs-gf-synopsis.tex
```

I may have under or over estimated the number of times you need to run `pdflatex pbs-gf-synopsis.tex`. Run it until the table of contents and all the references and figure references are up-to-date.

Problems? Sorry! File an [issue](https://github.com/pbs-assess/gfsynopsis/issues).
