# gfsynopsis: An R package for generating an annual data synopsis report for groundfish

The combination of fishery dependent data, such as catch and effort, and fishery independent survey data, such as biomass indices and age compositions, forms the backbone of most fisheries stock assessments. For British Columbia groundfish fisheries, we collect vast quantities of such data with 100% at-sea observer coverage, 100% dockside monitoring of landings, and multiple trawl, trap, and hook-and-line surveys deployed annually. However, we lack the capacity to conduct formal stock assessments for most stocks annually, and therefore, much of this data is not regularly published or readily accessible. We are developing a reproducible report to give a snapshot of long-term and recent population and fishing trends and data availability for all British Columbia groundfish stocks of commercial and conservation interest. The report generation is fully automated --- pulling data from databases, generating visualizations, and stitching the document together to facilitate annual publication. Our goals are (1) to facilitate groundfish scientists and managers regularly reviewing trends in survey indices and stock composition to potentially flag stocks for prioritized assessment; (2) to generate standardized datasets and visualizations that will help assessment scientists develop operating models and select candidate management procedures as part of a planned management-procedure framework for data-limited groundfish stocks; and (3) to increase data transparency between Fisheries and Oceans Canada, the fishing industry, non-governmental organizations, and the public.

This package uses the data extraction, data tidying, model fitting, and plotting functions from [gfplot](https://github.com/pbs-assess/gfplot).

# Building the document

1. Install gfsynopsis and csasdown:

```r
# install.packages("devtools")
devtools::install_github("pbs-assess/gfsynopsis")
devtools::install_github("pbs-assess/csasdown")
```

2. Clone or download the gfsynopsis GitHub repository.

3. With the R working directory set to the root folder of the project (e.g. open the RStudio `gfsynopsis.Rproj` file), run:

```r
source("report/make.R")
```

4. Wait for a long time for all the data to download, all the models to fit, and all the plots to generate. If you are starting from scratch then it may take a day or so to fit all the commercial CPUE index standardization models. If you already have these results cached, then it could take anywhere from ~10 minutes to a couple hours depending what needs to be built.

5. In RStudio, open `gfsynopsis/report/report-rmd/index.Rmd` and click the "Knit" button, or in any R console run:

```r
setwd("report/report-rmd")
bookdown::render_book("index.Rmd")
setwd("../../")
```

Problems? Sorry! File an [issue](https://github.com/pbs-assess/gfsynopsis/issues).
