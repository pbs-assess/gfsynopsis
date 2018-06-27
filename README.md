# gfsynopsis: An R package for generating an annual data synopsis report for groundfish

The combination of fishery dependent data, such as catch and effort, and fishery independent survey data, such as biomass indices and age compositions, forms the backbone of most fisheries stock assessments. For British Columbia groundfish fisheries, we collect vast quantities of such data with 100% at-sea observer coverage, 100% dockside monitoring of landings, and multiple trawl, trap, and hook-and-line surveys deployed annually. However, we lack the capacity to conduct formal stock assessments for most stocks annually, and therefore, much of this data is not regularly published or readily accessible. We are developing a reproducible report to give a snapshot of long-term and recent population and fishing trends and data availability for all British Columbia groundfish stocks of commercial and conservation interest. The report generation is fully automated --- pulling data from databases, generating visualizations, and stitching the document together to facilitate annual publication. Our goals are (1) to facilitate groundfish scientists and managers regularly reviewing trends in survey indices and stock composition to potentially flag stocks for prioritized assessment; (2) to generate standardized datasets and visualizations that will help assessment scientists develop operating models and select candidate management procedures as part of a planned management-procedure framework for data-limited groundfish stocks; and (3) to increase data transparency between Fisheries and Oceans Canada, the fishing industry, non-governmental organizations, and the public. We have developed an early version of the report and will be refining it based on consultation with other groundfish scientists, fisheries managers, industry representatives, and other interested parties.

This package uses the data extraction, data tidying, model fitting, and plotting functions from [gfplot](https://github.com/pbs-assess/gfplot).

# Installation

The gfsynopsis package is *not* ready for use yet. However, it can be installed with:

```r
# install.packages("devtools")
devtools::install_github("pbs-assess/gfsynopsis")
library("gfsynopsis")
```

# Building the document

Here's what Andy did to get things working (can move this somewhere else at some point, just want to keep track). In base folder

```r
cache_pbs_data(path="data-cache3-uncompressed")
```
which takes a while and created a 4.23Gb folder of 61 files (includes type A and B species, as listed in `inst\extdata\spp-of-interest.csv`, though looks like 5 might be missing [not loking into right now]). Using that pathname because that is what is used in `report\make.R` which is run next and creates `report\data-cache3-uncompressed` which is 4.02Gb and 32 files (just type A species).
