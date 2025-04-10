# This file generates all the main synopsis figures in `report/figure-pages`.

# It must be run before the report can be rendered.

# Things to update each year:
# - [ ] update the .csv in R/join_refs_spp.R
# - [ ] update the .csv in R/get_cosewic_data.R
# - [x] update the IPHC survey set data
# - [x] pull all new data from the servers; done as part of this file with get_data()
# - [x] update the years in the call to make_pages() below

args <- commandArgs(trailingOnly = TRUE)
if (length(args)) {
  ii <- as.numeric(args[1])
  interactive <- FALSE
} else {
  interactive <- TRUE
}

# ii <- 21
# interactive <- FALSE

library(here)
source(here("report/R/01-settings.R"))
# source(here("report/R/01-settings-haida.R"))
source(here("report/R/02-packages.R"))
source(here("report/R/03-load-data.R"))
# source(here("report/R/04-survey-index-standardization.R"))
# source(here("report/R/05-cpue-index-standardization.R"))
source(here("report/R/06-build-figure-pages.R"))
# source(here("report/R/07-cache-ggplots.R"))
# source(here("report/R/08-build-Rmd.R"))
# source(here("report/R/09-optimize-png.R"))

if (FALSE) {
  setwd(here("report/tech-report-main/"))
  csasdown::render()
  setwd(here("."))
}
if (FALSE) {
  setwd(here("report/tech-report-haida/"))
  csasdown::render()
  setwd(here("."))
}
