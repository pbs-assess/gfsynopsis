setwd(here::here())

args <- commandArgs(trailingOnly = TRUE)
if (length(args)) {
  ii <- as.numeric(args[1])
  interactive <- FALSE
} else {
  interactive <- TRUE
}

if (french) {
  options(french = TRUE)
  options(OutDec = ",")
}
is_rstudio <- !is.na(Sys.getenv("RSTUDIO", unset = NA))
is_unix <- .Platform$OS.type == "unix"

if (french) {
  build_dir <- paste0("report/tech-report-fr-", tag)
} else {
  build_dir <- paste0("report/tech-report-", tag)
}
library(here)
library(dplyr)
devtools::load_all("../gfplot/")
devtools::load_all(".") # gfsynopsis; weird hexbin errors if not load_all()ed
library(rosettafish)
library(future)
library(cli)
wd <- getwd()
if (!grepl("gfsynopsis", wd)) stop("Working directory wrong? Should be this repo main folder.")

is_hake_server <- function() {
  future::availableCores() > 50L
}
if (is_hake_server()) {
  cores <- 30L
  RhpcBLASctl::blas_set_num_threads(1) # default currently is all/80!
  RhpcBLASctl::omp_set_num_threads(1)
}

dir.create(file.path("report", paste0("cache-", tag)), showWarnings = FALSE)

