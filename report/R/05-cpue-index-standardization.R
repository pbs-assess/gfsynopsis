# CPUE model fits -----------------------------------------------------

d_cpue <- readRDS(file.path(dc, "cpue-index-dat.rds"))
if (parallel_processing && is_hake_server()) future::plan(future::multicore, workers = 4L)
if (!is_hake_server()) future::plan(future::sequential)
source(here("report/cpue-sdmTMB.R"))
future::plan(sequential)

