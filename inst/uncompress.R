library(gfsynopsis)
library(gfplot)
library(dplyr)

spp <- get_spp_names()

for (i in spp$spp_w_hyphens) {
  cat(i)
  cat("\n")
  d <- readRDS(paste0("report/data-cache3/", i, ".rds"))
  saveRDS(
    d,
    file = paste0("report/data-cache3-uncompressed/", i, ".rds"),
    compress = FALSE
  )
}
