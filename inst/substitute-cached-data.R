# An example script to replace one of the cached data elements,
# here the commercial samples.

library('gfplot')
dc <- '~/src/gfsynopsis/report/data-cache/'
.d <- gfsynopsis::get_spp_names()
.d <- .d[.d$type %in% c('A', 'B'), , drop = FALSE]

for (i in seq_len(nrow(.d))) {
  message('Extracting ', .d$species_common_name[i])
  temporary <- get_commercial_samples(.d$species_code[i], unsorted_only = FALSE)
  existing <- readRDS(paste0(dc, .d$spp_w_hyphens[i], ".rds"))
  existing$commercial_samples <- NULL
  existing$commercial_samples <- temporary
  saveRDS(existing, file = paste0(dc, .d$spp_w_hyphens[i], ".rds"), compress = FALSE)
}
