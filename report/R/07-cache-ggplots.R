# Extract saved ggplot objects for example plots ----------------------
# These objects are too big to cache in an .Rmd file otherwise.
# if (!exists("ii")) {
  if (!french) {
    g_alt <- readRDS(paste0(build_dir, "/ggplot-objects/pacific-cod.rds"))
    saveRDS(g_alt$cpue_spatial, file = paste0(build_dir, "/ggplot-objects/pacific-cod-cpue-spatial.rds"))
    saveRDS(g_alt$cpue_spatial_ll, file = paste0(build_dir, "/ggplot-objects/pacific-cod-cpue-spatial-ll.rds"))
  }
  if (french) {
    g_alt <- readRDS(paste0(build_dir, "/ggplot-objects/pacific-cod.rds"))
    saveRDS(g_alt$cpue_spatial, file = paste0(build_dir, "/ggplot-objects/pacific-cod-cpue-spatial.rds"))
    saveRDS(g_alt$cpue_spatial_ll, file = paste0(build_dir, "/ggplot-objects/pacific-cod-cpue-spatial-ll.rds"))
  }
# }

