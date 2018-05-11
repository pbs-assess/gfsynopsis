spp <- c("lingcod", "quillback-rockfish", "yelloweye-rockfish", "copper-rockfish")
surv <- c("iphc", "hbll", "synoptic")
dir.create("inst/rockfish-map-dat", showWarnings = FALSE)

for (i in spp) {
  for (j in surv) {
  d <- readRDS(paste0("report/map-cache/synoptic/", i, ".rds"))
  d <- d$pred_dat
  d <- dplyr::rename(d, depth = akima_depth)
  d <- dplyr::select(d, -depth_scaled, -depth_scaled2)
  readr::write_csv(d, paste0("inst/rockfish-map-dat/", i, "-", j, ".csv"))
}}
