## test cpue with Haida shape file
cpue_cache <- file.path("report", "cpue-sdmTMB-cache")
raw_cpue_cache <- file.path("report", "raw-cpue-cache")

shp_dir <- here::here("report", "spatial-filtering", "shape-files", "haida")
shp_file <- "Haida_Territory.shp"

shp <- sf::read_sf(file.path(shp_dir, shp_file))

# xx <- "Dover Sole"
xx <- "Lingcod"

purrr::walk(xx, \(.sp) {
  spp_file <- gfsynopsis:::clean_name(.sp)
  cpue_cache_spp <- paste0(file.path(cpue_cache, spp_file), ".rds")
  raw_cpue_cache_spp <-
    regions <- list(
      c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG"),
      c("SYN HS", "SYN WCHG"),
      c("SYN QCS"),
      c("SYN WCVI")
    )
  shp <- sf::read_sf(file.path(shp_dir, shp_file))
  if (!file.exists(cpue_cache_spp)) {
    cat(.sp, "\n")
    cpue_index_l <- lapply(regions, \(r) {
      .r <- gsub(" ", "-", paste(r, collapse = "-"))
      .f <- paste0(file.path(raw_cpue_cache, paste0(spp_file, "-", .r)), ".rds")
      ret <- fit_sdmTMB_cpue(
        cpue_data_file = here::here("report/data-cache/cpue-index-dat.rds"), # update this
        raw_cpue_caching_file = here::here(.f),
        survey_grids = r,
        final_year = 2023,
        index_shape_file = shp, # new
        species = .sp
      )
      gc()
      ret
    })
    cpue_index <- do.call(rbind, cpue_index_l)
    saveRDS(cpue_index, file = cpue_cache_spp, compress = FALSE)
  }
})
