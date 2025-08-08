# get all survey years to convert NAs to 0s:
dog <- readRDS(paste0(dc, "/north-pacific-spiny-dogfish.rds"))$survey_index
all_survey_years <- dplyr::select(dog, survey_abbrev, year) %>%
  dplyr::distinct()

# these are complex, do outside first:
source(here("report", "plot-indices.R"))
if (tag == "main") {
  stitch_cache <- here::here("report", paste0("cache-", tag), "stitch-cache")
} else {
  stitch_cache <- here::here("report", paste0("cache-", tag), "spatial-stitch-cache")
}
if (interactive) {
  index_ggplots <- furrr::future_map(spp$spp_w_hyphens, make_index_panel,
    all_survey_years = all_survey_years, shapefile = shapefile, french = french,
    stitch_cache = stitch_cache)
} else { # just do one because we're running in parallel for one species
  gg <- purrr::map(spp$spp_w_hyphens[ii], make_index_panel,
    all_survey_years = all_survey_years, shapefile = shapefile, french = french,
    stitch_cache = stitch_cache)
  index_ggplots <- list()
  index_ggplots[[ii]] <- gg[[1]]
}
# saveRDS(index_ggplots, file = here::here("report", paste0("cache-", tag), "index-ggplots.rds"), compress = FALSE)

# Make figure pages ---------------------------------------------------

message("Make figure pages")
fig_check <- file.path(
  build_dir, "figure-pages",
  gfsynopsis:::clean_name(spp$species_common_name)
)
fig_check1 <- paste0(fig_check, "-1.", ext)
fig_check2 <- paste0(fig_check, "-2.", ext)
missing <- !file.exists(fig_check1) | !file.exists(fig_check2)
for (i in which(!missing)) {
  cat(
    crayon::green(clisymbols::symbol$tick),
    "Figure pages for", spp$species_common_name[i], "already exist\n"
  )
}
missing_spp <- spp$species_common_name[missing]
to_build <- which(missing)

if (exists("ii")) {
  to_build <- to_build[to_build %in% ii]
}
cli_inform("Building")
cli_inform(paste(spp$species_common_name)[to_build])

# Trash compiled objects for safety:
# unlink("vb_gfplot.*")
# unlink("lw_gfplot.*")

for (i in to_build) {
  cli_inform("------------------------------------")
  cli_progress_step(paste0("Building figure pages for ", spp$species_common_name[i]), spinner = TRUE)

  dat <- readRDS(file.path(dc, paste0(spp$spp_w_hyphens[i], ".rds")))
  dat_iphc <- gfdata::load_iphc_dat(species = spp$species_common_name[i]) |>
    rename(lat = "latitude", lon = "longitude")
  hbll_bait_counts <- readRDS(file.path(dc, "bait-counts.rds"))

  length_ticks <- readr::read_csv(here("report/length-axis-ticks.csv"),
    show_col_types = FALSE
  ) |> as.data.frame()

  gfsynopsis::make_pages(
    dat = dat,
    dat_iphc = dat_iphc,
    spp = spp$species_common_name[i],
    all_survey_years = all_survey_years,
    d_geostat_index = NULL, # dat_geostat_index, # spatiotemporal model fits
    include_map_square = FALSE, # to check the map aspect ratio
    french = french,
    report_lang_folder = build_dir,
    tag = tag,
    resolution = 150, # balance size with resolution
    png_format = if (ext == "png") TRUE else FALSE,
    parallel = FALSE, # for CPUE fits; need a lot of memory if true!
    save_gg_objects = spp$species_common_name[i] %in% example_spp,
    synoptic_max_survey_years = list("SYN WCHG" = 2024, "SYN HS" = 2023, "SYN WCVI" = 2024, "SYN QCS" = 2023),
    hbll_out_max_survey_years = list("HBLL OUT N" = 2023, "HBLL OUT S" = 2024),
    final_year_comm = 2024,
    final_year_surv = 2024,
    length_ticks = length_ticks[length_ticks$species_code == spp$species_code[i], ],
    hbll_bait_counts = hbll_bait_counts,
    index_ggplot = index_ggplots[[i]],
    spatiotemporal_cpue = TRUE,
    raw_cpue = NULL,
    shapefile = shapefile
  )
}

