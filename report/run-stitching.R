# to be run as part of `report/make.R`

furrr::future_walk(spp_vector, function(.sp) {
  # purrr::walk(spp_vector, function(.sp) {
  spp_filename <- paste0(gfsynopsis:::clean_name(.sp), "_", model_type, ".rds")
  stitch_cached_sp <- file.path(c(sc_synoptic, sc_hbll_out, sc_hbll_ins), spp_filename)

  if(any(!file.exists(stitch_cached_sp))) {
    survey_dat <- readRDS(file.path(dc, paste0(gfsynopsis:::clean_name(.sp), ".rds")))$survey_sets |>
      prep_stitch_dat(survey_dat = _, bait_counts = bait_counts)
  }

  get_stitched_index(
    survey_dat = survey_dat, species = .sp,
    survey_type = "synoptic", model_type = model_type, cache = sc_synoptic,
    grid_dir = grid_dir, check_cache = TRUE
  )

  get_stitched_index(
    survey_dat = survey_dat, species = .sp,
    survey_type = "hbll_outside", model_type = model_type, cache = sc_hbll_out,
    family = sdmTMB::nbinom2(link = "log"), grid_dir = grid_dir,
    check_cache = TRUE
  )

  get_stitched_index(
    survey_dat = survey_dat, species = .sp,
    survey_type = "hbll_inside", model_type = model_type, cache = sc_hbll_ins,
    family = sdmTMB::nbinom2(link = "log"), grid_dir = grid_dir,
    check_cache = TRUE
  )
})

# try delta-gamma
sc_synoptic_dg <- "report/stitch-cache/synoptic-delta-gamma"
dir.create(sc_synoptic_dg)
# purrr::walk(spp_vector, function(.sp) {
furrr::future_walk(spp_vector, function(.sp) {
  spp_filename <- paste0(gfsynopsis:::clean_name(.sp), "_", model_type, ".rds")
  stitch_cached_sp <- file.path(sc_synoptic_dg, spp_filename)

  if(any(!file.exists(stitch_cached_sp))) {
    survey_dat <- readRDS(file.path(dc, paste0(gfsynopsis:::clean_name(.sp), ".rds")))$survey_sets |>
      prep_stitch_dat(survey_dat = _, bait_counts = bait_counts)
  }
  get_stitched_index(
    survey_dat = survey_dat, species = .sp, family = sdmTMB::delta_gamma(),
    survey_type = "synoptic", model_type = model_type, cache = sc_synoptic_dg,
    grid_dir = grid_dir, check_cache = TRUE
  )
})

# try delta-gamma with no spatial field
sc_synoptic_dg_sp_off <- "report/stitch-cache/synoptic-delta-gamma-sp-off"
dir.create(sc_synoptic_dg_sp_off, showWarnings = FALSE)
# purrr::walk(spp_vector, function(.sp) {
furrr::future_walk(spp_vector, function(.sp) {
  spp_filename <- paste0(gfsynopsis:::clean_name(.sp), "_", model_type, ".rds")
  stitch_cached_sp <- file.path(sc_synoptic_dg_sp, spp_filename)

  if(any(!file.exists(stitch_cached_sp))) {
    survey_dat <- readRDS(file.path(dc, paste0(gfsynopsis:::clean_name(.sp), ".rds")))$survey_sets |>
      prep_stitch_dat(survey_dat = _, bait_counts = bait_counts)
  }
  get_stitched_index(
    survey_dat = survey_dat, species = .sp, family = sdmTMB::delta_gamma(), spatial = "off",
    survey_type = "synoptic", model_type = model_type, cache = sc_synoptic_dg_sp_off,
    grid_dir = grid_dir, check_cache = TRUE
  )
})

# Stitch IPHC surveys if not cached
furrr::future_walk(spp_vector, function(.sp) {
  # purrr::walk(spp_vector, function(.sp) {
  spp_filename <- paste0(gfsynopsis:::clean_name(.sp), "_", model_type, ".rds")

  if(!file.exists(file.path(sc_iphc, spp_filename))) {
    survey_dat <- readRDS(file.path(dc_iphc, paste0(gfsynopsis:::clean_name(.sp), ".rds")))$set_counts |>
      mutate(species_common_name = .sp) |>
      gfsynopsis::prep_iphc_stitch_dat(survey_dat = _, hook_dat = iphc_hook_counts)

    gfsynopsis::get_iphc_stitched_index(survey_dat = survey_dat, species = .sp,
      form = 'catch ~ 1',
      family = sdmTMB::nbinom2(link = "log"),
      time = 'year',
      spatial = 'on',
      spatiotemporal = 'rw',
      model_type = 'st-rw',
      offset = 'offset',
      gradient_thresh = 0.001,
      cutoff = 20,
      grid = iphc_grid, silent = FALSE,
      cache = sc_iphc,
      check_cache = TRUE)
  }
})
