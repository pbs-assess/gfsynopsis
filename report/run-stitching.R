# to be run as part of `report/make.R`

furrr::future_walk(spp_vector, function(.sp) {
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

  get_stitched_index(
    survey_dat = survey_dat, species = .sp, family = sdmTMB::delta_gamma(),
    survey_type = "synoptic", model_type = model_type, cache = sc_synoptic_dg,
    grid_dir = grid_dir, check_cache = TRUE
  )
})

# work through all individual synoptics:
syns <- c("SYN HS", "SYN QCS", "SYN WCVI", "SYN WCHG")
families <- c("tweedie", "delta-gamma")
furrr::future_walk(spp_vector, function(.sp) {
  purrr::walk(families, function(.family) {
    purrr::walk(syns, function(.syn) {
      if (.family == "tweedie") .fam <- sdmTMB::tweedie()
      if (.family == "delta-gamma") .fam <- sdmTMB::delta_gamma()
      tag <- paste0(.syn, "-", .family)
      .cache <- paste0("report/stitch-cache/synoptic-", tag)
      spp_filename <- paste0(gfsynopsis:::clean_name(.sp), "_", model_type, ".rds")
      stitch_cached_sp <- file.path(.cache, spp_filename)
      if(!file.exists(stitch_cached_sp)) {
        survey_dat <- readRDS(file.path(dc, paste0(gfsynopsis:::clean_name(.sp), ".rds")))$survey_sets |>
          prep_stitch_dat(survey_dat = _, bait_counts = bait_counts) |>
          filter(survey_abbrev == .syn)
      }
      get_stitched_index(
        survey_dat = survey_dat, species = .sp, family = .fam, spatial = .spatial,
        survey_type = .syn, model_type = model_type, cache = .cache,
        grid_dir = grid_dir, check_cache = TRUE, cutoff = 10
      )
    })
  })
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

# Stitch MSSM Survey if not cached
# future::plan(future::multicore, workers = 5)
furrr::future_walk(spp_vector, function(.sp) {
  # purrr::walk(spp_vector, function(.sp) {
  spp_filename <- paste0(gfsynopsis:::clean_name(.sp), "_", model_type, ".rds")

  if(!file.exists(file.path(sc_iphc, spp_filename))) {

    survey_dat <- readRDS(file.path(dc, paste0(gfsynopsis:::clean_name(.sp), ".rds")))$survey_sets |>
      filter(survey_abbrev == "MSSM WCVI")
    # Some species not included in survey_set data frame at all, so we need to skip these
    if (nrow(survey_dat) == 0) {
      out <- "No MSSM survey data"
      message(out)
      saveRDS(out, file.path(sc_mssm, spp_filename))
    } else {
      survey_dat <- prep_mssm_dat(survey_dat)

      get_stitched_index(
        form = 'catch ~ 1',
        survey_dat = survey_dat, species = .sp,
        family = sdmTMB::tweedie(),
        survey_type = "mssm", model_type = 'st-rw', cache = sc_mssm,
        cutoff = 5, silent = FALSE,
        grid_dir = NULL, check_cache = TRUE
      )

      get_stitched_index(
        form = 'catch ~ 1',
        survey_dat = survey_dat, species = .sp,
        family = sdmTMB::delta_gamma(),
        survey_type = "mssm", model_type = 'st-rw', cache = sc_mssm_dg,
        cutoff = 5, silent = FALSE,
        grid_dir = NULL, check_cache = TRUE
      )
    }
  }
})

