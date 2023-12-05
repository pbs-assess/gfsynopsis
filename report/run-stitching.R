# to be run as part of `report/make.R`

sc_synoptic <- file.path(stitch_cache, "synoptic-tweedie")
sc_synoptic_dg <- file.path(stitch_cache, "synoptic-delta-gamma")
sc_synoptic_dl <- file.path(stitch_cache, "synoptic-delta-lognormal")
sc_synoptic_dpg <- file.path(stitch_cache, "synoptic-delta-poisson-link-gamma")
sc_synoptic_dpl <- file.path(stitch_cache, "synoptic-delta-poisson-link-lognormal")

sc_hbll_out <- file.path(stitch_cache, "hbll_outside")
sc_hbll_out_n <- file.path(stitch_cache, "hbll_outside_n")
sc_hbll_out_s <- file.path(stitch_cache, "hbll_outside_s")
sc_hbll_ins <- file.path(stitch_cache, "hbll_inside")
sc_iphc <- file.path(stitch_cache, "iphc")
sc_mssm <- file.path(stitch_cache, 'mssm-tweedie')
sc_mssm_dg <- file.path(stitch_cache, 'mssm-delta-gamma')
sc_mssm_dl <- file.path(stitch_cache, 'mssm-lognormal')
sc_mssm_dpg <- file.path(stitch_cache, 'mssm-delta-poisson-link-gamma')
sc_mssm_dpl <- file.path(stitch_cache, 'mssm-delta-poisson-link-lognormal')

dir.create(sc_synoptic, showWarnings = FALSE, recursive = TRUE)
dir.create(sc_synoptic_dg, showWarnings = FALSE, recursive = TRUE)
dir.create(sc_synoptic_dl, showWarnings = FALSE, recursive = TRUE)
dir.create(sc_synoptic_dpg, showWarnings = FALSE, recursive = TRUE)
dir.create(sc_synoptic_dpl, showWarnings = FALSE, recursive = TRUE)

dir.create(sc_hbll_out, showWarnings = FALSE, recursive = TRUE)
dir.create(sc_hbll_out_n, showWarnings = FALSE, recursive = TRUE)
dir.create(sc_hbll_out_s, showWarnings = FALSE, recursive = TRUE)
dir.create(sc_hbll_ins, showWarnings = FALSE, recursive = TRUE)
dir.create(sc_iphc, showWarnings = FALSE, recursive = TRUE)
dir.create(sc_mssm, showWarnings = FALSE, recursive = TRUE)
dir.create(sc_mssm_dg, showWarnings = FALSE, recursive = TRUE)
dir.create(sc_mssm_dl, showWarnings = FALSE, recursive = TRUE)
dir.create(sc_mssm_dpg, showWarnings = FALSE, recursive = TRUE)
dir.create(sc_mssm_dpl, showWarnings = FALSE, recursive = TRUE)

model_type_iid <- "st-iid"

furrr::future_walk(spp_vector, function(.sp) {
  spp_filename <- paste0(gfsynopsis:::clean_name(.sp), "_", model_type, ".rds")
  stitch_cached_sp <- file.path(c(
    sc_synoptic,
    sc_hbll_out,
    sc_hbll_ins,
    sc_hbll_out_n,
    sc_hbll_out_s,
    sc_synoptic_dg,
    sc_synoptic_dl,
    sc_synoptic_dpg,
    sc_synoptic_dpl
  ),
    spp_filename)

  # if(any(!file.exists(stitch_cached_sp))) {
    survey_dat <- readRDS(file.path(dc, paste0(gfsynopsis:::clean_name(.sp), ".rds")))$survey_sets |>
      prep_stitch_dat(survey_dat = _, bait_counts = bait_counts)
  # }

  get_stitched_index(
    survey_dat = survey_dat, species = .sp,
    survey_type = "synoptic", model_type = model_type, cache = sc_synoptic,
    grid_dir = grid_dir, check_cache = TRUE
  )

  get_stitched_index(
    survey_dat = filter(survey_dat, survey_abbrev == "HBLL OUT S"), species = .sp,
    cutoff = 10,
    survey_type = "HBLL OUT S", model_type = model_type, cache = sc_hbll_out_s,
    family = sdmTMB::nbinom2(link = "log"), grid_dir = grid_dir,
    check_cache = TRUE
  )

  get_stitched_index(
    survey_dat = filter(survey_dat, survey_abbrev == "HBLL OUT N"), species = .sp,
    cutoff = 10,
    survey_type = "HBLL OUT N", model_type = model_type, cache = sc_hbll_out_n,
    family = sdmTMB::nbinom2(link = "log"), grid_dir = grid_dir,
    check_cache = TRUE
  )

  # IID
  get_stitched_index(
    survey_dat = filter(survey_dat, survey_abbrev == "HBLL OUT S"), species = .sp,
    cutoff = 10,
    # priors = sdmTMB::sdmTMBpriors(
    #   matern_s = pc_matern(range_gt = 10, sigma_lt = 5),
    #   matern_st = pc_matern(range_gt = 10, sigma_lt = 2)
    # ),
    survey_type = "HBLL OUT S", model_type = model_type_iid, cache = sc_hbll_out_s,
    family = sdmTMB::nbinom2(link = "log"), grid_dir = grid_dir,
    spatiotemporal = "iid", spatial = "on",
    form = catch ~ 0 + as.factor(year),
    check_cache = TRUE
  )

  # IID
  get_stitched_index(
    survey_dat = filter(survey_dat, survey_abbrev == "HBLL OUT N"), species = .sp,
    cutoff = 10,
    # priors = sdmTMB::sdmTMBpriors(
    #   matern_s = pc_matern(range_gt = 10, sigma_lt = 5),
    #   matern_st = pc_matern(range_gt = 10, sigma_lt = 2)
    # ),
    survey_type = "HBLL OUT N", model_type = model_type_iid, cache = sc_hbll_out_n,
    family = sdmTMB::nbinom2(link = "log"), grid_dir = grid_dir,
    spatiotemporal = "iid", spatial = "on",
    form = catch ~ 0 + as.factor(year),
    check_cache = TRUE
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

  get_stitched_index(
    survey_dat = survey_dat, species = .sp, family = sdmTMB::delta_lognormal(),
    survey_type = "synoptic", model_type = model_type, cache = sc_synoptic_dl,
    grid_dir = grid_dir, check_cache = TRUE
  )

  get_stitched_index(
    survey_dat = survey_dat, species = .sp, family = sdmTMB::delta_poisson_link_gamma(),
    survey_type = "synoptic", model_type = model_type, cache = sc_synoptic_dpg,
    grid_dir = grid_dir, check_cache = TRUE
  )

  get_stitched_index(
    survey_dat = survey_dat, species = .sp, family = sdmTMB::delta_poisson_link_lognormal(),
    survey_type = "synoptic", model_type = model_type, cache = sc_synoptic_dpl,
    grid_dir = grid_dir, check_cache = TRUE
  )
})

# work through all individual synoptics:
syns <- c("SYN HS", "SYN QCS", "SYN WCVI", "SYN WCHG")
families <- c("tweedie", "delta-gamma", "delta-lognormal", "delta-poisson-link-lognormal", "delta-poisson-link-gamma")

tofit <- tidyr::expand_grid(.sp = spp_vector, .syn = syns, .family = families)
# furrr::future_walk(spp_vector, function(.sp) {
#   purrr::walk(families, function(.family) {
#     purrr::walk(syns, function(.syn) {
furrr::future_pmap(tofit, function(.sp, .syn, .family) {
# purrr::pmap(tofit, function(.sp, .syn, .family) {
  if (.family == "tweedie") .fam <- sdmTMB::tweedie()
  if (.family == "delta-gamma") .fam <- sdmTMB::delta_gamma()
  if (.family == "delta-lognormal") .fam <- sdmTMB::delta_lognormal()
  if (.family == "delta-poisson-link-lognormal") .fam <- sdmTMB::delta_poisson_link_lognormal()
  if (.family == "delta-poisson-link-gamma") .fam <- sdmTMB::delta_poisson_link_gamma()
  tag <- paste0(.syn, "-", .family)
  .cache <- paste0("report/stitch-cache/synoptic-", tag)
  spp_filename <- paste0(gfsynopsis:::clean_name(.sp), "_", model_type, ".rds")
  stitch_cached_sp <- file.path(.cache, spp_filename)
  if(!file.exists(stitch_cached_sp)) {
    survey_dat <- readRDS(file.path(dc, paste0(gfsynopsis:::clean_name(.sp), ".rds")))$survey_sets |>
      prep_stitch_dat(survey_dat = _, bait_counts = bait_counts) |>
      filter(survey_abbrev == .syn)
    if (.syn == "SYN WCHG") {
      survey_dat <- filter(survey_dat, year != 2014) # partial year
      .cutoff <- 8
    } else {
      .cutoff <- 10
    }
  }
  get_stitched_index(
    survey_dat = survey_dat, species = .sp, family = .fam,
    survey_type = .syn, model_type = model_type, cache = .cache,
    grid_dir = grid_dir, check_cache = TRUE, cutoff = .cutoff
  )
})
# })
# })

# IID versions
furrr::future_pmap(tofit, function(.sp, .syn, .family) {
# purrr::pmap(tofit[786,,drop=FALSE], function(.sp, .syn, .family) {
  if (.family == "tweedie") .fam <- sdmTMB::tweedie()
  if (.family == "delta-gamma") .fam <- sdmTMB::delta_gamma()
  if (.family == "delta-lognormal") .fam <- sdmTMB::delta_lognormal()
  if (.family == "delta-poisson-link-lognormal") .fam <- sdmTMB::delta_poisson_link_lognormal()
  if (.family == "delta-poisson-link-gamma") .fam <- sdmTMB::delta_poisson_link_gamma()
  tag <- paste0(.syn, "-", .family)
  .cache <- paste0("report/stitch-cache/synoptic-", tag)
  spp_filename <- paste0(gfsynopsis:::clean_name(.sp), "_", model_type_iid, ".rds")
  stitch_cached_sp <- file.path(.cache, spp_filename)
  # if(!file.exists(stitch_cached_sp)) {
    survey_dat <- readRDS(file.path(dc, paste0(gfsynopsis:::clean_name(.sp), ".rds")))$survey_sets |>
      prep_stitch_dat(survey_dat = _, bait_counts = bait_counts) |>
      filter(survey_abbrev == .syn)
    if (.syn == "SYN WCHG") {
      survey_dat <- filter(survey_dat, year != 2014) # partial year
      .cutoff <- 8
    } else {
      .cutoff <- 10
    }
  # }
  get_stitched_index(
    survey_dat = survey_dat, species = .sp, family = .fam,
    survey_type = .syn, model_type = model_type_iid, cache = .cache,
    spatiotemporal = "iid", spatial = "on",
    form = catch ~ 0 + as.factor(year),
    # priors = sdmTMB::sdmTMBpriors(
    #   matern_s = pc_matern(range_gt = 10, sigma_lt = 5),
    #   matern_st = pc_matern(range_gt = 10, sigma_lt = 2)
    # ),
    grid_dir = grid_dir, check_cache = TRUE, cutoff = .cutoff, silent = FALSE
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

# Stitch MSSM Survey if not cached
# future::plan(future::multicore, workers = 5)
furrr::future_walk(spp_vector, function(.sp) {
  # purrr::walk(spp_vector, function(.sp) {
  spp_filename <- paste0(gfsynopsis:::clean_name(.sp), "_", model_type, ".rds")

  if (
    !file.exists(file.path(sc_mssm, spp_filename)) ||
      !file.exists(file.path(sc_mssm_dg, spp_filename)) ||
      !file.exists(file.path(sc_mssm_dl, spp_filename)) ||
      !file.exists(file.path(sc_mssm_dpl, spp_filename)) ||
      !file.exists(file.path(sc_mssm_dpg, spp_filename))
  ) {

    survey_dat <- readRDS(file.path(dc, paste0(gfsynopsis::clean_name(.sp), ".rds")))$survey_sets |>
      filter(survey_abbrev == "MSSM WCVI")
    # Some species not included in survey_set data frame at all, so we need to skip these
    if (nrow(survey_dat) == 0) {
      out <- "No MSSM survey data"
      message(out)
      saveRDS(out, file.path(sc_mssm, spp_filename))
    } else {
      survey_dat <- prep_mssm_dat(survey_dat)

      CUTOFF <- 8
      get_stitched_index(
        form = 'catch ~ 1',
        survey_dat = survey_dat, species = .sp,
        family = sdmTMB::tweedie(),
        survey_type = "mssm", model_type = 'st-rw', cache = sc_mssm,
        cutoff = CUTOFF, silent = FALSE,
        grid_dir = NULL, check_cache = TRUE
      )

      get_stitched_index(
        form = 'catch ~ 1',
        survey_dat = survey_dat, species = .sp,
        family = sdmTMB::delta_gamma(),
        survey_type = "mssm", model_type = 'st-rw', cache = sc_mssm_dg,
        cutoff = CUTOFF, silent = FALSE,
        grid_dir = NULL, check_cache = TRUE
      )

      get_stitched_index(
        form = 'catch ~ 1',
        survey_dat = survey_dat, species = .sp,
        family = sdmTMB::delta_lognormal(),
        survey_type = "mssm", model_type = 'st-rw', cache = sc_mssm_dl,
        cutoff = CUTOFF, silent = FALSE,
        grid_dir = NULL, check_cache = TRUE
      )

      get_stitched_index(
        form = 'catch ~ 1',
        survey_dat = survey_dat, species = .sp,
        family = sdmTMB::delta_poisson_link_gamma(),
        survey_type = "mssm", model_type = 'st-rw', cache = sc_mssm_dpg,
        cutoff = CUTOFF, silent = FALSE,
        grid_dir = NULL, check_cache = TRUE
      )

      get_stitched_index(
        form = 'catch ~ 1',
        survey_dat = survey_dat, species = .sp,
        family = sdmTMB::delta_poisson_link_lognormal(),
        survey_type = "mssm", model_type = 'st-rw', cache = sc_mssm_dpl,
        cutoff = CUTOFF, silent = FALSE,
        grid_dir = NULL, check_cache = TRUE
      )


    }
  }
})

# fit SYN WCVI but predict on MSSM grid
# first we need to grab the families from when they were fit before to save time
get_index <- function(folder, spp, .family = "", model_tag = "st-rw") {
  paths <- list.files(folder, pattern = ".rds", full.names = TRUE)
  path <- paths[grepl(spp, paths)]
  if (length(path) > 1) {
    path <- path[grepl(model_tag, path)]
  }
  if (length(path)) {
    file_names <- list.files(folder, pattern = ".rds")
    sp <- gsub("-", " ", spp)
    if (file.exists(path)) {
      d <- readRDS(path)
      if (length(d) > 1L) {
        return(dplyr::mutate(d, species = sp, family = .family))
      }
    }
  }
}
families <- c(
  "delta-gamma",
  "delta-poisson-link-gamma",
  "tweedie",
  "delta-poisson-link-lognormal",
  "delta-lognormal"
)
take_min_aic <- function(x) {
  if (nrow(x)) {
    filter(x, aic == min(aic))
  }
}
syn_wcvi <- tidyr::expand_grid(.s = gfsynopsis::clean_name(spp_vector), f = families) |>
  purrr::pmap_dfr(function(.s, f) {
  get_index(paste0("report/stitch-cache/synoptic-SYN WCVI-", f, "/"), .s, .family = f)
}) |> group_by(species) |>
  take_min_aic()

# now fit just the best family but predict on MSSM grid:
syn_wcvi |>
  select(.sp = species, .family = family) |>
  distinct() |>
  # purrr::pmap(\(.sp, .family) {
  furrr::future_pmap(\(.sp, .family) {
    if (.family == "tweedie") .fam <- sdmTMB::tweedie()
    if (.family == "delta-gamma") .fam <- sdmTMB::delta_gamma()
    if (.family == "delta-lognormal") .fam <- sdmTMB::delta_lognormal()
    if (.family == "delta-poisson-link-lognormal") .fam <- sdmTMB::delta_poisson_link_lognormal()
    if (.family == "delta-poisson-link-gamma") .fam <- sdmTMB::delta_poisson_link_gamma()
    .syn <- "SYN WCVI"
    tag <- paste0(.syn, "-", .family)
    .cache <- paste0("report/stitch-cache/synoptic-mssm-", tag)
    survey_dat <- readRDS(file.path(dc, paste0(gfsynopsis:::clean_name(.sp), ".rds")))$survey_sets |>
      prep_stitch_dat(survey_dat = _, bait_counts = bait_counts) |>
      filter(survey_abbrev == .syn)
    .cutoff <- 10
    mssm_grid <- gfdata::mssm_grid |>
      mutate(survey = "SYN WCVI") |>
      filter(year >= 2009 & year < 2022) |>
      distinct(X, Y, survey, area)

    get_stitched_index(
      survey_dat = survey_dat, species = .sp, family = .fam,
      survey_type = .syn, model_type = model_type, cache = .cache,
      check_cache = TRUE, cutoff = .cutoff, survey_grid = mssm_grid
    )
  })
