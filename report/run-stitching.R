# to be run as part of `report/make.R`

sc_synoptic <- file.path(stitch_cache, "synoptic")
sc_hbll_out <- file.path(stitch_cache, "hbll_outside")
sc_hbll_out_n <- file.path(stitch_cache, "hbll_outside_n")
sc_hbll_out_s <- file.path(stitch_cache, "hbll_outside_s")
sc_hbll_ins <- file.path(stitch_cache, "hbll_inside")
sc_iphc <- file.path(stitch_cache, "iphc")
sc_mssm <- file.path(stitch_cache, "mssm")

syns <- c("SYN HS", "SYN QCS", "SYN WCVI", "SYN WCHG")
families <- c(
  "tweedie",
  "delta-gamma", "delta-lognormal", "delta-gengamma",
  "delta-lognormal-poisson-link", "delta-gamma-poisson-link",
  "delta-gengamma-poisson-link"
)

load_stitch_survey_data <- function(.sp) {
  readRDS(file.path(dc, "survey-sets", paste0(gfsynopsis:::clean_name(.sp), ".rds"))) |>
    prep_stitch_dat(survey_dat = _, bait_counts = bait_counts)
}

load_iphc_stitch_data <- function(.sp) {
  gfdata::load_iphc_dat(species = .sp) |>
    prep_iphc_stitch_dat(survey_dat = _)
}

set_model_fit_threads <- function() {
  if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
    RhpcBLASctl::blas_set_num_threads(1)
    RhpcBLASctl::omp_set_num_threads(1)
  }
}

stitch_opts <- furrr::furrr_options(seed = NULL, scheduling = Inf)

fit_rw_stitched_job <- function(.sp, .family, .job) {
  set_model_fit_threads()
  survey_dat <- load_stitch_survey_data(.sp)

  if (.job == "synoptic") {
    return(get_stitched_index(
      survey_dat = survey_dat,
      species = .sp,
      survey_type = "synoptic",
      cutoff = 20,
      form = catch ~ 1,
      family = .family,
      spatial = "on",
      spatiotemporal = "rw",
      use_extra_time = TRUE,
      offset = "offset",
      cache = sc_synoptic,
      check_cache = TRUE,
      shapefile = shapefile
    ))
  }

  if (.job == "mssm") {
    return(get_stitched_index(
      survey_dat = survey_dat |> dplyr::filter(survey_abbrev == "MSSM WCVI"),
      species = .sp,
      survey_type = "MSSM WCVI",
      cutoff = 8,
      form = catch ~ 1,
      family = .family,
      spatial = "on",
      spatiotemporal = "rw",
      use_extra_time = FALSE,
      cache = sc_mssm,
      check_cache = TRUE,
      shapefile = shapefile
    ))
  }

  stop("Unknown RW stitched job: ", .job)
}

fit_single_region_job <- function(.sp, .job) {
  set_model_fit_threads()
  survey_dat <- load_stitch_survey_data(.sp)

  if (.job == "hbll_outside_rw") {
    return(get_stitched_index(
      survey_dat = dplyr::filter(survey_dat, survey_abbrev %in% c("HBLL OUT N", "HBLL OUT S")),
      survey_type = "hbll_outside",
      species = .sp,
      cutoff = 20,
      form = catch ~ 1,
      family = "nb2",
      spatial = "on",
      spatiotemporal = "rw",
      use_extra_time = TRUE,
      offset = "offset",
      cache = sc_hbll_out,
      check_cache = TRUE,
      shapefile = shapefile
    ))
  }

  if (.job == "hbll_out_n_rw") {
    return(get_stitched_index(
      survey_dat = dplyr::filter(survey_dat, survey_abbrev == "HBLL OUT N"),
      survey_type = "HBLL OUT N",
      species = .sp,
      cutoff = 10,
      form = catch ~ 1,
      family = "nb2",
      spatial = "on",
      spatiotemporal = "rw",
      use_extra_time = TRUE,
      offset = "offset",
      cache = sc_hbll_out_n,
      check_cache = TRUE,
      shapefile = shapefile
    ))
  }

  if (.job == "hbll_out_s_rw") {
    return(get_stitched_index(
      survey_dat = dplyr::filter(survey_dat, survey_abbrev == "HBLL OUT S"),
      survey_type = "HBLL OUT S",
      species = .sp,
      cutoff = 10,
      form = catch ~ 1,
      family = "nb2",
      spatial = "on",
      spatiotemporal = "rw",
      use_extra_time = TRUE,
      offset = "offset",
      cache = sc_hbll_out_s,
      check_cache = TRUE,
      shapefile = shapefile
    ))
  }

  if (.job == "hbll_out_n_iid") {
    return(get_stitched_index(
      survey_dat = dplyr::filter(survey_dat, survey_abbrev == "HBLL OUT N"),
      survey_type = "HBLL OUT N",
      species = .sp,
      cutoff = 10,
      form = catch ~ 0 + as.factor(year),
      family = "nb2",
      spatial = "on",
      spatiotemporal = "iid",
      use_extra_time = FALSE,
      cache = sc_hbll_out_n,
      check_cache = TRUE,
      shapefile = shapefile
    ))
  }

  if (.job == "hbll_out_s_iid") {
    return(get_stitched_index(
      survey_dat = dplyr::filter(survey_dat, survey_abbrev == "HBLL OUT S"),
      survey_type = "HBLL OUT S",
      species = .sp,
      cutoff = 10,
      form = catch ~ 0 + as.factor(year),
      family = "nb2",
      spatial = "on",
      spatiotemporal = "iid",
      use_extra_time = FALSE,
      cache = sc_hbll_out_s,
      check_cache = TRUE,
      shapefile = shapefile
    ))
  }

  if (.job == "hbll_inside_rw") {
    return(get_stitched_index(
      survey_dat = dplyr::filter(survey_dat, survey_abbrev %in% c("HBLL INS N", "HBLL INS S")),
      survey_type = "hbll_inside",
      species = .sp,
      cutoff = 20,
      form = catch ~ 1,
      family = "nb2",
      spatial = "on",
      spatiotemporal = "rw",
      use_extra_time = TRUE,
      offset = "offset",
      cache = sc_hbll_ins,
      check_cache = TRUE,
      shapefile = shapefile
    ))
  }

  if (.job == "iphc_rw") {
    return(get_stitched_index(
      survey_dat = load_iphc_stitch_data(.sp),
      species = .sp,
      survey_type = "IPHC FISS",
      form = catch ~ 1,
      family = "nb2",
      time = "year",
      spatial = "on",
      spatiotemporal = "rw",
      use_extra_time = TRUE,
      offset = "offset",
      gradient_thresh = 0.001,
      cutoff = 20,
      cache = sc_iphc,
      check_cache = TRUE,
      shapefile = shapefile
    ))
  }

  stop("Unknown single-region job: ", .job)
}

fit_synoptic_single_region_job <- function(.sp, .syn, .family) {
  set_model_fit_threads()
  .cache <- file.path(stitch_cache, paste0("synoptic-", .syn))
  spp_filename <- paste0(gfsynopsis:::clean_name(.sp), "_", .family, "_sp-on-st-iid", ".rds")
  stitch_cached_sp <- file.path(.cache, spp_filename)

  if (file.exists(stitch_cached_sp)) {
    return(invisible(NULL))
  }

  survey_dat <- load_stitch_survey_data(.sp) |>
    dplyr::filter(survey_abbrev == .syn)

  if (.syn == "SYN WCHG") {
    survey_dat <- dplyr::filter(survey_dat, year != 2014)
    .cutoff <- 8
  } else {
    .cutoff <- 10
  }

  get_stitched_index(
    survey_dat = survey_dat,
    species = .sp,
    survey_type = .syn,
    cutoff = .cutoff,
    form = catch ~ 0 + as.factor(year),
    family = .family,
    spatial = "on",
    spatiotemporal = "iid",
    use_extra_time = FALSE,
    offset = "offset",
    cache = .cache,
    check_cache = TRUE,
    shapefile = shapefile
  )
}

# RW stitched regions (and MSSM WCVI)
# -----------------------------------------
rw_jobs <- tidyr::expand_grid(
  .sp = spp_vector,
  .family = families,
  .job = c("synoptic", "mssm")
)
furrr::future_pwalk(
  rw_jobs,
  fit_rw_stitched_job,
  .options = stitch_opts
)

# HBLL / IPHC stitched jobs
# -----------------------------------------
single_region_jobs <- tidyr::expand_grid(
  .sp = spp_vector,
  .job = c(
    "hbll_outside_rw",
    "hbll_out_n_rw",
    "hbll_out_s_rw",
    "hbll_out_n_iid",
    "hbll_out_s_iid",
    "hbll_inside_rw",
    "iphc_rw"
  )
)
furrr::future_pwalk(
  single_region_jobs,
  fit_single_region_job,
  .options = stitch_opts
)

# Fit individual SYN surveys
# -----------------------------------------
tofit <- tidyr::expand_grid(.sp = spp_vector, .syn = syns, .family = families)
furrr::future_pwalk(
  tofit,
  fit_synoptic_single_region_job,
  .options = stitch_opts
)

# ------------------------------------------------------------------------------
# Get best model
# -----------------------------------------

take_min_aic <- function(x) {
  if (nrow(x)) {
    dplyr::filter(x, aic == min(aic))
  }
}

min_aic_dir <- file.path(stitch_cache, "min_aic")
dir.create(min_aic_dir, showWarnings = FALSE, recursive = TRUE)
sc_list <- list.files(stitch_cache)
sc_list <- sc_list[!grepl("min_aic", sc_list)]
single_regions <- c("hbll_outside_n", "hbll_outside_s",
  paste0("synoptic-", syns)
)
purrr::map_dfr(spp_vector, function(.sp) {
  sp_hyphens <- gfsynopsis::clean_name(.sp)
  out_file <- file.path(min_aic_dir, paste0(sp_hyphens, ".rds"))

  ind_df <- purrr::map_dfr(sc_list, function(.sc) {
    cache_files <- list.files(file.path(stitch_cache, .sc), full.names = TRUE)
    index_files <- cache_files[grepl(sp_hyphens, cache_files)]

    if (.sc %in% single_regions) {
      index_files <- index_files[grepl("iid", index_files)]
    } else {
      index_files <- index_files[grepl("rw", index_files)]
    }

    if (length(index_files) == 0) {
      message("No indices found for: ", paste0(.sc, "/", sp_hyphens, "*"))
      return(invisible(NULL))
    }

    purrr::map(index_files, readRDS) |>
      purrr::keep(is.data.frame) |>
      purrr::list_rbind() |>
      take_min_aic()
  })
  saveRDS(ind_df, out_file)
})
