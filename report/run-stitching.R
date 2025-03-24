# to be run as part of `report/make.R`

sc_synoptic <- file.path(stitch_cache, "synoptic")
sc_hbll_out <- file.path(stitch_cache, "hbll_outside")
sc_hbll_out_n <- file.path(stitch_cache, "hbll_outside_n")
sc_hbll_out_s <- file.path(stitch_cache, "hbll_outside_s")
sc_hbll_ins <- file.path(stitch_cache, "hbll_inside")
sc_iphc <- file.path(stitch_cache, "iphc")
sc_mssm <- file.path(stitch_cache, 'mssm')

# survey_dat <- readRDS(file.path(dc, paste0(gfsynopsis:::clean_name(.sp), ".rds")))$survey_sets |>
#     prep_stitch_dat(survey_dat = _, bait_counts = bait_counts)

syns <- c("SYN HS", "SYN QCS", "SYN WCVI", "SYN WCHG")
families <- c(
  "tweedie",
  "delta-gamma", "delta-lognormal", "delta-gengamma",
  "delta-lognormal-poisson-link", "delta-gamma-poisson-link",
  "delta-gengamma-poisson-link"
)

# RW stitched regions (and MSSM WCVI)
# -----------------------------------------
furrr::future_walk(spp_vector, function(.sp) {
# purrr::walk(spp_vector, function(.sp) {
  # Load and clean survey data
  survey_dat <- readRDS(file.path(dc, paste0(gfsynopsis:::clean_name(.sp), ".rds")))$survey_sets |>
    prep_stitch_dat(survey_dat = _, bait_counts = bait_counts)

  purrr::walk(families, function(.fam) {
    # Stitched SYN surveys - all regions
    get_stitched_index(
      survey_dat = survey_dat,
      species = .sp,
      survey_type = "synoptic",
      cutoff = 20,
      form = catch ~ 1,
      family = .fam,
      spatial = "on",
      spatiotemporal = "rw",
      use_extra_time = TRUE,
      offset = 'offset',
      cache = sc_synoptic,
      check_cache = TRUE,
      shapefile = shapefile
    )
    #  # MSSM WCVI
    # get_stitched_index(
    #   survey_dat = survey_dat |> filter(survey_abbrev == "MSSM WCVI"),
    #   species = .sp,
    #   survey_type = "MSSM WCVI",
    #   cutoff = 8,
    #   form = catch ~ 1,
    #   family = .fam,
    #   spatial = "on",
    #   spatiotemporal = "iid",
    #   use_extra_time = FALSE,
    #   cache = sc_mssm,
    #   check_cache = TRUE,
    #   shapefile = shapefile
    # )
  })

  # HBLL outside N/S
  # -----------------
  get_stitched_index(
    survey_dat = filter(survey_dat, survey_abbrev %in% c("HBLL OUT N", "HBLL OUT S")),
    survey_type = "hbll_outside",
    species = .sp,
    cutoff = 20,
    form = catch ~ 1,
    family = "nb2",
    spatial = "on",
    spatiotemporal = "rw",
    use_extra_time = TRUE,
    offset = 'offset',
    cache = sc_hbll_out,
    check_cache = TRUE,
    shapefile = shapefile
  )

  # HBLL OUT N st = rw
  get_stitched_index(
    survey_dat = filter(survey_dat, survey_abbrev == "HBLL OUT N"),
    survey_type = "HBLL OUT N",
    species = .sp,
    cutoff = 10,
    form = catch ~ 1,
    family = "nb2",
    spatial = "on",
    spatiotemporal = "rw",
    use_extra_time = TRUE,
    offset = 'offset',
    cache = sc_hbll_out_n,
    check_cache = TRUE,
    shapefile = shapefile
  )

  # HBLL OUT S st = rw
  get_stitched_index(
    survey_dat = filter(survey_dat, survey_abbrev == "HBLL OUT S"),
    survey_type = "HBLL OUT S",
    species = .sp,
    cutoff = 10,
    form = catch ~ 1,
    family = "nb2",
    spatial = "on",
    spatiotemporal = "rw",
    use_extra_time = TRUE,
    offset = 'offset',
    cache = sc_hbll_out_s,
    check_cache = TRUE,
    shapefile = shapefile
  )

  # HBLL OUT N st = iid
  get_stitched_index(
    survey_dat = filter(survey_dat, survey_abbrev == "HBLL OUT N"),
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
  )

  # HBLL OUT S st = iid
  get_stitched_index(
    survey_dat = filter(survey_dat, survey_abbrev == "HBLL OUT S"),
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
  )

  # HBLL inside N/S
  # -----------------
  get_stitched_index(
    survey_dat = filter(survey_dat, survey_abbrev %in% c("HBLL INS N", "HBLL INS S")),
    survey_type = "hbll_inside",
    species = .sp,
    cutoff = 20,
    form = catch ~ 1,
    family = "nb2",
    spatial = "on",
    spatiotemporal = "rw",
    use_extra_time = TRUE,
    offset = 'offset',
    cache = sc_hbll_ins,
    check_cache = TRUE,
    shapefile = shapefile
  )
})

# Fit individual SYN surveys
# -----------------------------------------
tofit <- tidyr::expand_grid(.sp = spp_vector, .syn = syns, .family = families)
# furrr::future_pmap(tofit, function(.sp, .syn, .family) {
purrr::pmap(tofit, function(.sp, .syn, .family) {

  .cache <- file.path(stitch_cache, paste0("synoptic-", .syn))
  spp_filename <- paste0(gfsynopsis:::clean_name(.sp), "_", .family, "_sp-on-st-rw", ".rds")
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
  # RW individual SYN
  get_stitched_index(
    survey_dat = survey_dat,
    species = .sp,
    survey_type = .syn,
    cutoff = .cutoff,
    form = catch ~ 1,
    family = .family,
    spatial = "on",
    spatiotemporal = "rw",
    use_extra_time = TRUE,
    offset = 'offset',
    cache = .cache,
    check_cache = TRUE,
    shapefile = shapefile
  )

  # IID individual SYN
  get_stitched_index(
    survey_dat = survey_dat,
    species = .sp,
    survey_type = .syn,
    cutoff = .cutoff,
    form = catch ~ 1,
    family = .family,
    spatial = "on",
    spatiotemporal = "iid",
    use_extra_time = FALSE,
    offset = 'offset',
    cache = .cache,
    check_cache = TRUE,
    shapefile = shapefile
  )
})

# IPHC FISS
# -----------------------------------------
# furrr::future_walk(spp_vector, function(.sp) {
purrr::walk(spp_vector, function(.sp) {
  survey_dat <- gfdata::load_iphc_dat(species = .sp) |>
    prep_iphc_stitch_dat(survey_dat = _)
  get_stitched_index(
    survey_dat = survey_dat,
    species = .sp,
    survey_type = "IPHC FISS",
    form = catch ~ 1,
    family = "nb2",
    time = 'year',
    spatial = 'on',
    spatiotemporal = 'rw',
    use_extra_time = TRUE,
    offset = 'offset',
    gradient_thresh = 0.001,
    cutoff = 20,
    cache = sc_iphc,
    check_cache = TRUE
  )
})

# ------------------------------------------------------------------------------
# Get best model
# -----------------------------------------

take_min_aic <- function(x) {
  if (nrow(x)) {
    filter(x, aic == min(aic))
  }
}

# Question: I don't know if some should only be comparing iid, or compare it all?
sc_list <- list.files(stitch_cache)

purrr::map_dfr(spp_vector, function(.sp) {
  sp_hyphens <- gfsynopsis::clean_name(spp_vector)
  out_file <- file.path(min_aic_dir, paste0(sp_hyphens, ".rds"))

  ind_df <- purrr::map_dfr(sc_list, function(.sc) {
    cache_files <- list.files(file.path(stitch_cache, .sc), full.names = TRUE)
    index_files <- cache_files[grepl(sp_hyphens, cache_files)]

    if (length(index_files) == 0) {
      message("No indices found for: ", paste0(.sc, "/", sp_hyphens, "*"))
      return(invisible(NULL))
    }

    purrr::map_dfr(index_files, \(i) {
      readRDS(i)
    }) |> take_min_aic()
  })
  saveRDS(ind_df, out_file)
})