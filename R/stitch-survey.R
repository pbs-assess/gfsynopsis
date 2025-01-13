#' Prepare survey set data for index stitching
#'
#' @param survey_dat A dataframe from `[gfplot::get_survey_sets()]`
#' @param bait_counts A dataframe from [gfsynopsis::get_ll_bait_counts()]
#'
#' @returns A dataframe the same length as `survey_dat`
#'
#' @export
prep_stitch_dat <- function(survey_dat, bait_counts) {
  if ("survey_series_id.x" %in% names(survey_dat)) {
    colnames(survey_dat)[colnames(survey_dat) == "survey_series_id.x"] <- "survey_series_id"
  }
  # Add baited hook counts to survey_dat for LL surveys
  # @FIXME this chunk is probably unecessary if all surveys are in survey_dat
  ll <- grepl("HBLL", unique(survey_dat$survey_abbrev))
  if (sum(ll) > 0) {
    survey_dat <- dplyr::left_join(survey_dat, bait_counts,
      by = c("year", "fishing_event_id", "survey_series_id" = "ssid")
    ) |>
      dplyr::mutate(count_bait_only = replace(count_bait_only, which(count_bait_only == 0), 1)) |>
      dplyr::mutate(prop_bait_hooks = count_bait_only / hook_count) |>
      dplyr::mutate(
        hook_adjust_factor = -log(prop_bait_hooks) / (1 - prop_bait_hooks),
        prop_removed = 1 - prop_bait_hooks
      )
  }
  out <-
    survey_dat |>
    sdmTMB::add_utm_columns(c("longitude", "latitude"), utm_crs = 32609) |>
    # @FIXME: area swept has been or will be added to gfdata function
    dplyr::mutate(
      area_swept1 = doorspread_m * (speed_mpm * duration_min),
      area_swept2 = tow_length_m * doorspread_m,
      area_swept = dplyr::case_when(
        grepl("SYN", survey_abbrev) & !is.na(area_swept2) ~ area_swept2,
        grepl("SYN", survey_abbrev) & is.na(area_swept2) ~ area_swept1,
        grepl("HBLL", survey_abbrev) ~ hook_count * 0.0024384 * 0.009144 * 1000
      )
    ) |>
    dplyr::mutate(hook_adjust_factor = ifelse(
      grepl("SYN", survey_abbrev), NA, hook_adjust_factor)) |>
    dplyr::mutate(offset = dplyr::case_when(
      grepl("SYN", survey_abbrev) ~ log(area_swept / 1e5),
      grepl("HBLL", survey_abbrev) ~ log(hook_count / hook_adjust_factor)
    )) |>
    dplyr::mutate(catch = ifelse(grepl("SYN", survey_abbrev), catch_weight, catch_count)) |>
    dplyr::mutate(present = ifelse(catch > 0, 1, 0)) |>
    dplyr::mutate(survey_type = dplyr::case_when(
      grepl("SYN", survey_abbrev) ~ "synoptic",
      grepl("HBLL OUT", survey_abbrev) ~ "hbll_outside",
      grepl("HBLL INS", survey_abbrev) ~ "hbll_inside"
    )) |>
    dplyr::mutate(log_hook_count = log(hook_count)) |>
    dplyr::mutate(fyear = as.factor(year)) |>
    dplyr::mutate(prop_removed = ifelse(grepl("HBLL", survey_abbrev), 1 - prop_bait_hooks, NA)) |>
    dplyr::filter(!is.na(offset), is.finite(offset)) |>
    # 150 remove duplicated fishing event
    # @FIXME shouldn't need this after switch to trials version of gfdata)
    dplyr::distinct(species_common_name, fishing_event_id, .keep_all = TRUE)
}

#' Prepare Multispecies Small Mesh (MSSM) survey set data for index stitching
#'
#' @param survey_dat A dataframe from `[gfplot::get_survey_sets()]`
#'
#' @returns A dataframe the same length as `survey_dat`
#'
#' @export
prep_mssm_dat <- function(survey_dat) {
  # Some doorspreads were missing in 2022, use the average of other 2022 doorspreads
  mean_2022_door <- filter(survey_dat, year == 2022, doorspread_m != 0) |>
    filter(species_code == unique(survey_dat$species_code)) |>
    summarise(mean_door = mean(doorspread_m))

  survey_dat |>
    # This might be needed as gfdata gets updated, but currently grouping_desc
    # is not retrieved in data call used for 2023 report.
    #filter(grepl('WCVI Shrimp Survey Area 124|125', grouping_desc)) |>
    sdmTMB::add_utm_columns(c("longitude", "latitude"), utm_crs = 32609) |>
    mutate(doorspread_m = ifelse((year == 2022 & doorspread_m == 0), mean_2022_door[[1]], doorspread_m)) |>
    # @FIXME: area swept has been or will be added to gfdata function
    dplyr::mutate(
      area_swept1 = doorspread_m * (speed_mpm * duration_min),
      area_swept2 = tow_length_m * doorspread_m,
      area_swept = dplyr::case_when(
        !is.na(area_swept2) ~ area_swept2,
         is.na(area_swept2) ~ area_swept1)
    ) |>
    dplyr::mutate(offset = log(area_swept / 1e5),
                  catch = catch_weight,
                  present = ifelse(catch > 0, 1, 0),
                  survey_type = 'mssm',
                  year_bin = ifelse(year >= 2003, ">=2003", "<2003"), # All species ID'd to lowest taxonomic level in 2003 onward
                  fyear = as.factor(year)) |>
    dplyr::mutate(year_bin = factor(year_bin, levels = c("<2003", ">=2003"))) |>
    dplyr::filter(!is.na(offset), is.finite(offset)) |>
    dplyr::filter(!is.na(catch)) |> # small catches of small fish might have a count but no weight
    dplyr::filter(!(year %in% 1977:1978 & month == 9)) |> # Filter out the extra sampling in September
    dplyr::mutate(gear = dplyr::case_when( # add gear type change
      year < 1977 ~ 'Shrimp Balloon',
      year < 2006 & year > 1977 ~ 'NMFS',
      year > 2006 ~ 'American',
      year == 2006 & fishing_event_id %in% c(1158541, 1158542) ~ 'American',
      year == 2006 & fishing_event_id %in% c(1158559, 1158560) ~ 'NMFS',
      year == 2006 & !(fishing_event_id %in% c(1158541, 1158542, 1158559, 1158560)) ~ 'American'
    )
  )
}


#' Get table of positive sets for each region and survey type
#'
#' @param survey_dat A dataframe from [gfsynopsis::prep_stitch_dat()]
#' @param species A string specifying the `species_common_name`
#' @param survey_type A string matching one of: "synoptic", "hbll_outside", "hbll_inside"
#' @param survey_col The name of the column to match `survey_type` in, one of:
#'   "survey_abbrev", 'survey_type' (default = 'survey_abbrev')
#'
#' @returns A dataframe
#' @export
get_stitch_lu <- function(survey_dat, species, survey_type, survey_col = 'survey_type') {
  # stopifnot(survey_type %in% c("synoptic", "mssm", "hbll_outside", "hbll_inside",
  #   "SYN WCVI"))
  if (survey_type %in% c('SYN WCVI', 'SYN WCHG', 'SYN HS', 'SYN QCS', "HBLL OUT N", "HBLL OUT S")) survey_col = 'survey_abbrev'
  survey_dat |>
    dplyr::filter(species_common_name %in% {{ species }}, .data[[survey_col]] %in% {{ survey_type }}) |>
    dplyr::group_by(species_common_name, survey_type, survey_abbrev, year) |>
    dplyr::add_count(name = "n_sets") |>
    dplyr::add_tally(present, name = "n_pos") |>
    dplyr::distinct(species_common_name, year, survey_type, survey_abbrev, n_pos, n_sets) |>
    dplyr::group_by(species_common_name, survey_type, survey_abbrev) |>
    dplyr::summarise(
      mean_n_pos = mean(n_pos), mean_n_sets = mean(n_sets),
      prop_pos = mean_n_pos / mean_n_sets,
      .groups = "drop"
    ) |>
    dplyr::mutate_at(c("mean_n_pos", "mean_n_sets"), round, 0) |>
    dplyr::mutate_at("prop_pos", round, 2) |>
    dplyr::mutate(include_in_stitch = ifelse(prop_pos < 0.05, 0, 1)) |>
    dplyr::arrange(survey_type, species_common_name)
}

# Prepare grids ----------------------------------------------------------------
#' Write grids used for stitching index
#'
#' @description
#' Write grid objects used for stitching index. These are written to a
#' <data-outputs/grids> directory.
#' Will not be needed if clean grids are added to `gfdata`
#' @param grid_dir Path where cleaned grids should be stored
#' @param hbll_ins_grid_input RDS object containing the HBLL INS grid with water
#'   area values.
#'
#' @returns Synoptic, HBLL OUT, and HBLL INS grids as .rds files.
#'
#' @export
#'
prep_stitch_grids <- function(grid_dir, hbll_ins_grid_input) {
  dir.create(grid_dir, showWarnings = FALSE, recursive = TRUE)

  synoptic_grid_file <- file.path(grid_dir, "synoptic_grid.rds")
  hbll_out_grid_file <- file.path(grid_dir, "hbll_out_grid.rds")
  hbll_ins_grid_file <- file.path(grid_dir, "hbll_ins_grid.rds")
  # @TODO will update when grids are added to gfdata
  if (!file.exists(synoptic_grid_file)) {
    syn_grid <-
      gfplot::synoptic_grid |>
      dplyr::rename(area = "cell_area") |>
      dplyr::select(-survey_series_name, -utm_zone, -survey_domain_year)
    saveRDS(syn_grid, synoptic_grid_file)
  }

  if (!file.exists(hbll_out_grid_file)) {
    hbll_n_grid <- gfplot::hbll_n_grid$grid |>
      dplyr::mutate(survey = "HBLL OUT N")
    hbll_s_grid <- gfplot::hbll_s_grid$grid |>
      dplyr::mutate(survey = "HBLL OUT S")

    hbll_out_grid <- dplyr::bind_rows(hbll_n_grid, hbll_s_grid) |>
      dplyr::rename(longitude = "X", latitude = "Y") |>
      sdmTMB::add_utm_columns(c("longitude", "latitude"), utm_crs = 32609) |>
      dplyr::mutate(area = 4) |>
      dplyr::select(survey, X, Y, depth, area)
    saveRDS(hbll_out_grid, hbll_out_grid_file)
  }

  if (!file.exists(hbll_ins_grid_file)) {
    hbll_ins_grid <-
      readRDS(hbll_ins_grid_input) |>
      sdmTMB::add_utm_columns(c("longitude", "latitude"), utm_crs = 32609) |>
      dplyr::select(survey, X, Y, depth, area)
    saveRDS(hbll_ins_grid, hbll_ins_grid_file)
  }
}

# Utility functions ------------------------------------------------------------
#' Choose the survey grid matching the survey type. For the MSSM survey, the grid
#' is filtered to include only cells that were last sampled from 2009 to 2022
#' (year grid was last updated)
#'
#' TODO: update with calls to gfdata once updated grid functions are added there.
#'
#' @param survey_type A string matching one of: "synoptic", "hbll_outside", "hbll_inside", "mssm"
#' @param grid_dir Path where cleaned grids were stored from [gfsynopsis::prep_stitch_grids()]
#'
#' @return A dataframe containing a survey grid from [gfsynopsis::prep_stitch_grids()]
#' @export
#'
choose_survey_grid <- function(survey_type, grid_dir) {
  if (grepl("SYN", survey_type)) survey_type <- 'synoptic'
  if (survey_type %in% c("HBLL OUT N", "HBLL OUT S")) survey_type <- "hbll_outside"
  switch(survey_type,
    synoptic = readRDS(file.path(grid_dir, "synoptic_grid.rds")),
    hbll_outside = readRDS(file.path(grid_dir, "hbll_out_grid.rds")),
    hbll_inside = readRDS(file.path(grid_dir, "hbll_ins_grid.rds")),
    mssm = gfdata::mssm_grid |>
      dplyr::filter(year >= 2009 & year < 2022) |>
      dplyr::distinct(X, Y, survey, area),
    stop("Invalid `survey_type` value")
  )
}

# ------------------------------------------------------------------------------
#' Add column containing upper limit for censored poisson
#'
#' @param dat A data frame from [gfsynopsis::prep_stitch_dat()].
#' @param prop_removed_col Name of the column containing the proportion of
#' baits removed in each fishing event from *any* species. I.e., the proportion
#' of hooks returning without bait for any reason.
#' @param n_catch_col Name of the column containing the observed catch counts on
#' each fishing event of the target species.
#' @param n_hooks_col Name of the column containing the number of hooks retrieved
#' on each fishing event.
#' @param pstar_col Name of the column containing a single value between
#' `0 <= pstar <= 1` specifying the breakdown point of observed catch counts as
#' a result of hook competition. See [gfsynopsis::get_pstar()], `[sdmTMB::censored_poisson()]`
#' @param pstar Optional. If `pstar_col` is not specified, pstar can be provided
#' as a single value between 0 <= pstar <= 1` default = NULL.
#'
#' @returns `dat` with a new column `upr` containing  numeric vector of upper
#' bound catch counts of the target species to improve convergence of the censored
#' method. See the documentation in `[sdmTMB::get_censored_upper()]`
#'
#' @export
add_upr <- function(
    dat, prop_removed_col, n_catch_col, n_hooks_col,
    pstar_col = "pstar", pstar = NULL) {
  na_catch <- sum(is.na(dat[[n_catch_col]]))
  stopifnot(
    "\n\tError: missing catch values, filter before adding cpois upr" =
      (na_catch == 0)
  )

  if (is.null(pstar)) {
    pstar <- dat[[pstar_col]][1]
  }

  dat$upr <- sdmTMB:::get_censored_upper(
    dat[[prop_removed_col]], dat[[n_catch_col]],
    dat[[n_hooks_col]], pstar
  )
  dat
}

#' Get stitched index across survey regions in synoptic trawl and HBLL surveys
#'
#' @param survey_dat A data frame from [gfsynopsis::prep_stitch_dat()].
#' @param species A string specifying the `species_common_name`.
#' @param survey_type A string matching one of: "synoptic" (the default), "hbll_outside", "hbll_inside".
#' @param model_type A string matching one of: "st-rw" (the default), "st-rw_tv-rw", or "custom".
#' @param form Optional string specifying model formula.
#'   'catch ~ 1' (the default, unless `family = poisson()` or `family = sdmTMB::censored_possion()`
#'   then "catch ~ 1 + (1|obs_id)").
#' @param form Optional string specifying model formula.
#'    'catch ~ 1' (the default, unless `family = poisson()` or `family = sdmTMB::censored_possion()`
#'    then "catch ~ 1 + (1|obs_id)").
#' @param time An optional time column name (as character), used in [sdmTMB::sdmTMB()].
#'    (default = 'year')
#' @param spatial Estimate spatial random fields? See [sdmTMB::sdmTMB()].
#'    (Default is 'rw').
#' @param spatiotemporal Estimate the spatiotemporal random fields, see [sdmTMB::sdmTMB()].
#'    (Default is 'rw').
#' @param time_varying An optional one-sided formula describing covariates that
#'    should be modelled as a time-varying process. See [sdmTMB::sdmTMB()].
#'    Default is `NULL` if `model_type = st-rw`, and `~1` if `model_type = 'st-rw_tv-rw'`.
#' @param time_varying_type Type of time-varying process to apply to
#'    ‘time_varying’ formula. See [sdmTMB::sdmTMB()]. #'    Default is `NULL`
#'     if `model_type = st-rw`, and `~1` if `model_type = 'st-rw_tv-rw'`.
#' @param mesh Optional mesh object created using [sdmTMB::make_mesh()].
#' @param cutoff If `mesh = NULL`, mesh cutoff for [sdmTMB::make_mesh()].
#' @param family The family and link for [sdmTMB::sdmTMB()].
#' @param offset A string naming the offset column in `dat` used in [sdmTMB::sdmTMB()]
#' @param priors Optional penalties/priors used in [sdmTMB::sdmTMBpriors()].
#' @param silent A boolean. Silent or include optimization details.
#' @param ctrl Optimization control options via [sdmTMB::sdmTMBcontrol()].
#' @param gradient_thresh Threshold used in [sdmTMB::sanity()] (default = 0.001).
#' @param cache A string specifying file path to cache directory.
#' @param check_cache Check whether index file already exists? Default = `FALSE`.
#' @param cache_predictions Cache model predictions? Can be large.
#' @param cache_fits Cache model fits? Can be large.
#' @param survey_grid A data frame containing the spatial grid over which predictions are to be made.
#'    If `survey_grid` = NULL (the default). Grid should contain cell area.
#' @param grid_dir Path where cleaned grids were stored from [gfsynopsis::prep_stitch_grids()]
#'
#' @returns Either a string or dataframe:
#' * `insufficient data to stitch regions` if the number of positive sets is too low to stitch
#' * `Failed sanity check` if the model failed to converge
#' * A dataframe containing the stitched index formatted to use with [gfplot::plot_survey_index()]
#' @export
#'
get_stitched_index <- function(
    survey_dat, species = "arrowtooth flounder",
    survey_type = "synoptic",
    model_type = "st-rw", # TODO: rethink this parameter - either - need?
    form = NULL,
    family = sdmTMB::tweedie(),
    time = "year",
    spatial = "on",
    spatiotemporal = "rw",
    time_varying = NULL,
    time_varying_type = NULL,
    mesh = NULL, cutoff = 20,
    offset = "offset",
    priors = sdmTMB::sdmTMBpriors(),
    silent = TRUE,
    ctrl = sdmTMB::sdmTMBcontrol(nlminb_loops = 1L, newton_loops = 1L),
    gradient_thresh = 0.001,
    cache = NULL,
    check_cache = FALSE,
    cache_predictions = FALSE,
    cache_fits = FALSE,
    survey_grid = NULL,
    grid_dir) {
  pred_cache <- file.path(cache, "predictions")
  fit_cache <- file.path(cache, "fits")
  dir.create(cache, showWarnings = FALSE, recursive = TRUE)
  if (cache_predictions) dir.create(pred_cache, showWarnings = FALSE, recursive = TRUE)
  if (cache_fits) dir.create(fit_cache, showWarnings = FALSE, recursive = TRUE)

  species_hyphens <- clean_name(species)
  .survey_abbrev <- unique(survey_dat$survey_abbrev)

  out_filename <- file.path(cache, paste0(species_hyphens, "_", model_type, ".rds"))

  if (check_cache & file.exists(out_filename)) {
    out <- readRDS(out_filename)
    return(out)
  }
# FIXME START: pull out data filtering before data are passed to stitching function
  if (survey_type == 'mssm' & is.null(survey_dat)) {
    out <- "No MSSM survey data"
    saveRDS(out, out_filename)
    return(out)
  }

  stitch_lu <- get_stitch_lu(survey_dat, species, survey_type)

  # Skip model fitting if fewer than 2 regions have >= 0.05 positive sets
  if (survey_type != "synoptic") {
    stitch_regions_df <- stitch_lu |>
      dplyr::filter(species_common_name %in% {{ species }} &
        include_in_stitch == 1)
  } else {
    wchg_pos_0.05 <- stitch_lu[[which(stitch_lu$survey_abbrev == "SYN WCHG"), "prop_pos"]] > 0.05
    other_regions_0.05 <- sum(stitch_lu[which(stitch_lu$survey_abbrev != "SYN WCHG"), "prop_pos"]) > 0.05

    # If WCHG and other regions are all > 0.05 stitch use all regions
    if (wchg_pos_0.05 & other_regions_0.05) {
      stitch_regions_df <- stitch_lu
    }

    # If WCHG < 0.05 but other regions > 0.05, stitch other regions
    if (!wchg_pos_0.05 & other_regions_0.05) {
      stitch_regions_df <- stitch_lu[which(stitch_lu$survey_abbrev != "SYN WCHG"), ]
    }
    # If WCHG > 0.05 but total coverage of other regions < 0.05, do not stitch
    # Or, if WCGH and all other regions < 0.05 do not stitch
    if ((wchg_pos_0.05 & !other_regions_0.05) | (!wchg_pos_0.05 & !other_regions_0.05)) {
      stitch_regions_df <- data.frame("survey_abbrev" = NA)
    }
  }

  stitch_regions <- stitch_regions_df[["survey_abbrev"]]

  if ((survey_type %in% c('synoptic', 'hbll_outside', 'hbll_inside') &
       length(stitch_regions) < 2) |
      (survey_type %in% c('mssm', 'SYN WCVI', 'SYN WCHG', 'SYN QCS', 'SYN HS', 'HBLL OUT N', 'HBLL OUT S') && length(stitch_regions) == 0)) {
    cat("\n\tInsufficient data to stitch regions for: ", survey_type, species, "\n")
    out <- "insufficient data to stitch regions"
    saveRDS(out, out_filename)
    return(out)
  }
# FIXME END: the above chunk can be pulled out
  # Only calculate positive sets if stitching
  mean_num_sets <- sum(stitch_regions_df$mean_n_sets)
  mean_num_pos_sets <- sum(stitch_regions_df$mean_n_pos)
# FIXME: I think the below filtering can also be pulled out)
  survey_dat <- survey_dat |>
    dplyr::filter(species_common_name == species & survey_type == survey_type &
      survey_abbrev %in% stitch_regions)
# TODO: not sure if this is even needed anymore
  survey_dat <- drop_duplicated_fe(survey_dat) #150

  survey_dat <- droplevels(survey_dat) # drop extra factor levels before running models

  cat("\n\tStitching index for:", species)
  cat("\n\t\t- For regions: ", paste(stitch_regions, collapse = ", "))

  if (is.null(mesh)) {
    cat("\n\t\t- No mesh provided, making mesh with cutoff:", cutoff)
    mesh <- sdmTMB::make_mesh(survey_dat, c("X", "Y"), cutoff = cutoff)
  }

  missing_years <- sdmTMB:::find_missing_time(survey_dat$year)

  if (length(missing_years) < 1L) {
    cat("\n\t\t- No missing time to be filled in.")
    missing_years <- NULL
  } else {
    cat("\n\t\t- Filling in extra_time with:", missing_years)
  }

  if (!is.null(offset)) offset <- survey_dat[[offset]]

  cat("\n\tFitting:", model_type, " ", species, "\n")

  is_cpois <- family$family[[1]] == "censored_poisson"
  intercept <- as.integer(model_type == "st-rw")
  if (is.null(form)) {
    if (is_cpois) {
      survey_dat$obs_id <- as.factor(seq(1, nrow(survey_dat)))
      form <- paste0("catch ~ ", intercept, " + (1 | obs_id)")
    } else {
      form <- paste0("catch ~ ", intercept)
    }
  }
  form <- stats::as.formula(form)
# TODO: get rid of this switch and have only custom function passed through
  fit <- switch(model_type,
    `st-iid` = try(
      sdmTMB::sdmTMB(
        formula = form, family = family,
        time = "year", spatiotemporal = spatiotemporal, spatial = spatial,
        data = survey_dat, mesh = mesh, offset = offset, priors = priors,
        silent = silent, control = ctrl
      )
    ),
    `st-rw` = try(
      sdmTMB::sdmTMB(
        formula = form, family = family,
        time = "year", spatiotemporal = spatiotemporal, spatial = spatial,
        data = survey_dat, mesh = mesh, offset = offset, extra_time = missing_years,
        silent = silent, control = ctrl, priors = priors
      )
    ),
    `st-rw_tv-rw` = try(
      sdmTMB::sdmTMB(
        formula = form, family = family,
        time_varying = ~1, time_varying_type = "rw",
        time = "year", spatiotemporal = spatiotemporal, spatial = spatial,
        data = survey_dat, mesh = mesh, offset = offset, extra_time = missing_years,
        silent = silent, control = ctrl, priors = priors
      )
    ),
    custom = try(
      sdmTMB::sdmTMB(
        formula = form,
        family = family,
        time = time,
        spatial = spatial,
        spatiotemporal = spatiotemporal,
        time_varying = time_varying,
        time_varying_type = time_varying_type,
        data = survey_dat, mesh = mesh, offset = offset, extra_time = missing_years,
        priors = priors,
        silent = silent, control = ctrl
      )
    ),
    stop("Invalid `model_type` value")
  )

  sanity_check <- all(unlist(sdmTMB::sanity(fit, gradient_thresh = gradient_thresh)))

  # Turn off spatial fields if model doesn't fit
  if (!sanity_check && spatiotemporal == "rw" && spatial == "on") {
    message("Sanity check failed, refitting with spatial = 'off'")
    spatial <- "off"
    fit <- try(
      sdmTMB::sdmTMB(
        formula = form, family = family,
        time = "year", spatiotemporal = "rw", spatial = spatial,
        data = survey_dat, mesh = mesh, offset = offset, extra_time = missing_years,
        silent = silent, control = ctrl
      )
    )
    sanity_check <- all(unlist(sdmTMB::sanity(fit, gradient_thresh = gradient_thresh)))
  }

  # Turn off st fields if IID:
  if (!sanity_check && spatiotemporal == "iid") {
    message("Sanity check failed, refitting with spatiotemporal = 'off'")
    spatial <- "off"
    spatiotemporal <- "off"
    fit <- try(
      sdmTMB::sdmTMB(
        formula = form, family = family,
        time = "year", spatiotemporal = spatiotemporal, spatial = spatial,
        data = survey_dat, mesh = mesh, offset = offset,
        silent = silent, control = ctrl, priors = priors
      )
    )
    sanity_check <- all(unlist(sdmTMB::sanity(fit, gradient_thresh = gradient_thresh)))
  }

  if (cache_fits) {
    fit_filename <- file.path(fit_cache, paste0(species_hyphens, "_", model_type, ".rds"))
    cat("\n\tSaving:", fit_filename, "\n")
    saveRDS(fit, fit_filename)
  }

  if (!sanity_check) {
    cat("\n\tFailed sanity check for:", model_type, " ", species, "\n")
    out <- "Failed sanity check"
    saveRDS(out, out_filename)
    return(out)
  }

  #fit <- readRDS(fit_filename)
  if (inherits(fit, "sdmTMB")) {
    cat("\n\tGetting predictions\n")
    # Prepare newdata for getting predictions
    year_range_seq <- min(survey_dat$year):max(survey_dat$year)

    # Allow variable input of grids
    if (is.null(survey_grid)) survey_grid <- choose_survey_grid(survey_type, grid_dir)

    newdata <- sdmTMB::replicate_df(dat = survey_grid, time_name = "year",
      time_values = sort(union(fit$data$year, fit$extra_time))) |>
      dplyr::filter(
        survey %in% fit$data$survey_abbrev
        # year %in% fit$data$year
      ) |>
      droplevels()

    if (survey_type == 'mssm' & isTRUE(grep('year_bin', form))) {
      newdata <- sdmTMB::replicate_df(newdata, time_name = 'year_bin', time_values = unique(fit$data$year_bin))
    }

    newdata$obs_id <- 1L # fake; needed something (1 | obs_id) in formula
    # re_form_iid = NA, so obs_id ignored in prediction
    pred <- stats::predict(fit, newdata, return_tmb_object = TRUE, re_form_iid = NA)

    if (cache_predictions) {
      pred_filename <- file.path(pred_cache, paste0(species_hyphens, "_", model_type, ".rds"))
      cat("\n\tSaving:", pred_filename, "\n")
      saveRDS(pred, pred_filename)
    }
  }
# FIXME: I think I want to add the family used
# TODO: species should probably match rest of the data: species_common_name
  if (length(pred)) {
    cat("\n\tCalculating index\n")
    index <- sdmTMB::get_index(pred, bias_correct = TRUE, area = newdata$area)
    index$aic <- stats::AIC(fit)
    index$spatial <- spatial
    index$spatiotemporal <- spatiotemporal
    index$mean_cv <- mean(sqrt(exp(index$se^2) - 1))
    index$num_sets <- mean_num_sets
    index$num_pos_sets <- mean_num_pos_sets
    index$survey_type <- survey_type
    index$stitch_regions <- paste(stitch_regions, collapse = ", ")
    index$species_common_name <- species
    out <- index |>
      dplyr::rename(
        survey_abbrev = survey_type, biomass = "est",
        lowerci = "lwr", upperci = "upr"
      )
  }
  cat("\n\tSaving:", out_filename, "\n")
  saveRDS(out, out_filename)
  out
}

#' Get proportion of positive sets in each SYN or HBLL region
#'
#' @description
#' A lookup table to see the proportion of positive sets across species and regions.
#' Renders a `flextable` object in a browser with rows highlighted based on
#' the proportion of positive sets ("red" less than or equal to 0.01, "orange"
#' between 0.01 and 0.03, "yellow" between 0.03 and 0.05).
#'
#' @param survey_dat A dataframe from `[gfplot::get_survey_sets()]`
#' @param survey_type A string matching one of: "synoptic", "hbll_outside", "hbll_inside"
#'
#' @returns A `flextable` object in a browser.
#'
get_inclusion_table <- function(survey_dat = NULL, survey_type) {
  survey_dat <- prep_stitch_dat(survey_dat)

  positive_sets <- get_stitch_lu(survey_dat,
    species = unique(survey_dat$species_common_name),
    survey_type = survey_type
  )

  stitch_ft <- positive_sets |>
    dplyr::group_by(species_common_name, survey_type) |>
    dplyr::summarise(stitch_tally = sum(include_in_stitch)) |>
    dplyr::mutate(to_stitch = ifelse(stitch_tally < 2, 0, 1)) |>
    dplyr::right_join(positive_sets) |>
    dplyr::arrange(survey_type, species_common_name)

  cl <- officer::fp_border(color = "black", width = 3)

  break_position <- function(x) {
    z <- data.table::rleidv(x)
    c(z[-length(z)] != z[-1], FALSE)
  }

  inclusion_table <- stitch_ft |>
    flextable::flextable() |>
    flextable::merge_v(x = _, j = "species_common_name") |>
    flextable::hline(i = ~ break_position(species_common_name)) |>
    flextable::fix_border_issues() |>
    flextable::bg(x = _, i = ~ prop_pos <= 0.01, j = 5, bg = "red") |>
    flextable::bg(x = _, i = ~ (prop_pos > 0.01 & prop_pos <= 0.03), j = 5, bg = "orange") |>
    flextable::bg(x = _, i = ~ (prop_pos > 0.03 & prop_pos <= 0.05), j = 5, bg = "yellow")
  inclusion_table
}
# ------------------------------------------------------------------------------

drop_duplicated_fe <- function(x) {
  stopifnot("fishing_event_id" %in% names(x))
  x[!duplicated(x[["fishing_event_id"]]), , drop = FALSE]
}

# ------------------------------------------------------------------------------
# TODO: Putting this here for now, but maybe best moved to gfplot
#' Scale Indices
#'
#' This function scales index values from design-based, geostatistical model-
#' based, or the combination of both for plotting.
#'
#' @param dat_design A data frame containing the design-based indices.
#' @param dat_geostat A data frame containing the output from
#'   \code{sdmTMB::get_index()}.
#' @param .groups A character vector specifying the grouping variables to use
#'   for scaling. Default is \code{c("species_common_name", "survey_abbrev")}.
#' @param scale_by A character string specifying the method to scale by, with
#'   the following cases:
#'   \itemize{
#'     \item If both \code{dat_design} and \code{dat_geostat} are provided:
#'       \itemize{
#'         \item \code{"upperci"}: Indices will be scaled by the maximum scaled
#'           upper confidence interval (\code{upperci}), which itself is scaled
#'           by the geometric mean of the geostatistical index.
#'         \item \code{"geomean"}: Indices will be scaled by the scaled geometric
#'           mean of the geostatistical index.
#'       }
#'     \item If only one of \code{dat_design} or \code{dat_geostat} is provided:
#'       \itemize{
#'         \item \code{"upperci"}:w Indices will be scaled by the maximum
#'           \code{upperci} of the available index.
#'         \item \code{"geomean"}: Indices will be scaled by the geometric mean
#'           of the available index.
#'       }
#'   }
#'
#' @return A data frame similar to \code{dat_design}, with additional scaled
#'   columns such as \code{biomass_scaled}, \code{lowerci_scaled}, and
#'   \code{upperci_scaled}.
#' @details
#' This function scales index values based on specified methods, grouping by
#' specified columns. If no valid data is available (e.g., all \code{NA} values
#' in the relevant column), scaled values will be set to \code{NA_real_}.
#' @examples
#' # Example?

scale_indices <- function(dat_design = NULL, dat_geostat = NULL,
  .groups = c("species_common_name", "survey_abbrev"),
  scale_by = c("upperci", "geomean")) {

  if (!is.null(dat_design)) dat_design$method <- "design"
  if (!is.null(dat_geostat))    dat_geostat$method <- "geostat"

  dat <- bind_rows(dat_design, dat_geostat)

  dat_scaled <- dat |>
    group_by(across(all_of(.groups))) |>
    group_split() |>
    purrr::map_dfr(\(x) {
      scale_by <- match.arg(scale_by, c("upperci", "geomean"))
      both_present <- length(unique(x$method[!is.na(x$biomass)])) > 1L

      if (both_present) {
        x_geo <- filter(x, method == "geostat")
        x_des <- filter(x, method == "design")

        overlapping_years <- inner_join(
          select(x_geo, year),
          select(x_des, year, biomass),
          by = join_by(year)
        ) |>
          filter(biomass > 0) |> # can't take geometric mean of these!
          pull("year")

        x_geo_mean <- exp(mean(log(x_geo$biomass[x_geo$year %in% overlapping_years])))
        x_des_mean <- exp(mean(log(x_des$biomass[x_des$year %in% overlapping_years])))

        x_geo <- mutate(x_geo,
          biomass_scaled = biomass / x_geo_mean,
          lowerci_scaled = lowerci / x_geo_mean,
          upperci_scaled = upperci / x_geo_mean
        )
        x_des <- mutate(x_des,
          biomass_scaled = biomass / x_des_mean,
          lowerci_scaled = lowerci / x_des_mean,
          upperci_scaled = upperci / x_des_mean
        )

        scale_factor <- switch(
          scale_by,
          geomean = exp(mean(log(x_geo$biomass_scaled[x_geo$year %in% overlapping_years]), na.rm = TRUE)),
          upperci = max(x_geo$upperci_scaled, na.rm = TRUE)
        )

        xx <- bind_rows(x_geo, x_des)
        x <- mutate(xx,
          biomass_scaled = biomass_scaled / scale_factor,
          lowerci_scaled = lowerci_scaled / scale_factor,
          upperci_scaled = upperci_scaled / scale_factor
        )
      } else {
        if (sum(!is.na(x$biomass))) {
          switch(scale_by,
            upperci = mutate(x,
              biomass_scaled = biomass / max(upperci, na.rm = TRUE),
              lowerci_scaled = lowerci / max(upperci, na.rm = TRUE),
              upperci_scaled = upperci / max(upperci, na.rm = TRUE)
            ),
            geomean = mutate(x,
              biomass_scaled = biomass / exp(mean(log(biomass), na.rm = TRUE)),
              lowerci_scaled = lowerci / exp(mean(log(biomass), na.rm = TRUE)),
              upperci_scaled = upperci / exp(mean(log(biomass), na.rm = TRUE))
            ))
        } else {
          mutate(x,
            biomass_scaled = NA_real_,
            lowerci_scaled = NA_real_,
            upperci_scaled = NA_real_
          )
        }
      }
    })
}