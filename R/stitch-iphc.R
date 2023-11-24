#' Prepare IPHC FISS data for index stitching
#'
#' @param survey_dat A dataframe from `[gfplot::get_survey_sets()]`
#' @param hook_dat A dataframe from [gfsynopsis::get_ll_bait_counts()]
#'
#' @returns A dataframe the same length as `survey_dat`
#' @export

prep_iphc_stitch_dat <- function(survey_dat, hook_dat) {
  clean_dat <-
    dplyr::left_join(survey_dat, hook_dat, by = c("year", "station", "lat", "lon")) |> # get observed hook counts
    sdmTMB::add_utm_columns(c("lon", "lat"), utm_crs = 32609) |>
    dplyr::mutate(
      catch = ifelse(!is.na(N_it), N_it, N_it20),
      sample_n = ifelse(!is.na(N_it), "whole_haul", "20_hook"),
      effSkate = ifelse(!is.na(N_it), E_it, E_it20),
      hook_removed = obsHooksPerSet - baited_hooks
    ) |>
    dplyr::mutate(prop_removed = hook_removed / obsHooksPerSet) |>
    dplyr::mutate(baited_hooks = replace(baited_hooks, which(baited_hooks == 0), 1)) |>
    dplyr::mutate(prop_bait_hooks = baited_hooks / obsHooksPerSet) |>
    dplyr::mutate(
      hook_adjust_factor = -log(prop_bait_hooks) / (1 - prop_bait_hooks),
      prop_removed = 1 - prop_bait_hooks
    ) |>
    dplyr::mutate(offset = log(effSkate * hook_adjust_factor)) |> # use ICR for hook competition for now
    dplyr::mutate(present = case_when(catch > 0 ~ 1, catch == 0 ~ 0, TRUE ~ NA)) |> # useful for plotting and pos sets
    dplyr::mutate(
      fyear = factor(year),
      fstation = factor(station)
    ) |> # mgcv needs factor inputs
    dplyr::filter(usable == "Y", standard == "Y", !is.na(catch)) #|> # some species weren't measured at different points in time series
    # ADD filtering out of species before they were explicitly identified
    #dplyr::filter(!(species_common_name %in% c("big skate", "longnose skate") & year < 1998)) |>
    #dplyr::filter(!(species_common_name == "shortspine thornyhead" & year < 1998)) # first shows up in 1998
}

#' Get table of positive sets for IPHC FISS data
#'
#' @param survey_dat A dataframe from [gfsynopsis::prep_iphc_stitch_dat()]
#'
#' @returns A dataframe
#' @export
get_iphc_pos_sets <- function(survey_dat) {
  survey_dat |>
    mutate(measured = ifelse(!is.na(present), 1, 0)) |>
    group_by(species_common_name, year) |>
    summarise(
      n_sets = sum(measured), # get sets where we are pretty sure they counted this species
      n_pos = sum(present, na.rm = TRUE),
      .groups = "drop_last"
    ) |>
    dplyr::summarise(
      mean_n_pos = mean(n_pos), mean_n_sets = mean(n_sets),
      mean_prop_pos = mean_n_pos / mean_n_sets,
      .groups = "drop"
    )
}

#' Get index using `sdmTMB` for IPHC surveys
#'
#' @param survey_dat A data frame from [gfsynopsis::prep_iphc_stitch_dat()].
#' @param species A string specifying the `species_common_name`.
#' @param model_type An suffix to the filename indicating model type (default = "st-rw").
#' @param form Optional string specifying model formula.
#'    'catch ~ 1' (the default, unless `family = poisson()` or `family = sdmTMB::censored_possion()`
#'    then "catch ~ 1 + (1|obs_id)").
#' @param time An optional time column name (as character), used in [sdmTMB::sdmTMB()].
#'    (default = 'year')
#' @param spatial Estimate spatial random fields? See [sdmTMB::sdmTMB()].
#'    Default is 'rw' if `model_type = "st-rw" or "st-rw_tv-rw".
#' @param spatiotemporal Estimate the spatiotemporal random fields, see [sdmTMB::sdmTMB()].
#'    Default is 'rw'   if `model_type = "st-rw" or "st-rw_tv-rw".
#' @param time_varying An optional one-sided formula describing covariates that
#'    should be modelled as a time-varying process. See [sdmTMB::sdmTMB()].
#'    Default is `NULL`.
#' @param time_varying_type Type of time-varying process to apply to
#'    ‘time_varying’ formula. See [sdmTMB::sdmTMB()]. Default is `NULL`.
#' @param mesh Optional mesh object created using [sdmTMB::make_mesh()].
#' @param cutoff If `mesh = NULL`, mesh cutoff for [sdmTMB::make_mesh()].
#' @param family The family and link for [sdmTMB::sdmTMB()].
#' @param offset A string naming the offset column in `dat` used in [sdmTMB::sdmTMB()]
#'    (default = 'log_eff_skate').
#' @param priors Optional penalties/priors used in [sdmTMB::sdmTMBpriors()].
#' @param silent A boolean. Silent or include optimization details.
#' @param ctrl Optimization control options via [sdmTMB::sdmTMBcontrol()].
#' @param gradient_thresh Threshold used in [sdmTMB::sanity()] (default = 0.001).
#' @param cache A string specifying file path to cache directory.
#' @param check_cache Check whether index file already exists? Default = `FALSE`.
#' @param grid A data frame containing the locations of IPHC stations used to make predictions and generate the index.
#'
#' @returns Either a string or dataframe:
#' * `insufficient data to stitch regions` if the number of positive sets is too low to stitch
#' * `Failed sanity check` if the model failed to converge
#' * A dataframe containing the stitched index formatted to use with [gfplot::plot_survey_index()]
#' @export
#'
get_iphc_stitched_index <- function(
    survey_dat,
    species,
    model_type = "st-rw",
    form = NULL,
    family = sdmTMB::tweedie(),
    time = "year",
    spatial = "on",
    spatiotemporal = "rw",
    time_varying = NULL,
    time_varying_type = NULL,
    mesh = NULL, cutoff = 20,
    offset = "log_eff_skate",
    priors = sdmTMB::sdmTMBpriors(),
    silent = TRUE,
    ctrl = sdmTMB::sdmTMBcontrol(), # sdmTMB::sdmTMBcontrol(nlminb_loops = 1L, newton_loops = 1L),
    gradient_thresh = 0.001,
    cache = NULL,
    check_cache = FALSE,
    grid) {
  pred_cache <- file.path(cache, "predictions")
  fit_cache <- file.path(cache, "fits")

  dir.create(cache, showWarnings = FALSE, recursive = TRUE)
  dir.create(pred_cache, showWarnings = FALSE, recursive = TRUE)
  dir.create(fit_cache, showWarnings = FALSE, recursive = TRUE)

  species_hyphens <- clean_name(species)
  out_filename <- file.path(cache, paste0(species_hyphens, "_", model_type, ".rds"))

  if (check_cache & file.exists(out_filename)) {
    out <- readRDS(out_filename)
    return(out)
  }

  iphc_pos_sets <- bind_rows(survey_dat) |>
    get_iphc_pos_sets()

  # Use only species with proportion of positive sets >= 5%
  iphc_stitch_lu <- iphc_pos_sets |>
    filter(mean_prop_pos >= 0.05)

  if (length(iphc_stitch_lu$species_common_name) == 0) {
    cat("\n\tInsufficient data to stitch regions for: ", species, "\n")
    out <- "insufficient data to stitch regions"
    saveRDS(out, out_filename)
    return(out)
  }

  mean_num_sets <- iphc_stitch_lu$mean_n_sets
  mean_num_pos_sets <- iphc_stitch_lu$mean_n_pos

  cat("\n\tStitching index for:", species)

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

  is_cpois <- family$family == "censored_poisson"
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

  cat("\n\tFitting:", model_type, " ", species, "\n")

  fit <- try(sdmTMB::sdmTMB(
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
  ))

  sanity_check <- all(unlist(sdmTMB::sanity(fit, gradient_thresh = gradient_thresh)))

  # Turn off spatial fields if model doesn't fit
  if (!sanity_check && spatiotemporal == "rw" && spatial == "on") {
    message("Sanity check failed, refitting with spatial = 'off'")
    spatial <- "off"
    fit <- try(
      sdmTMB::sdmTMB(
        formula = form, family = family, time_varying = time_varying,
        time = "year", spatiotemporal = "rw", spatial = "off", priors = priors,
        data = survey_dat, mesh = mesh, offset = offset, extra_time = missing_years,
        silent = silent, control = ctrl
      )
    )
    sanity_check <- all(unlist(sdmTMB::sanity(fit, gradient_thresh = gradient_thresh)))
  }

  fit_filename <- file.path(fit_cache, paste0(species_hyphens, "_", model_type, ".rds"))
  # cat("\n\tSaving:", fit_filename, "\n")
  # saveRDS(fit, fit_filename)

  if (!all(unlist(sdmTMB::sanity(fit, gradient_thresh = gradient_thresh)))) {
    cat("\n\tFailed sanity check for:", model_type, " ", species, "\n")
    out <- "Failed sanity check"
    saveRDS(out, out_filename)
    return(out)
  }

  if (inherits(fit, "sdmTMB")) {
    cat("\n\tGetting predictions\n")
    # Prepare newdata for getting predictions
    year_range_seq <- min(fit$data$year):max(fit$data$year)
    newdata <- sdmTMB::replicate_df(dat = grid, time_name = "year", time_values = year_range_seq) |>
      dplyr::filter(year %in% fit$data$year) |>
      droplevels()
    newdata$obs_id <- 1L # fake; needed something (1 | obs_id) in formula
    pred <- stats::predict(fit, newdata, return_tmb_object = TRUE, re_form_iid = NA)
    pred$species <- unique(fit$data$species_common_name)

    pred_filename <- file.path(pred_cache, paste0(species_hyphens, "_", model_type, ".rds"))
    # cat("\n\tSaving:", pred_filename, "\n")
    # saveRDS(pred, pred_filename)
  }

  message("Getting index for: ", species)
  index <- try(sdmTMB::get_index(pred, bias_correct = TRUE, area = 1))
  index$spatial <- spatial
  index$aic <- stats::AIC(fit)
  index$mean_cv <- mean(sqrt(exp(index$se^2) - 1))
  index$num_sets <- mean_num_sets
  index$num_pos_sets <- mean_num_pos_sets
  index$survey_type <- "iphc"
  out <- index |>
    dplyr::rename(biomass = "est", lowerci = "lwr", upperci = "upr") |>
    dplyr::mutate(species = species)

  cat("\n\tSaving:", out_filename, "\n")
  saveRDS(out, out_filename)
  out
}
