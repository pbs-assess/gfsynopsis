#' Prepare IPHC FISS data for index stitching
#'
#' @param spp_dat A dataframe from [gfplot::get_survey_sets()]
#' @param hook_dat A dataframe from [gfsynopsis::get_ll_bait_counts()]
#'
#' @returns A dataframe the same length as `spp_dat`

prep_iphc_stitch_dat <- function(spp_dat, hook_dat) {
  clean_dat <-
    left_join(spp_dat, hook_dat, by = join_by('year', 'station', 'lat', 'lon')) |> # get observed hook counts
    # @QUESTION: Should we calculate hook values from effective skate for 1995?
    # For now for 1995, multiply effective skate number by 100, since and effective
    # skate of 1 is meant to represent 100 hooks (with a few other caveats).
    sdmTMB::add_utm_columns(c("lon", "lat"), utm_crs = 32609) |>
    mutate(obsHooksPerSet = ifelse(year == 1995 & is.na(obsHooksPerSet), E_it * 100, obsHooksPerSet)) |>
    mutate(catch = ifelse(!is.na(N_it), N_it, N_it20),
           sample_n = ifelse(!is.na(N_it), 'whole_haul', '20_hook'),
           effSkate = ifelse(!is.na(N_it), E_it, E_it20),
           hook_removed = obsHooksPerSet - baited_hooks) |>
    mutate(prop_removed = hook_removed / obsHooksPerSet) |>
    mutate(present = case_when(catch > 0 ~ 1, catch == 0 ~ 0, TRUE ~ NA)) |> # useful for plotting and pos sets
    mutate(fyear = factor(year),
           log_eff_skate = log(effSkate),
           fstation = factor(station)) # mgcv needs factor inputs
}

#' Get table of positive sets for IPHC FISS data
#'
#' @param spp_dat A dataframe from [gfsynopsis::prep_stitch_dat()]
#' @param species A string specifying the `species_common_name`
#' @param survey_type A string matching one of: "synoptic", "hbll_outside", "hbll_inside"
#'
#' @returns A dataframe
#' @export
get_iphc_pos_sets <- function(clean_iphc_dat) {
  clean_iphc_dat |>
  mutate(measured = ifelse(!is.na(present), 1, 0)) |>
  group_by(species_common_name, year) |>
  summarise(n_sets = sum(measured), # get sets where we are pretty sure they counted this species
            n_pos  = sum(present, na.rm = TRUE),
            .groups = 'drop_last') |>
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
#' @param mesh Optional mesh object created using [sdmTMB::make_mesh()].
#' @param cutoff If `mesh = NULL`, mesh cutoff for [sdmTMB::make_mesh()].
#' @param family The family and link for [sdmTMB::sdmTMB()].
#' @param offset A string naming the offset column in `dat` used in [sdmTMB::sdmTMB()] (default = 'log_eff_skate').
#' @param silent A boolean. Silent or include optimization details.
#' @param ctrl Optimization control options via [sdmTMB::sdmTMBcontrol()].
#' @param gradient_thresh Threshold used in [sdmTMB::sanity()] (default = 0.001).
#' @param cache A string specifying file path to cache directory.
#' @param grid A data frame containing the locations of IPHC stations used to make predictions and generate the index.
#'
#' @returns Either a string or dataframe:
#' * `insufficient data to stitch regions` if the number of positive sets is too low to stitch
#' * `Failed sanity check` if the model failed to converge
#' * A dataframe containing the stitched index formatted to use with [gfplot::plot_survey_index()]
#' @export
#'
get_iphc_stitched_index <- function(survey_dat,
    species,
    model_type = "st-rw",
    form = NULL,
    family = sdmTMB::tweedie(),
    time = 'year',
    spatial = 'on',
    spatiotemporal = 'rw',
    time_varying = NULL,
    time_varying_type = NULL,
    data = survey_dat,
    mesh = NULL, cutoff = 20,
    offset = 'log_eff_skate',
    extra_time = missing_years,
    priors = sdmTMB::sdmTMBpriors(),
    silent = TRUE,
    ctrl = sdmTMB::sdmTMBcontrol(), #sdmTMB::sdmTMBcontrol(nlminb_loops = 1L, newton_loops = 1L),
    gradient_thresh = 0.001,
    cache = NULL,
    grid) {

  pred_cache <- file.path(cache, 'predictions')
  fit_cache <- file.path(cache, 'fits')

  dir.create(cache, showWarnings = FALSE, recursive = TRUE)
  dir.create(pred_cache, showWarnings = FALSE, recursive = TRUE)
  dir.create(fit_cache, showWarnings = FALSE, recursive = TRUE)

  species_hyphens <- gfsynopsis:::clean_name(species)
  out_filename <- file.path(cache, paste0(species_hyphens, "_", model_type, ".rds"))

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
  form <- as.formula(form)

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
      )
    )

  fit_filename <- file.path(fit_cache, paste0(species_hyphens, "_", model_type, ".rds"))
  cat("\n\tSaving:", fit_filename, "\n")
  saveRDS(fit, fit_filename)

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
    newdata <- sdmTMB::replicate_df(dat = grid, time_name = 'year', time_values = year_range_seq) |>
    dplyr::filter(year %in% fit$data$year) |>
    droplevels()
    newdata$obs_id <- 1L # fake; needed something (1 | obs_id) in formula
    pred <- predict(fit, newdata, return_tmb_object = TRUE, re_form_iid = NA)
    pred$species <- unique(fit$data$species)

    pred_filename <- file.path(pred_cache, paste0(species_hyphens, "_", model_type, ".rds"))
    cat("\n\tSaving:", pred_filename, "\n")
    saveRDS(pred, pred_filename)
  }

  message('Getting index for: ', pred$species)
  index <- try(sdmTMB::get_index(pred, bias_correct = TRUE, area = 1))
  index$mean_cv <- mean(sqrt(exp(index$se^2) - 1))
  index$num_sets <- mean_num_sets
  index$num_pos_sets <- mean_num_pos_sets
  index$survey_type <- survey_type
  out <- index |>
    dplyr::rename(biomass = "est", lowerci = "lwr", upperci = "upr") |>
    dplyr::mutate(species = pred$species)

  cat("\n\tSaving:", out_filename, "\n")
  saveRDS(out, out_filename)
  out
}