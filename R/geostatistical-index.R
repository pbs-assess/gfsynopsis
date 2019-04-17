#' Fit a geostatistical index standardization model to survey data
#'
#' @param species_rds File path to an .rds file from [gfplot::cache_pbs_data()].
#' @param survey Survey abbreviation for the survey to fit.
#' @param species_name Species name. Only used in the output to help with processing.
#' @param n_knots Number of knots in the SPDE approximation.
#' @param cell_width Sell width for the prediction grid.
#' @param anisotropy Logical for anisotropy.
#' @param silent Logical for verbosity of output.
#' @param bias_correct Logical for bias correction from TMB.
#' @param include_depth Logical for whether to include depth in the model.
#'
#' @return A list object.
#' @export
fit_sdmTMB_westcoast <- function(species_rds, survey,
  species_name = "", n_knots = 150, cell_width = 2,
  anisotropy = FALSE, silent = TRUE, bias_correct = FALSE,
  include_depth = FALSE) {

  d <- readRDS(species_rds)$survey_sets
  d <- dplyr::filter(d, !(year == 2014 & survey_abbrev == "SYN WCHG")) # not used
  col <- if (grepl("SYN", survey)) "density_kgpm2" else "density_ppkm2"
  dat <- gfplot:::tidy_survey_sets(d, survey, years = seq(1, 1e6),
    density_column = col)

  if (mean(dat$present) < 0.05) stop("Not enough data.")

  .scale <- if (grepl("SYN", survey)) 1000 else 1 # for computational stability
  dat <- dplyr::mutate(dat, density = density * .scale)
  if (any(is.na(dat$depth)))
    dat <- gfplot:::interp_survey_bathymetry(dat)$data
  dat <- gfplot:::scale_survey_predictors(dat)

  if (grepl("SYN", survey)) {
    # grid_locs <- gfplot:::make_prediction_grid(
    #   dplyr::filter(dat, year == max(dat$year)), survey = survey,
    #   cell_width = cell_width)$grid
    # grid_locs <- dplyr::rename(grid_locs, depth = akima_depth)

    grid_locs <- gfplot::synoptic_grid %>%
      dplyr::filter(.data$survey == survey) %>%
      dplyr::select(.data$X, .data$Y, .data$depth)

    grid_locs$depth_scaled <-
      (log(grid_locs$depth) - dat$depth_mean[1]) / dat$depth_sd[1]
    grid_locs$depth_scaled2 <- grid_locs$depth_scaled^2

    # Expand the prediction grid to create a slice for each time:
    original_time <- sort(unique(dat$year))
    nd <- do.call("rbind",
      replicate(length(original_time), grid_locs, simplify = FALSE))
    nd[["year"]] <- rep(original_time, each = nrow(grid_locs))
    grid_locs <- nd

    # grid_locs <- dplyr::select(grid_locs, .data$X, .data$Y, .data$depth_scaled,
    #   .data$depth_scaled2)
  } else {
    stop("Non-synoptic surveys are not implemented yet.")
    grid_locs <- if (surv == "HBLL OUT N") gfplot::hbll_n_grid$grid else gfplot::hbll_s_grid$grid
  }
  # grid_locs$year <- NULL
  formula <- if (include_depth) {
    stats::as.formula(density ~ 0 + as.factor(year))
  } else {
    stats::as.formula(density ~ 0 + as.factor(year) + depth_scaled + depth_scaled2)
  }
  spde <- sdmTMB::make_spde(dat$X, dat$Y, n_knots = n_knots)
  m <- sdmTMB::sdmTMB(
    formula = formula,
    data = dat, time = "year", spde = spde, family = sdmTMB::tweedie(link = "log"),
    anisotropy = anisotropy, silent = silent)
  predictions <- stats::predict(m, newdata = grid_locs)
  index <- sdmTMB::get_index(predictions, bias_correct = bias_correct)
  index <- dplyr::mutate(index, cv = sqrt(exp(se^2) - 1))
  list(
    data = dat,
    model = m,
    spde = spde,
    predictions = predictions,
    index = index,
    scale = .scale,
    survey = survey,
    species_name = species_name
  )
}
