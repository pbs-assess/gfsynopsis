#' Fit geostatistical survey models and make maps
#'
#' `fit_survey_maps()` is a helper function that calls
#' [gfplot::fit_survey_sets()] appropriately for the various surveys.
#'
#' @param dat Data from [gfplot::get_survey_sets()].
#' @param species The species name.
#' @param include_depth Logical for whether or not to include depth and depth
#'   squared as predictors.
#' @param model Whether to fit an INLA or glmmfields model.
#' @param surveys Character vector describing the surveys to fit. Can include
#'   multiple surveys if desired.
#' @param years The years of survey data to use.
#' @param ... Any other arguments to pass to [gfplot::fit_survey_sets()].
#'
#' @export
fit_survey_maps <- function(dat,
  species = "pacific cod", include_depth = TRUE,
  model = c("inla", "glmmfields", "sdmTMB"),
  surveys = c("SYN QCS", "SYN HS", "SYN WCHG", "SYN WCVI"),
  years = c(2017, 2018),
  ...) {
  dat <- dplyr::filter(dat, species_common_name %in% species)
  dat <- dplyr::filter(dat, year %in% years)

  model <- match.arg(model)
  out <- lapply(surveys, function(surv) {
    if (!surv %in% c("HBLL OUT N", "HBLL OUT S", "IPHC FISS", "SYN QCS", "SYN HS", "SYN WCHG", "SYN WCVI"))
      stop("survey value was '", surv, "' but must be one of ",
        "c('HBLL OUT N', 'HBLL OUT S', 'IPHC FISS', 'SYN QCS', 'SYN HS', 'SYN WCHG', 'SYN WCVI')")
    message("Fitting model for the survey ", surv)

    if (surv %in% c("HBLL OUT N", "HBLL OUT S")) {
      density_column <- "density_ppkm2"
      .dat <- filter(dat, survey_abbrev %in% surv)
      if (nrow(.dat) == 0L) stop("No survey data.")
      .dat$survey_abbrev <- surv
      # .dat$year <- years[2]
      premade_grid <- if (surv == "HBLL OUT N") gfplot::hbll_n_grid else gfplot::hbll_s_grid
      raw_dat <- tidy_survey_sets(.dat, surv,
        years = years, density_column = density_column
      )
    }
    if (surv == "IPHC FISS") {
      density_column <- "density_ppkm2"
      # .dat <- filter(dat, year %in% years[2]) # just last year
      .dat <- filter(dat, year %in% years)
      .dat <- filter(.dat, survey_abbrev %in% surv)
      if (nrow(.dat) == 0L) stop("No survey data.")
      raw_dat <- tidy_survey_sets(.dat, surv,
        # years = years[2], density_column = density_column
        years = years, density_column = density_column
      )
      premade_grid <- filter(raw_dat, year == max(year)) %>%
        select(lon, lat, depth) %>%
        rename(X = lon, Y = lat)
      premade_grid <- list(grid = premade_grid, cell_area = 1.0)
    }
    if (surv %in% c("SYN QCS", "SYN HS", "SYN WCHG", "SYN WCVI")) {
      density_column <- "density_kgpm2"
      .dat <- filter(dat, survey_abbrev %in% surv)
      premade_grid <- NULL
      raw_dat <- tidy_survey_sets(.dat, surv,
        years = years, density_column = "density_kgpm2"
      )
    }

    if (model == "inla") {
      m <- fit_survey_sets(.dat, survey = surv, years = years,
        model = "inla", mcmc_posterior_samples = 800,
        density_column = density_column,
        premade_grid = premade_grid, required_obs_percent = 0.02,
        ...)
    }
    if (model == "glmmfields") {
      stop("NEED TO CHECK GLMMFIELDS")
      m <- fit_survey_sets(.dat, survey = surv, years = years,
        model = "glmmfields", chains = 1, iter = 800,
        mcmc_posterior_samples = 300, n_knots = 25, ...)
    }
    if (model == "sdmTMB") {
      m <- fit_survey_sets(.dat, survey = surv, years = years,
        model = "sdmTMB",
        density_column = density_column, tmb_knots = 200,
        premade_grid = premade_grid, required_obs_percent = 0.02,
        ...)
      # we may have predicted all years, but just save last year for plotting:
      raw_dat <- filter(m$data, year == max(m$data$year))
    }
    list(model = m, raw_dat = raw_dat)
  })

  pred_dat <- purrr::map_df(out, function(x) data.frame(x$model$predictions, survey = x$model$survey, stringsAsFactors = FALSE))
  raw_dat  <- purrr::map_df(out, function(x) data.frame(x$raw_dat, survey = x$model$survey, stringsAsFactors = FALSE))
  models   <- purrr::map(out,    function(x) x$model)

  list(pred_dat = pred_dat, models = models, raw_dat = raw_dat,
    species = species, include_depth = include_depth)
}

#' Plot survey maps
#'
#' `plot_survey_maps()` is a helper function that calls
#' [gfplot::plot_survey_sets()] appropriately for the various surveys.
#'
#' @param pred_dat The `pred_dat` element from [fit_survey_maps()].
#' @param raw_dat The `raw_dat` element from [fit_survey_maps()].
#' @param show_axes Logical for whether or not to show axes.
#' @param show_raw_data Logical for whether or not show the raw data.
#' @param pos_pt_col The colour for the positive points.
#' @param bin_pt_col The colour for the occurrence/binary points.
#' @param pos_pt_fill The fill colour for the positive points.
#' @param north_symbol Logical for whether or not to include the North symbol.
#' @param fill_column A character object describing the fill column name.
#' @param trans The transformation function to apply to the color scale. Passed
#'   to [viridis::scale_fill_viridis()].
#' @param show_model_predictions Logical for whether or not to show the model
#'   predictions.
#' @param annotations A character object describing which annotations to
#'   include. These annotations include the relevent years on the map.
#' @param syn_qcs_hs_year Year for a synoptic survey label.
#' @param syn_wcvi_wchg_year Year for a synoptic survey label.
#' @param hbll_n_year Year for a HBLL N survey label.
#' @param hbll_s_year Year for a HBLL S survey label.
#' @param iphc_year Year for IPHC survey label.
#' @param ... Any other arguments to pass to [gfplot::plot_survey_sets()].
#'
#' @export
plot_survey_maps <- function(pred_dat, raw_dat, show_axes = FALSE,
  show_raw_data = TRUE, pos_pt_col = "#FFFFFF60",
  bin_pt_col = "#FFFFFF40",
  pos_pt_fill = "#FFFFFF05",
  north_symbol = FALSE,
  fill_column = "combined",
  trans = "sqrt",
  show_model_predictions = TRUE,
  annotations = c("SYN", "IPHC", "HBLL"),
  syn_qcs_hs_year = 2019,
  syn_wcvi_wchg_year = 2018,
  hbll_n_year = 2017,
  hbll_s_year = 2016,
  iphc_year = 2017,
  ...) {

  annotations <- match.arg(annotations)

  # Manually avoid extrapolating within each survey:
  raw_dat_depth_ranges <- group_by(raw_dat, survey) %>%
    summarise(min_raw_depth = min(depth, na.rm = TRUE),
      max_raw_depth = max(depth, na.rm = TRUE)) %>%
    mutate(
      min_raw_depth = ifelse(!is.na(min_raw_depth), min_raw_depth, Inf),
      max_raw_depth = ifelse(!is.na(max_raw_depth), max_raw_depth, -Inf))

  # Set to NA predictions outside of the range within that survey and year:
  pred_dat <- left_join(pred_dat, raw_dat_depth_ranges)
  pred_dat[pred_dat$akima_depth < pred_dat$min_raw_depth |
      pred_dat$akima_depth > pred_dat$max_raw_depth, fill_column] <- NA

  # # xxx <- pred_dat
  # pred_dat <- xxx
  .q <- quantile(pred_dat$combined, probs = 0.998, na.rm = TRUE)[[1]]
  pred_dat$combined[pred_dat$combined > .q] <- .q

  g <- plot_survey_sets(pred_dat, raw_dat,
    fill_column = fill_column, show_model_predictions = show_model_predictions,
    show_raw_data = show_raw_data,
    pos_pt_col = pos_pt_col,
    bin_pt_col = bin_pt_col,
    pos_pt_fill = pos_pt_fill,
    fill_scale =
      viridis::scale_fill_viridis(trans = trans, option = "C"),
    colour_scale =
      viridis::scale_colour_viridis(trans = trans, option = "C"),
    rotation_center = c(500, 5700), rotation_angle = 40,
    north_symbol = north_symbol,
    xlim = c(375, 680), ylim = c(5200, 6150), x_buffer = 0, y_buffer = 0,
    north_symbol_coord = c(130, 5975), show_axes = show_axes,
    extrapolate_depth = TRUE, ...
  ) + ggplot2::theme(legend.position = "bottom") +
    guides(fill = FALSE, size = FALSE)

  if (annotations == "SYN")
    g <- g + ggplot2::annotate("text", 390, 6090, label = syn_wcvi_wchg_year, col = "grey30") +
      ggplot2::annotate("text", 390, 5800, label = syn_qcs_hs_year, col = "grey30") +
      ggplot2::annotate("text", 390, 5450, label = syn_wcvi_wchg_year, col = "grey30")

  if (annotations == "HBLL")
    g <- g + ggplot2::annotate("text", 390, 5990, label = hbll_n_year, col = "grey30") +
    ggplot2::annotate("text", 390, 5550, label = hbll_s_year, col = "grey30")

  if (annotations == "IPHC")
    g <- g + ggplot2::annotate("text", 390, 5700, label = iphc_year, col = "grey30")

  g
}
