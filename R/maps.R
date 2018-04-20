#' @export
fit_survey_maps <- function(dat,
  species = "pacific cod", include_depth = TRUE,
  model = c("inla", "glmmfields"),
  surveys = c("SYN QCS", "SYN HS", "SYN WCHG", "SYN WCVI"),
  years = c(2016, 2017),
  ...) {
  dat <- dplyr::filter(dat, species_common_name %in% species)
  dat <- dplyr::filter(dat, year %in% years)

  model <- match.arg(model)
  out <- lapply(surveys, function(surv) {
    if (!surv %in% c("HBLL OUT", "IPHC FISS", "SYN QCS", "SYN HS", "SYN WCHG", "SYN WCVI"))
      stop("survey value was '", surv, "' but must be one of ",
        "c('HBLL OUT', 'IPHC FISS', 'SYN QCS', 'SYN HS', 'SYN WCHG', 'SYN WCVI')")
    message("Fitting model for the survey ", surv)

    if (surv == "HBLL OUT") {
      density_column <- "density_ppkm2"
      .dat <- filter(dat, survey_abbrev %in% c("HBLL OUT N", "HBLL OUT S"))
      .dat$survey_abbrev <- "HBLL OUT"
      .dat$year <- years[2]
      premade_grid <- gfplot::hbll_grid
      raw_dat <- tidy_survey_sets(.dat, "HBLL OUT",
        years = years[2], density_column = density_column
      )
    }
    if (surv == "IPHC FISS") {
      density_column <- "density_ppkm2"
      .dat <- filter(dat, year %in% years[2]) # just last year
      .dat <- filter(.dat, survey_abbrev %in% surv)
      raw_dat <- tidy_survey_sets(.dat, surv,
        years = years[2], density_column = density_column
      )

      premade_grid <- select(raw_dat, lon, lat, depth) %>%
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
      model <- fit_survey_sets(.dat, survey = surv, years = years,
        model = "inla", mcmc_posterior_samples = 800,
        density_column = density_column,
        premade_grid = premade_grid,
        ...)
    } else {
      stop("NEED TO CHECK GLMMFIELDS")
      model <- fit_survey_sets(.dat, survey = surv, years = years,
        model = "glmmfields", chains = 1, iter = 800,
        mcmc_posterior_samples = 300, n_knots = 25, ...)
    }
    list(model = model, raw_dat = raw_dat)
  })

  pred_dat <- purrr::map_df(out, function(x) x$model$predictions)
  raw_dat  <- purrr::map_df(out, function(x) x$raw_dat)
  models   <- purrr::map(out,    function(x) x$model)

  list(pred_dat = pred_dat, models = models, raw_dat = raw_dat,
    species = species, include_depth = include_depth)
}

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
  ...) {

  annotations <- match.arg(annotations)

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
    rotation_center = c(500, 5700), rotation_angle = 40, north_symbol = north_symbol,
    xlim = c(375, 680), ylim = c(5200, 6150), x_buffer = 0, y_buffer = 0,
    north_symbol_coord = c(130, 5975), show_axes = show_axes,
    extrapolate_depth = TRUE, ...
  ) + ggplot2::theme(legend.position = "bottom") +
    guides(fill = FALSE, size = FALSE)

  if (annotations == "SYN")
    g <- g + ggplot2::annotate("text", 390, 6090, label = "2016", col = "grey30") +
      ggplot2::annotate("text", 390, 5800, label = "2017", col = "grey30") +
      ggplot2::annotate("text", 390, 5450, label = "2016", col = "grey30")

  if (annotations == "HBLL")
    g <- g + ggplot2::annotate("text", 390, 5990, label = "2017", col = "grey30") +
    ggplot2::annotate("text", 390, 5550, label = "2016", col = "grey30")

  if (annotations == "IPHC")
    g <- g + ggplot2::annotate("text", 390, 5700, label = "2017", col = "grey30")

  g
}
