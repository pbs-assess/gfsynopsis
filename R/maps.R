#' Fit geostatistical survey models and make maps
#'
#' `fit_survey_maps()` is a helper function that calls
#' [gfplot::fit_survey_sets()] appropriately for the various surveys.
#'
#' @param dat Data from [gfdata::get_survey_sets()].
#' @param species The species name.
#' @param include_depth Logical for whether or not to include depth and depth
#'   squared as predictors.
#' @param model Whether to fit an INLA or glmmfields model.
#' @param surveys Character vector describing the surveys to fit. Can include
#'   multiple surveys if desired.
#' @param years The years of survey data to use.
#' @param family Family
#' @param ... Any other arguments to pass to [gfplot::fit_survey_sets()].
#'
#' @export
fit_survey_maps <- function(dat,
  species = "pacific cod", include_depth = TRUE, family = sdmTMB::tweedie(),
  surveys = c("SYN QCS", "SYN HS", "SYN WCHG", "SYN WCVI"),
  years = c(2017, 2018),
  ...) {
  dat <- dplyr::filter(dat, species_common_name %in% species)
  dat <- dplyr::filter(dat, year %in% years)
  # grab last year of data only:
  dat <- dplyr::group_by(dat, survey_abbrev) %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::ungroup()

  dat <- drop_duplicated_fe(dat) #150

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
      last_year <- max(.dat$year)
      raw_dat <- tidy_survey_sets(.dat, surv,
        years = last_year, density_column = density_column
      )
    }
    if (surv == "IPHC FISS") {
      density_column <- "density_ppkm2"
      # .dat <- filter(dat, year %in% years[2]) # just last year
      .dat <- filter(dat, year %in% years)
      .dat <- filter(.dat, survey_abbrev %in% surv)
      last_year <- max(.dat$year)
      if (nrow(.dat) == 0L) stop("No survey data.")
      raw_dat <- tidy_survey_sets(.dat, surv,
        years = last_year, density_column = density_column
      )
      premade_grid <- filter(raw_dat, year == max(year)) %>%
        select(lon, lat, depth) %>%
        rename(X = lon, Y = lat)
      premade_grid <- list(grid = premade_grid, cell_area = 1.0)
    }
    if (surv %in% c("SYN QCS", "SYN HS", "SYN WCHG", "SYN WCVI")) {
      density_column <- "density_kgpm2"
      .dat <- filter(dat, survey_abbrev %in% surv)
      # premade_grid <- NULL
      premade_grid <- dplyr::filter(gfplot::synoptic_grid, survey %in% surv)
      premade_grid <- list(grid = premade_grid, cell_area = 4.0)
      last_year <- max(.dat$year)
      raw_dat <- tidy_survey_sets(.dat, surv,
        years = last_year, density_column = "density_kgpm2"
      )
    }
    m <- fit_survey_sets(.dat, survey = surv, years = max(.dat$year),
      density_column = density_column, tmb_knots = 200, family = family,
      premade_grid = premade_grid, required_obs_percent = 0.02, ...)
    # we may have predicted all years, but just save last year for plotting:
    raw_dat <- dplyr::filter(m$data, year == max(m$data$year))
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
#' [plot_survey_sets_synopsis()] appropriately for the various surveys.
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
#' @param syn_wcvi_year Year for WCVI synoptic survey label.
#' @param syn_wchg_year Year for WCHG synoptic survey label.
#' @param syn_qcs_hs_year Year for a synoptic survey label.
#' @param hbll_n_year Year for a HBLL N survey label.
#' @param hbll_s_year Year for a HBLL S survey label.
#' @param iphc_year Year for IPHC survey label.
#' @param ... Any other arguments to pass to [plot_survey_sets_synopsis()].
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
  syn_wcvi_year = 2018,
  syn_wchg_year = 2018,
  hbll_n_year = 2017,
  hbll_s_year = 2018,
  iphc_year = 2018,
  shapefile = NULL,
  ...) {

  annotations <- match.arg(annotations)

  # Manually avoid extrapolating within each survey:
  raw_dat_depth_ranges <- group_by(raw_dat, survey) %>%
    summarise(min_raw_depth = min(depth, na.rm = TRUE),
      max_raw_depth = max(depth, na.rm = TRUE)) %>%
    mutate(
      min_raw_depth = ifelse(!is.na(min_raw_depth), min_raw_depth, Inf),
      max_raw_depth = ifelse(!is.na(max_raw_depth), max_raw_depth, -Inf))

  if (nrow(pred_dat) > 1L && annotations != "IPHC") {
    # Set to NA predictions outside of the range within that survey and year:
    pred_dat <- left_join(pred_dat, raw_dat_depth_ranges)
    pred_dat[pred_dat$akima_depth < pred_dat$min_raw_depth |
        pred_dat$akima_depth > pred_dat$max_raw_depth, fill_column] <- NA
    .q <- quantile(pred_dat$combined, probs = 0.998, na.rm = TRUE)[[1]]
    pred_dat$combined[pred_dat$combined > .q] <- .q
  }

  g <- plot_survey_sets_synopsis(pred_dat, raw_dat,
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
    shapefile = shapefile,
    extrapolate_depth = TRUE, ...
  ) + ggplot2::theme(legend.position = "bottom") +
    guides(fill = "none", size = "none")

  if (annotations == "SYN")
    g <- g + ggplot2::annotate("text", 390, 6090, label = syn_wchg_year, col = "grey30") +
      ggplot2::annotate("text", 390, 5800, label = syn_qcs_hs_year, col = "grey30") +
      ggplot2::annotate("text", 390, 5450, label = syn_wcvi_year, col = "grey30")

  if (annotations == "HBLL")
    g <- g + ggplot2::annotate("text", 390, 5990, label = hbll_n_year, col = "grey30") +
    ggplot2::annotate("text", 390, 5550, label = hbll_s_year, col = "grey30")

  if (annotations == "IPHC")
    g <- g + ggplot2::annotate("text", 390, 5700, label = iphc_year, col = "grey30")

  g
}

#' Plot the output from a geostatistical model of survey data
#'
#' Takes the output from [fit_survey_sets()] and creates a map of the model
#' predictions and/or the raw data. Includes a number of options for customizing
#' the map including the ability to rotate the map.
#'
#' @param pred_dat The `predictions` element of the output from
#'   [fit_survey_sets()].
#' @param raw_dat The `data` element of the output from [fit_survey_sets()].
#' @param fill_column The name of the column to plot. Options are `"combined"`
#'   for the combined model, `"bin"` for the binary component model, or `"pos"`
#'   for the positive component model.
#' @param fill_scale A ggplot `scale_fill_*` object.
#' @param colour_scale A ggplot `scale_colour_*` object. You likely want this to
#'   match `fill_scale` unless you want the map to look strange.
#' @param pos_pt_col The color for positive set location points.
#' @param bin_pt_col The color for binary set location points.
#' @param pos_pt_fill The fill color for positive set location points.
#' @param pt_size_range The range of point sizes for positive set location
#'   points.
#' @param show_legend Logical for whether or not to show the legend.
#' @param extrapolate_depth Logical for whether or not to show predictions
#'   across all depths in the survey domain (the default) or to not extrapolate
#'   beyond the range of the observed sets in the data set.
#' @param extrapolation_buffer A buffer to add to the minimum and maximum
#'   observed depths if `extrapolate_depth = TRUE`.
#' @param show_model_predictions Logical for whether or not to show the
#'   geostatistical model predictions.
#' @param show_raw_data Logical for whether or not to show the raw data.
#' @param utm_zone The UTM zone to plot in. Should match the zone used in
#'   [fit_survey_sets()].
#' @param fill_label A label to use in the legend for the fill color.
#' @param pt_label A label to use in the legend for the point size.
#' @param rotation_angle An angle to rotate the entire map. Can be useful to
#'   make a map of the BC coast take up less. Defaults to not rotating the map.
#'   The groundfish synopsis report uses `rotation_angle = 40`.
#' @param rotation_center The coordinates around which to rotate the mouth.
#'   These should be in UTM coordinates.
#' @param show_axes Logical for whether or not to show the axes.
#' @param xlim X axis limits in UTM coordinates. The synopsis report uses
#'   `c(360, 653)`. Defaults to the range of the data.
#' @param ylim Y axis limits in UTM coordinates. The synopsis report uses
#'   `c(5275, 6155)`. Defaults to the range of the data.
#' @param x_buffer A buffer in UTM coordinates to extend the X axis. Mostly
#'   useful if the axis limits aren't explicitly specified.
#' @param y_buffer A buffer in UTM coordinates to extend the Y axis. Mostly
#'   useful if the axis limits aren't explicitly specified.
#' @param north_symbol Logical for whether to include a north symbol.
#' @param north_symbol_coord Coordinates for the north symbol in UTM
#'   coordinates.
#' @param north_symbol_length Length of the north assemble arrow.
#' @param cell_size The size of the grid cells for the model predictions.
#' @param circles Logical for whether to plot the model predictions in circles.
#'   This analysis report uses this for the IPHC survey.
#' @param french Logical for French or English.
#'
#' @return
#' A ggplot object.
#'
#' @export
#' @family spatial survey modelling functions
#' @examples
#' \dontrun{
#' set.seed(123)
#' # pop_surv <- gfdata::get_survey_sets("pacific ocean perch")
#' # or use built-in data:
#' fit <- fit_survey_sets(pop_surv,
#'   years = 2015,
#'   survey = "SYN QCS")
#'
#' # The combined model:
#' plot_survey_sets_synopsis(fit$predictions, fit$data, fill_column = "combined")
#' # The positive component model:
#' plot_survey_sets_synopsis(fit$predictions, fit$data, fill_column = "pos")
#' # Add a custom color scale for the binary model:
#' plot_survey_sets_synopsis(fit$predictions, fit$data, fill_column = "bin") +
#'   ggplot2::scale_fill_gradient2(midpoint = 0.5,
#'     high = scales::muted("red"),
#'     mid = "white",
#'     low = scales::muted("blue"), limits = c(0, 1), breaks = c(0, 0.5, 1)) +
#'   ggplot2::scale_colour_gradient2(midpoint = 0.5,
#'     high = scales::muted("red"),
#'     mid = "white",
#'     low = scales::muted("blue"), limits = c(0, 1))
#' }

plot_survey_sets_synopsis <- function(pred_dat, raw_dat, fill_column = c("combined", "bin", "pos"),
                             fill_scale =
                               ggplot2::scale_fill_viridis_c(trans = "sqrt", option = "C"),
                             colour_scale =
                               ggplot2::scale_colour_viridis_c(trans = "sqrt", option = "C"),
                             pos_pt_col = "#FFFFFF60",
                             bin_pt_col = "#FFFFFF40",
                             pos_pt_fill = "#FFFFFF05",
                             pt_size_range = c(0.5, 9),
                             show_legend = TRUE,
                             extrapolate_depth = TRUE,
                             extrapolation_buffer = 0,
                             show_model_predictions = TRUE,
                             show_raw_data = TRUE,
                             utm_zone = 9,
                             fill_label = "Predicted\nbiomass\ndensity (kg/m^2)",
                             pt_label = "Tow density (kg/km^2)",
                             rotation_angle = 0,
                             rotation_center = c(500, 5700),
                             show_axes = TRUE,
                             xlim = NULL,
                             ylim = NULL,
                             x_buffer = c(-5, 5),
                             y_buffer = c(-5, 5),
                             north_symbol = FALSE,
                             north_symbol_coord = c(130, 5975),
                             north_symbol_length = 30,
                             cell_size = 2, circles = FALSE,
                             shapefile = NULL,
                             french = FALSE) {
  fill_column <- match.arg(fill_column)
  if (!extrapolate_depth) {
    pred_dat <- filter(
      pred_dat,
      akima_depth >= min(raw_dat$depth, na.rm = TRUE) - extrapolation_buffer,
      akima_depth <= max(raw_dat$depth, na.rm = TRUE) + extrapolation_buffer,
      akima_depth > 0
    )
  }

  pred_dat$id <- NA # for circles
  if (show_model_predictions && !circles) {
    # turn grid into explicit rectangles for possible rotation:
    pred_dat <- lapply(seq_len(nrow(pred_dat)), function(i) {
      row_dat <- pred_dat[i, , drop = FALSE]
      X <- row_dat$X
      Y <- row_dat$Y
      data.frame(
        X = c(
          X - cell_size / 2, X + cell_size / 2,
          X + cell_size / 2, X - cell_size / 2
        ),
        Y = c(
          Y - cell_size / 2, Y - cell_size / 2,
          Y + cell_size / 2, Y + cell_size / 2
        ),
        combined = row_dat$combined,
        bin = row_dat$bin,
        pos = row_dat$pos,
        # year = row_dat$year,
        id = i
      )
    }) %>% bind_rows()
  }

  if (north_symbol) {
    north <- data.frame(
      X = c(north_symbol_coord[1], north_symbol_coord[1]),
      Y = c(north_symbol_coord[2], north_symbol_coord[2] + north_symbol_length)
    )
    north_lab_coord <- c(north$X[1], north$Y[1] - 15)

    north <- rotate_df(north, rotation_angle, rotation_center)

    north_sym <- data.frame(
      X = north$X[1],
      Xend = north$X[2],
      Y = north$Y[1],
      Yend = north$Y[2]
    )

    r <- rotate_coords(north_lab_coord[1], north_lab_coord[2],
      rotation_angle = rotation_angle,
      rotation_center = rotation_center
    )
    north_lab_coord <- c(r$x, r$y)
  }

  coast <- load_coastline(range(raw_dat$lon) + c(-1, 1),
    range(raw_dat$lat) + c(-1, 1),
    utm_zone = utm_zone
  )
  coast <- rotate_df(coast, rotation_angle, rotation_center)

  isobath <- load_isobath(range(raw_dat$lon) + c(-5, 5),
    range(raw_dat$lat) + c(-5, 5),
    bath = c(100, 200, 500), utm_zone = 9
  )
  isobath <- rotate_df(isobath, rotation_angle, rotation_center)

  pred_dat <- rotate_df(pred_dat, rotation_angle, rotation_center)
  raw_dat <- rotate_df(raw_dat, rotation_angle, rotation_center)

  if (!is.null(shapefile)) {
    # optional shape file to highlight:
    coords <- sf::st_coordinates(shapefile) |>
      as.data.frame() |>
      dplyr::mutate(X = X / 1000, Y = Y / 1000)
    polyset <- data.frame(
      PID = 1,              # Polygon ID
      POS = seq_len(nrow(coords)), # Point sequence
      X = coords[, 1],      # X-coordinates (longitude)
      Y = coords[, 2]       # Y-coordinates (latitude)
    )
    shape_rotated <- rotate_df(polyset, rotation_angle, rotation_center)
  }

  if (is.null(xlim) || is.null(ylim)) {
    xlim <- range(raw_dat$X) + x_buffer
    ylim <- range(raw_dat$Y) + y_buffer
  }

  g <- ggplot()

  if (show_model_predictions && !circles) {
    g <- g + ggplot2::geom_polygon(
      data = pred_dat, aes_string("X", "Y",
        fill = fill_column,
        colour = fill_column, group = "id"
      )
    ) +
      fill_scale + colour_scale
  }
  if (show_raw_data) {
    g <- g +
      geom_point(
        data = filter(raw_dat, present == 0),
        aes_string(x = "X", y = "Y"),
        col = if (show_model_predictions) bin_pt_col else "grey50",
        pch = 4, size = 1.55
      ) +
      geom_point(
        data = filter(raw_dat, present == 1),
        aes_string(
          x = "X", y = "Y",
          size = "density * 1e6"
        ), fill = pos_pt_fill,
        col = if (show_model_predictions) pos_pt_col else "grey30", pch = 21
      )
  }

  g <- g +
    ggplot2::scale_size_continuous(range = pt_size_range) +
    theme_pbs() +
    coord_equal(xlim = xlim, ylim = ylim) +
    guides(
      shape = ggplot2::guide_legend(override.aes = list(colour = "grey30")),
      size = ggplot2::guide_legend(override.aes = list(colour = "grey30"))
    ) +
    geom_polygon(
      data = coast, aes_string(x = "X", y = "Y", group = "PID"),
      fill = "grey87", col = "grey70", lwd = 0.2
    ) +
    guides(shape = "none", colour = "none") +
    labs(size = pt_label, fill = fill_label) +
    ylab(en2fr("Northing", translate = french)) +
    xlab(en2fr("Easting", translate = french))

  if (!show_legend) {
    g <- g + theme(legend.position = "none")
  }

  if (!show_axes) {
    g <- g + theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  }

  suppressWarnings({
    suppressMessages({
      g <- g + geom_path(
        data = isobath, aes_string(
          x = "X", y = "Y",
          group = "paste(PID, SID)"
        ),
        inherit.aes = FALSE, lwd = 0.4, col = "grey70", alpha = 0.4
      )})})

  # plot circles on top of land for inlets:
  if (show_model_predictions && circles) {
    g <- g + ggplot2::geom_point(
      data = pred_dat, aes_string("X", "Y",
        fill = fill_column, colour = fill_column, group = "id"
      ), size = cell_size, pch = 21
    ) +
      fill_scale + colour_scale
  }

  if (!is.null(shapefile)) {
    g <- g + geom_polygon(mapping = aes_string(x = "X", y = "Y", group = "PID"),
      fill = NA, colour = "black", data = shape_rotated, lwd = 0.7)
  }

  if (north_symbol) {
    g <- g + ggplot2::geom_segment(
      data = north_sym,
      aes_string(x = "X", y = "Y", xend = "Xend", yend = "Yend"),
      inherit.aes = FALSE, colour = "grey30", lwd = 0.8,
      arrow = ggplot2::arrow(length = unit(0.7, "char"))
    )
    g <- g + ggplot2::annotate("text",
      label = "N", colour = "grey30",
      x = north_lab_coord[1], y = north_lab_coord[2]
    )
  }

  g
}

utm2ll <- function(x, utm_zone = 9) {
  attr(x, "projection") <- "UTM"
  attr(x, "zone") <- utm_zone
  suppressMessages(PBSmapping::convUL(x))
}

ll2utm <- function(x, utm_zone = 9) {
  attr(x, "projection") <- "LL"
  attr(x, "zone") <- utm_zone
  suppressMessages(PBSmapping::convUL(x))
}

load_coastline <- function(xlim_ll, ylim_ll, utm_zone, buffer = 2) {
  data("nepacLLhigh", package = "PBSmapping", envir = environment())
  np <- PBSmapping::clipPolys(nepacLLhigh,
    xlim = xlim_ll + c(-buffer, buffer),
    ylim = ylim_ll + c(-buffer, buffer)
  )
  ll2utm(np, utm_zone = utm_zone)
}

load_isobath <- function(xlim_ll, ylim_ll, bath, utm_zone) {
  data("isobath", package = "PBSdata", envir = environment())
  isobath <- filter(isobath, .data$PID %in% bath)
  isobath <- PBSmapping::clipPolys(isobath,
    xlim = xlim_ll + c(-3, 3),
    ylim = ylim_ll + c(-3, 3)
  )
  ll2utm(isobath, utm_zone = utm_zone)
}

load_boundaries <- function(utm_zone = 9) {
  # library(PBSmapping)
  data("major", package = "PBSdata", envir = environment())
  class(major) <- "data.frame" # this seems to prevent needing to library(PBSmapping)
  ll2utm(major, utm_zone = utm_zone)
}

boundary_labels <- function(utm_zone = 9, xmin = NULL){
  # library(PBSmapping)
  data("major", package = "PBSdata", envir = environment())

  labels <- attributes(major)$PolyData
  class(labels) <- "data.frame" # this seems to prevent needing to library(PBSmapping)
  labels <-  ll2utm(labels, utm_zone = utm_zone)
  labels[labels$label %in% c("4B", "5C", "5D"),]$X <- c(885, 445, 340)
  labels[labels$label %in% c("4B", "5C", "5D"),]$Y <- c(5475, 5840, 5970)
  if(!is.null(xmin)){labels[!(labels$label %in% c("4B", "5C", "5D")),]$X <- xmin + 50}
  labels
}

# Rotate coords
#
# @param x X coordinates.
# @param y Y coordinates.
# @param rotation_angle The rotation angle.
# @param rotation_center The coordinates about which to rotate.
#
# @examples
# x <- c(1:100, rep(100, 100), 100:1, rep(1, 100))
# y <- c(rep(1, 100), 1:100, rep(100, 100), 100:1)
# plot(x, y, asp = 1)
# points(50, 50, col = "red")
# z <- rotate_coords(x = x, y = y, rotation_angle = 24,
#   rotation_center = c(50, 50))
# plot(z$x, z$y, asp = 1)
# points(50, 50, col = "red")
rotate_coords <- function(x, y, rotation_angle, rotation_center) {
  assertthat::assert_that(identical(class(rotation_center), "numeric"))
  assertthat::assert_that(identical(class(rotation_angle), "numeric"))
  assertthat::assert_that(identical(length(rotation_center), 2L))
  assertthat::assert_that(identical(length(rotation_angle), 1L))
  assertthat::assert_that(identical(length(x), length(y)))

  rot <- -rotation_angle * pi / 180
  newangles <- atan2(y - rotation_center[2], x - rotation_center[1]) + rot
  mags <- sqrt((x - rotation_center[1])^2 + (y - rotation_center[2])^2)
  x <- rotation_center[1] + cos(newangles) * mags
  y <- rotation_center[2] + sin(newangles) * mags
  dplyr::tibble(x = x, y = y)
}

rotate_df <- function(df, rotation_angle, rotation_center) {
  r <- rotate_coords(df$X, df$Y,
    rotation_angle = rotation_angle,
    rotation_center = rotation_center
  )
  df$X <- r$x
  df$Y <- r$y
  df
}
