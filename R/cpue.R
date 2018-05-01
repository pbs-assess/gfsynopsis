#' @import gfplot
#' @importFrom dplyr filter mutate summarise select group_by n arrange ungroup
#' @importFrom dplyr inner_join left_join right_join anti_join full_join
#' @importFrom dplyr semi_join
#' @importFrom dplyr bind_rows case_when pull contains tibble rename as_tibble
#' @importFrom RColorBrewer brewer.pal
#' @importFrom dplyr "%>%"
#' @importFrom ggplot2 ggplot aes_string geom_hline geom_vline scale_fill_manual
#'   scale_colour_manual scale_x_continuous scale_size_area coord_cartesian
#'   guides geom_point facet_wrap xlab ylab geom_col ylim xlim geom_rect
#'   geom_text scale_fill_continuous geom_line labs scale_y_continuous
#'   guide_legend geom_ribbon element_text scale_shape_manual element_line
#'   geom_path geom_polygon coord_equal stat_summary_hex facet_grid
#'   position_identity coord_fixed

fit_cpue_indices <- function(dat,
  species = "pacific cod",
  areas = c("3[CD]+|5[ABCDE]+", "5[CDE]+", "5[AB]+", "3[CD]+"), ...) {

  cpue_models <- lapply(areas, function(area) {
    message("Determining qualified fleet for area ", area, ".")

    if (species == "quillback rockfish") # TODO CONVERGENCE ISSUES
      return(NA)

    fleet <- tidy_cpue_index(dat,
      year_range = c(1996, 2017),
      species_common = species,
      area_grep_pattern = area,
      min_positive_tows = 100,
      min_positive_trips = 4,
      min_yrs_with_trips = 4,
      lat_band_width = 0.2,
      depth_band_width = 50,
      clean_bins = TRUE,
      depth_bin_quantiles = c(0.02, 0.98),
      lat_bin_quantiles = c(0.01, 0.99)
    )

    if (!is.data.frame(fleet))
      if (is.na(fleet[[1]]))
        return(NA)
    if (length(unique(fleet$vessel_name)) < 10)
      return(NA)

    pos_catch_fleet <- dplyr::filter(fleet, pos_catch == 1)
    base_month    <- get_most_common_level(pos_catch_fleet$month)
    base_depth    <- get_most_common_level(pos_catch_fleet$depth)
    base_lat      <- get_most_common_level(pos_catch_fleet$latitude)
    base_vessel   <- get_most_common_level(pos_catch_fleet$vessel)
    base_locality <- get_most_common_level(pos_catch_fleet$locality)

    message("Fitting standardization model for area ", area, ".")

    invisible(capture.output(
      m_cpue <- try(fit_cpue_index(fleet,
        formula_binomial = pos_catch ~ year_factor +
          f(month, base_month) +
          f(vessel, base_vessel) +
          f(locality, base_locality) +
          f(depth, base_depth) +
          f(latitude, base_lat),
        formula_lognormal = log(spp_catch / hours_fished) ~
          year_factor +
          f(month, base_month) +
          f(vessel, base_vessel) +
          f(locality, base_locality) +
          f(depth, base_depth) +
          f(latitude, base_lat)
      ), silent = TRUE)
    ))

    if (identical(class(m_cpue), "try-error")) {
      warning("TMB CPUE model for area ", area, " didn't converge.")
      return(NA)
    }
    list(model = m_cpue, fleet = fleet, area = gsub("\\[|\\]|\\+", "", area))
  })

  indices_centered <- purrr::map_df(cpue_models, function(x) {
    if (is.na(x[[1]])[[1]]) return()
    p <- predict_cpue_index(x$model, center = TRUE)
    p$area <- x$area
    p
  })
  if (nrow(indices_centered) == 0) # none exist
    return(NA)

  # coef_plots <- lapply(cpue_models, function(x) {
  # if (is.na(x[[1]])[[1]]) return()
  #   plot_cpue_index_coefs(x$model) + labs(title = x$area)
  # })
  # ignore <- lapply(coef_plots, print)

  jks <- purrr::map_df(cpue_models, function(x) {
    if (is.na(x[[1]])[[1]]) return()
    out <- plot_cpue_index_jk(x$model, terms = NULL, return_data = TRUE)
    out$area <- x$area
    out
  })

  jks %>%
    dplyr::filter(term == "Unstandardized") %>%
    dplyr::rename(est_unstandardized = pred) %>%
    dplyr::inner_join(dplyr::filter(indices_centered, model == "Combined"),
      by = c("year", "area"))
}

plot_cpue_indices <- function(dat, blank_plot = FALSE, xlim = c(1996, 2017)) {

  yrs <- xlim

  if (!blank_plot) {
    dat <- dat %>%
      group_by(area) %>%
      mutate(max_value = max(c(upr, est_unstandardized))) %>%
      mutate(
        est = est / max_value,
        upr = upr / max_value,
        lwr = lwr / max_value,
        est_unstandardized = est_unstandardized / max_value
      ) %>%
      arrange(area) %>%
      ungroup() %>%
      mutate(area = factor(area, levels = c("3CD|5ABCDE", "5CDE", "5AB", "3CD")))
  }
  labs <- tibble(area = factor(levels(dat$area), levels = levels(dat$area)))

  g <- ggplot(dat, aes_string("year", "est", ymin = "lwr", ymax = "upr"))

  g <- g +
    geom_vline(xintercept = seq(yrs[1], yrs[2]), col = "grey98") +
    geom_vline(xintercept = seq(gfplot:::mround(yrs[1], 5), yrs[2], 5),
      col = "grey95") +
    facet_wrap(~area, scales = "free_y", ncol = 1, drop = FALSE) +
    ylab("Estimate") + xlab("Year") +
    guides(fill = FALSE) +
    theme_pbs() +
    theme(panel.spacing = unit(-0.1, "lines")) +
    ylim(0, 1.03) +
    coord_cartesian(expand = FALSE, xlim = yrs + c(-0.5, 0.5)) +
    theme(
      axis.text.y = element_text(colour = "white"),
      axis.ticks.y = element_line(colour = "white"),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    ) +
    guides(fill = FALSE, colour = FALSE) +
    geom_text(
      data = labs, x = yrs[1] + 0.5, y = 0.92,
      aes_string(label = "area"),
      inherit.aes = FALSE, colour = "grey30", size = 3, hjust = 0
    ) +
    scale_x_continuous(breaks = seq(0, yrs[2], 5))

  if (!blank_plot) {
    g <- g + geom_ribbon(alpha = 0.3, col = NA, fill = "grey60") +
      geom_line(aes_string(x = "year", y = "est_unstandardized"),
        inherit.aes = FALSE, lty = 2) +
      geom_line()
  }

  g
}
#
# d_cpue_index <- readRDS("report/data-cache/pbs-cpue-index.rds")
# ind <- gfsynopsis::fit_cpue_indices(d_cpue_index, "pacific cod",
#   areas = c("3[CD]+"))
# gfsynopsis::plot_cpue_indices(ind)

# library(mgcv)
#
# fleet <- tidy_cpue_index(d,
#     year_range = c(1996, 2017),
#     species_common = "pacific cod",
#     area_grep_pattern = "3[CD]+",
#     min_positive_tows = 100,
#     min_positive_trips = 4,
#     min_yrs_with_trips = 4,
#     lat_band_width = 0.02,
#     depth_band_width = 1,
#     clean_bins = TRUE,
#     depth_bin_quantiles = c(0.01, 0.99),
#     lat_bin_quantiles = c(0.01, 0.99)
# )
#
#   pos_catch_fleet <- dplyr::filter(fleet, pos_catch == 1)
#   base_month    <- get_most_common_level(pos_catch_fleet$month)
#   base_depth    <- get_most_common_level(pos_catch_fleet$depth)
#   base_lat      <- get_most_common_level(pos_catch_fleet$latitude)
#   base_vessel   <- get_most_common_level(pos_catch_fleet$vessel)
#   base_locality <- get_most_common_level(pos_catch_fleet$locality)
#
#   fleet$latitude <- as.numeric(as.character(fleet$latitude))
#   fleet$depth <- as.numeric(as.character(fleet$depth))
#   fleet$month <- as.numeric(as.character(fleet$month))
#
# m <- gam(spp_catch/hours_fished ~ year_factor + f(vessel) +
#     s(latitude) + s(depth) + s(month, k = 12, bs = "cc") + f(locality),
#   data = fleet, family = tw(link = "log"))
#
# # library(gbm)
# library(randomForest)
# mb <- randomForest(log(spp_catch/hours_fished) ~ year_factor + vessel +
#     latitude + depth + month + locality,
#   data = dplyr::filter(fleet, pos_catch == 1))
#
# nd <- data.frame(year_factor = unique(fleet$year_factor),
#   vessel = base_vessel, latitude = mean(fleet$latitude),
#   depth = mean(fleet$depth), month = as.numeric(as.character(base_month)),
#   locality = base_locality)
#
# nd <- data.frame(year_factor = unique(fleet$year_factor),
#   vessel = base_vessel, latitude = as.numeric(as.character(base_lat)),
#   depth = as.numeric(as.character(base_depth)), month = as.numeric(as.character(base_month)),
#   locality = base_locality)
#
# p <- predict(m, newdata = nd, se.fit = TRUE, type = "link")
#
# p$fit <- p$fit - mean(p$fit)
#
# pp <- data.frame(est = exp(p$fit),
#   lwr = exp(p$fit - 2 * p$se.fit),
#   upr = exp(p$fit + 2 * p$se.fit),
#   year = as.numeric(as.character(nd$year_factor))
# )
#
# tm <- gfsynopsis::fit_cpue_indices(dat = d, species = "pacific cod",
#   areas = "3[CD]+")
#
# library(ggplot2)
# g <- ggplot(pp, aes(year, est, ymin = lwr, ymax = upr)) +
#   geom_ribbon(colour = NA, alpha = 0.5) +
#   geom_line()
#
# g <- g + geom_line(data = tm, colour = "blue") +
#   geom_ribbon(data = tm, colour = NA, fill = "blue", alpha = 0.5)
#
# g <- g + geom_line(data = tm, aes(y = est_unstandardized), colour = "red")
# g
