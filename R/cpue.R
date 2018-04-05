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
  areas = c("3[CD]+|5[ABCDE]+", "5[AB]+", "5[CDE]+", "3[CD]+")) {

  cpue_models <- lapply(areas, function(area) {
    message("Determining qualified fleet for area ", area, ".")
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

    if (length(unique(fleet$vessel_name)) < 10)
      return(NA)

    pos_catch_fleet <- dplyr::filter(fleet, pos_catch == 1)
    base_month    <- get_most_common_level(pos_catch_fleet$month)
    base_depth    <- get_most_common_level(pos_catch_fleet$depth)
    base_lat      <- get_most_common_level(pos_catch_fleet$latitude)
    base_vessel   <- get_most_common_level(pos_catch_fleet$vessel)
    base_locality <- get_most_common_level(pos_catch_fleet$locality)

    message("Fitting standardization model for area ", area, ".")
    m_cpue <- fit_cpue_index(fleet,
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
    )
    list(model = m_cpue, fleet = fleet, area = gsub("\\[|\\]|\\+", "", area))
  })

  indices_centered <- purrr::map_df(cpue_models, function(x) {
    if (is.na(x[[1]])[[1]]) return()
    p <- predict_cpue_index(x$model, center = TRUE)
    p$area <- x$area
    p
  })

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

  # plot_cpue_facet <- function(dat, scales = "free_y") {
  #   dat %>%
  #     ggplot(aes(year, est, ymin = lwr, ymax = upr, fill = model)) +
  #     geom_ribbon(alpha = 0.3) +
  #     geom_line() +
  #     facet_grid(model~area, scales = scales) +
  #     theme_pbs() +
  #     ylab("Estimate") + xlab("Year") +
  #     guides(fill = FALSE)
  # }
  #
  # plot_cpue_facet(filter(indices_centered, model == "Combined")) +
  #   facet_wrap(~area, scales = "free_y", ncol = 1) +
  #   scale_fill_manual(values = "black")

  jks %>%
    dplyr::filter(term == "Unstandardized") %>%
    dplyr::rename(est_unstandardized = pred) %>%
    dplyr::inner_join(dplyr::filter(indices_centered, model == "Combined"),
      by = c("year", "area"))
}

plot_cpue_indices <- function(dat) {

  labs <- unique(select(dat, area))
  yrs <- range(dat$year, na.rm = TRUE)

  dat %>%
    group_by(area) %>%
    mutate(max_value = max(c(upr, est_unstandardized))) %>%
    mutate(
      est = est / max_value,
      upr = upr / max_value,
      lwr = lwr / max_value,
      est_unstandardized = est_unstandardized / max_value
      ) %>%
    ggplot(aes(year, est, ymin = lwr, ymax = upr)) +
    geom_ribbon(alpha = 0.3, col = NA, fill = "grey60") +
    geom_line(aes(x = year, y = est_unstandardized),
      inherit.aes = FALSE, lty = 2) +
    geom_line() +
    facet_wrap(~area, scales = "free_y", ncol = 1) +
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
      data = labs, x = yrs[1] + 0.5, y = 0.88,
      aes_string(label = "area"),
      inherit.aes = FALSE, colour = "grey30", size = 2.75, hjust = 0
    ) +
    scale_x_continuous(breaks = seq(0, yrs[2], 5))
}
#
# d_cpue_index <- readRDS("report/data-cache/pbs-cpue-index.rds")
# ind <- gfsynopsis::fit_cpue_indices(d_cpue_index, "pacific cod",
#   areas = c("3[CD]+"))
# gfsynopsis::plot_cpue_indices(ind)
