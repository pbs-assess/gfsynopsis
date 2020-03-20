#' Fit commercial catch per unit effort standardization models
#'
#' This function helps it fit the CPUE standardization models across multiple
#' areas for a single species.
#'
#' @param dat A data frame from [gfplot::get_cpue_index()].
#' @param species The species common name to fit.
#' @param areas A vector of regular expressions representing the statistical
#'   areas to fit.
#' @param center Logical for whether or not the index should be centered on its
#'   geometric mean.
#' @param cache A folder in which to cache the model output if desired.
#' @param save_model Logical for whether the model should be cached. Defaults to
#'   `FALSE` to save space.
#' @param arith_cpue_comparison Logical: should the unstandardized comparison be
#'   an arithmetic 'ratio estimator' CPUE (summed catch for this species divided
#'   by summed effort for the entire fleet) (if `TRUE`) or a GLM / GLMM with
#'   only a year predictor.
#' @param parallel Should the various areas be fit in parallel? Make sure you
#'   have enough memory. (Disabled for now.)
#' @param year_range Year range.
#'
#' @import gfplot
#' @importFrom dplyr filter mutate summarise select group_by n arrange ungroup
#' @importFrom dplyr inner_join left_join right_join anti_join full_join
#' @importFrom dplyr semi_join
#' @importFrom dplyr bind_rows case_when pull contains tibble rename as_tibble
#' @importFrom RColorBrewer brewer.pal
#' @importFrom dplyr "%>%"
#' @importFrom foreach "%dopar%" "%do%"
#' @importFrom ggplot2 ggplot aes_string geom_hline geom_vline scale_fill_manual
#'   scale_colour_manual scale_x_continuous scale_size_area coord_cartesian
#'   guides geom_point facet_wrap xlab ylab geom_col ylim xlim geom_rect
#'   geom_text scale_fill_continuous geom_line labs scale_y_continuous
#'   guide_legend geom_ribbon element_text scale_shape_manual element_line
#'   geom_path geom_polygon coord_equal stat_summary_hex facet_grid
#'   position_identity coord_fixed
#' @export

fit_cpue_indices <- function(dat,
  species = "pacific cod",
  areas = c("3[CD]+|5[ABCDE]+", "5[CDE]+", "5[AB]+", "3[CD]+"),
  center = TRUE, cache = here::here("report", "cpue-cache"),
  save_model = FALSE, arith_cpue_comparison = TRUE, parallel = FALSE,
  year_range = c(1996, lubridate::year(Sys.Date()) - 1)) {

  # cores <- if (parallel) parallel::detectCores()[1L] else 1L
  # cl <- parallel::makeCluster(min(c(cores, length(areas))))
  # doParallel::registerDoParallel(cl)
  cpue_models <- foreach::foreach(area = areas,
    .packages = c("gfplot", "gfsynopsis")) %do% {
      message("Determining qualified fleet for area ", area, ".")

      fleet <- gfplot::tidy_cpue_index(dat,
        year_range = year_range,
        species_common = species,
        gear = "bottom trawl",
        use_alt_year = FALSE,
        area_grep_pattern = area,
        min_positive_fe = 100,
        min_positive_trips = 5,
        min_yrs_with_trips = 5,
        lat_band_width = 0.1,
        depth_band_width = 25,
        depth_bin_quantiles = c(0.001, 0.999),
        min_bin_prop = 0.001
      )

      if (!is.data.frame(fleet))
        if (is.na(fleet[[1]]))
          return(NA)
      if (length(unique(fleet$vessel_registration_number)) < 5L)
        return(NA)

      message("Fitting standardization model for area ", area, ".")

      fleet$year_locality <- paste(fleet$year, fleet$locality)

      if (save_model && area == "5[AB]+") # example for report
        saveRDS(fleet, file = file.path(cache, paste0(gsub(" ", "-", species),
          "-", clean_area(area), "-fleet.rds")))

      clean_area <- function(area) gsub("\\^|\\[|\\]|\\+|\\|", "", area)
      model_file <- file.path(cache, paste0(gsub("\\/", "", gsub(" ", "-", species)),
        "-", clean_area(area), "-model.rds"))

      if (!file.exists(model_file)) {
        m_cpue <- try(gfplot::fit_cpue_index_glmmtmb(fleet,
          formula = cpue ~ 0 + year_factor +
            depth +
            month +
            latitude +
            (1 | locality) +
            (1 | vessel) +
            (1 | year_locality),
          verbose = FALSE))
        saveRDS(m_cpue, file = model_file)
      } else {
        m_cpue <- readRDS(model_file)
      }
      if (identical(class(m_cpue), "try-error")) {
        warning("TMB CPUE model for area ", area, " did not converge.")
        return(NA)
      }
      list(model = m_cpue, fleet = fleet, area = clean_area(area))
    }
  # doParallel::stopImplicitCluster()

  indices_centered <- purrr::map_df(cpue_models, function(x) {
    if (is.na(x[[1]])[[1]]) return()
    p <- gfplot::predict_cpue_index_tweedie(x$model, center = center)
    p$area <- x$area
    p
  })
  if (nrow(indices_centered) == 0) # none exist
    return(NA)

  unstand_est <- purrr::map_df(cpue_models, function(x) {
    if (is.na(x[[1]])[[1]]) return()

    if (arith_cpue_comparison) {
      group_by(x$fleet, year_factor) %>%
        summarise(est_unstandardized = sum(spp_catch) / sum(effort)) %>%
        mutate(area = x$area) %>%
        ungroup() %>%
        rename(year = year_factor) %>%
        mutate(year = as.numeric(as.character(year))) %>%
        dplyr::select(year, est_unstandardized, area) %>%
        mutate(est_unstandardized = est_unstandardized /
            exp(mean(log(est_unstandardized))))
    } else {
      fit_yr <- gfplot::fit_cpue_index_glmmtmb(x$fleet,
        formula = cpue ~ 0 + year_factor
      )
      p_yr <- predict_cpue_index_tweedie(fit_yr, center = center)
      p_yr$area <- x$area
      dplyr::rename(p_yr, est_unstandardized = est) %>%
        dplyr::filter(model == "Combined") %>%
        dplyr::select(year, est_unstandardized, area)
    }
  })

  unstand_est %>%
    dplyr::inner_join(dplyr::filter(indices_centered, model == "Combined"),
      by = c("year", "area"))
}

clean_area <- function(area) {
  gsub("\\^|\\[|\\]|\\+|\\|", "", area)
}

#' Plot CPUE indices
#'
#' @param dat The data
#' @param blank_plot Whether or not a blank plot should be generated
#' @param xlim The x limits
#'
#' @export
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
      mutate(area = factor(area, levels = c(clean_area("3CD|5ABCDE"), "5CDE", "5AB", "3CD")))
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
