#' Plot locality specific estimates from a CPUE standardization model
#'
#' @param model Output from [gfplot::fit_cpue_index_glmmtmb()].
#' @param fleet The original data object. Output from
#'   [gfplot::tidy_cpue_historical()] or [gfplot::tidy_cpue_index()].
#' @param index_data Standardized index data. Output from
#'   [gfplot::predict_cpue_index_tweedie()].
#' @param era Modern or historical.
#'
#' @export
plot_cpue_spaghetti <- function(model, fleet, index_data,
  era = c("modern", "historical")) {

  era <- match.arg(era)

  newdata <- select(fleet, year_factor, locality) %>%
    unique() %>%
    mutate(year = as.numeric(as.character(year_factor))) %>%
    group_by(locality) %>%
    dplyr::do({
      dplyr::data_frame(year = seq(min(.$year), max(.$year)))
    }) %>%
    group_by(locality) %>%
    mutate(
      depth = basel_level(fleet$depth),
      month = basel_level(fleet$month)
    )
  if (era == "modern") {
    newdata <- newdata %>% mutate(
      latitude = basel_level(fleet$latitude),
      vessel = basel_level(fleet$vessel))
  }

  newdata <- ungroup(newdata)
  newdata <- newdata %>% mutate(year_locality = paste(year, locality))
  newdata <- newdata %>% mutate(year_factor = as.character(year))
  newdata$cpue <- NA
  newdata <- newdata[ ,names(model$frame)]

  pp <- stats::predict(model, newdata = newdata, allow.new.levels = TRUE)

  newdata$loc_pred <- pp
  newdata <- rename(newdata, year = year_factor) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    arrange(year, locality)

  stand <- select(index_data, year, est_link, se_link)

  ggplot(newdata, aes_string('year', 'loc_pred')) +
    geom_line(alpha = 0.7, aes_string(group = 'locality', colour = 'locality')) +
    geom_line(data = stand, aes_string(x = 'year', 'est_link'),
      inherit.aes = FALSE, lwd = 1) +
    ggplot2::geom_ribbon(data = stand,
      aes_string(x = 'year', ymin = 'est_link - 1.96 * se_link',
        ymax = 'est_link + 1.96 * se_link'), inherit.aes = FALSE, alpha = 0.4) +
    ggplot2::guides(colour = FALSE) +
    ggplot2::scale_color_discrete() +
    ylab(en2fr("Log of standardized CPUE", french)) + xlab("")
}

basel_level <- function(x) levels(x)[[1]]
