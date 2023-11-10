#' Get pstar - Estimate Breakdown Point of Catch Counts
#'
#' Fits a Generalized Additive Model (GAM) and calculates the breakdown point
#' (pstar) of observed catch counts due to hook competition.
#'
#' @param survey_dat A dataframe from [gfsynopsis::prep_stitch_dat()] or
#' [gfsynopsis::prep_iphc_stitch_dat()].
#' @param gam_formula A string or formula specifying the generalised additive
#' used to estimate pstar.
#' @param survey_type A string specifying the survey type: "iphc",
#' "hbll_outside", or "hbll_inside".
#' @param prop_removed_min Optional. Minimum value of proportion of baits removed
#' for generating predictions. If not provided, uses the minimum value found in
#' `survey_dat`.
#' @param h Step size for the central difference approximation (default = 0.005).
#' @param pstar_cache Path to a folder for caching pstar objects.
#' @param save_out Whether to save the pstar object to `pstar_cache`
#' (default = TRUE).
#'
#' @returns A list of objects containing
#'  - `gam_fit`: The fitted GAM object.
#'  - `pred_df`: A data frame with predicted values from `gam_fit`
#'  - `f1`: A data frame containing the numerical approximation of the first derivative
#'  - `pstar_df`: A data frame with the estimated pstar value.
#'  - `species`: A character vector containing the species name.
#' @export
#'
get_pstar <- function(
    survey_dat, gam_formula, survey_type,
    prop_removed_min = NULL, h = 0.005, pstar_cache = NULL, save_out = TRUE) {
  species <- unique(survey_dat[["species_common_name"]])

  if (!inherits(gam_formula, "gam")) gam_formula <- formula(gam_formula)

  message("\tFitting GAM for ", species)
  gam_fit <- try(
    mgcv::gam(formula = gam_formula, family = "nb", data = survey_dat)
  )

  # Choose minimum proportion of hooks removed to predict over
  if (is.null(prop_removed_min)) {
    prop_removed_min <- floor(min(survey_dat$prop_removed) * 100) / 100
    assertthat::assert_that(!is.na(prop_removed_min))
  }

  # If the GAM fit, get GAM predictions based on survey type
  survey <- ifelse(survey_type == "iphc", "iphc", "hbll")
  if (inherits(gam_fit, "gam")) {
    pred_df <- switch(survey,
      hbll = expand_grid(
        prop_removed = seq(prop_removed_min, 1, h),
        fyear = survey_dat$fyear[[1]],
        hook_count = 1L,
        species = species
      ),
      iphc = expand_grid(
        prop_removed = seq(prop_removed_min, 1, h),
        log_eff_skate = 0L,
        fyear = survey_dat$fyear[[1]],
        fstation = survey_dat$fstation[[1]]
      ),
    )

    pred_mod <- mgcv::predict.gam(
      object = gam_fit, newdata = pred_df, se.fit = TRUE,
      type = "terms", terms = "s(prop_removed)"
    )

    # Calculate first derivative
    message("\tGetting f' for ", species)
    f1 <- get_fderiv(
      gam_object = gam_fit, newdata = pred_df,
      terms = "prop_removed", term_columns = "prop_removed", x = "prop_removed",
      h = h, nsim = 100
    )

    pred_df$fit <- pred_mod[[1]][, 1]
    pred_df$se <- pred_mod[[2]][, 1]
    pred_df <- pred_df |>
      mutate(lwr = fit - 1.96 * se, upr = fit + 1.96 * se, species = species)

    # Use f' to get pstar
    pstar_df <- f1 |>
      arrange(desc(prop_removed)) |>
      mutate(
        sign_change = ifelse(lag(derivative) * derivative > 0, "no", "yes"),
        slope_xh = ifelse(lag(derivative) > 0, "positive", "negative")
      ) |>
      filter(sign_change == "yes" & slope_xh == "negative") |>
      select(prop_removed, derivative, lower, upper, sign_change, slope_xh) |>
      rename(pstar = "prop_removed") |>
      slice(1)

    # This combination of objects is useful for examining pstar outputs
    out <- list(
      gam_fit = gam_fit, pred_df = pred_df, f1 = f1,
      pstar_df = pstar_df, species = species
    )
  } else {
    NA
  }

  if (save_out) {
    saveRDS(out, file.path(
      pstar_cache, survey_type,
      paste0(clean_name(species), ".rds")
    ))
  } else {
    out
  }
}

#' Plot pstar - Visualization of pstar Estimation
#'
#' Generates a visualization of pstar estimation, including a line plot of
#' predicted values, shaded ribbons representing confidence intervals, and a
#' vertical line indicating the estimated pstar value.
#'
#' @param pstar_object A list containing the following components:
#'  - `pred_df`: A data frame with predicted values and confidence intervals.
#'  - `pstar_df`: A data frame with the estimated pstar value.
#'  - `species`: A character vector containing the species name.
#'
#' @return A ggplot2 object displaying the pstar visualization.
#'
#' @export
#'
plot_pstar <- function(pstar_object) {
  ggplot() +
    geom_line(data = pstar_object$pred_df, aes(x = prop_removed, y = fit)) +
    geom_ribbon(data = pstar_object$pred_df, aes(
      x = prop_removed, y = fit,
      ymin = lwr, ymax = upr
    ), alpha = 0.3) +
    geom_vline(data = pstar_object$pstar_df, aes(xintercept = pstar)) +
    theme_pbs() +
    ggtitle(pstar_object$species)
}

#' Calculate the Central Difference Approximation of the First Derivative
#'
#' This function calculates the central difference approximation of the first
#' derivative of a generalized additive model (GAM) with respect to a specified
#' predictor variable.
#'
#' @param gam_object A fitted GAM model.
#' @param newdata A data frame containing the values of predictor variables
#'   used to calculate the derivative.
#' @param x Name of the predictor variable with respect to which you want to
#'   calculate the derivative.
#' @param h Step size for the central difference approximation (default = 0.005)
#'
#' @returns A numeric vector representing the central difference approximation of
#'   the first derivative.
#' @export
#'
central_diff <- function(gam_object, newdata, x, h) {
  ndf <- newdata
  ndf[[x]] <- ndf[[x]] + h / 2
  ndb <- newdata
  ndb[[x]] <- ndb[[x]] - h / 2

  Xf <- predict(gam_object, ndf, type = "lpmatrix") # , exclude = exclude_re)
  Xb <- predict(gam_object, ndb, type = "lpmatrix") # , exclude = exclude_re)
  Xdiff <- (Xf - Xb) / h
}

#' Calculate First Derivative and Confidence Intervals for GAM Smooth Terms
#'
#' This function calculates the first derivatives and confidence intervals for
#' specified smooth terms in a generalized additive model (GAM) using central
#' difference approximations. It provides point estimates and confidence
#' intervals for the derivatives of the specified terms.
#'
#'
#' @param gam_object A fitted GAM model.
#' @param newdata A data frame containing the values of predictor variables
#' to calculate the derivative.
#' @param terms A character vector specifying the smooth terms for which
#'   derivatives are calculated.
#' @param term_columns A character vector specifying the columns in 'newdata' that
#'   correspond to the terms specified in 'terms'.
#' @param x Name of the predictor variable with respect to which you want to
#'   calculate the derivatives.
#' @param h Step size for the central difference approximation (default = 0.005)
#' @param nsim The number of simulations used to estimate confidence intervals.
#' @param level Confidence level for the intervals (default is 0.95).
#'
#' @returns A tibble with the following columns:
#'   - `smooth`: Name of the smooth term.
#'   - `newdata`: Values of the corresponding term in 'newdata'.
#'   - `derivative`: Point estimate of the derivative.
#'   - `se`: Standard error of the estimate.
#'   - `crit`: Critical value for the confidence interval.
#'   - `lower`: Lower bound of the confidence interval.
#'   - `upper`: Upper bound of the confidence interval.
#' @export
#'
get_fderiv <- function(
    gam_object, newdata, terms, term_columns, x, h = 0.005, nsim,
    level = 0.95) {
  message("\tFitting f' for species: ", unique(newdata$species))
  Xdiff <- central_diff(gam_object, newdata, x, h)

  Vb <- vcov(gam_object, freq = FALSE, unconditional = TRUE) # Bayesian posterior CV
  betas <- coef(gam_object)

  lpmat_col_ids <- grep(terms, colnames(Xdiff), fixed = TRUE)

  Xi <- Xdiff * 0 # zero out the Xp matrix

  Xi[, lpmat_col_ids] <- Xdiff[, lpmat_col_ids] # copy bits of Xp we need

  d <- drop(Xi %*% betas) # drop some NULL dimension that shows up and get derivative
  se <- rowSums(Xi %*% Vb * Xi)^0.5

  d1_tbl <- tibble(
    smooth = rep(terms, length(d)),
    newdata = newdata[term_columns],
    derivative = d,
    se = se
  ) %>%
    unnest(c("newdata"))

  buDiff <- mvnfast::rmvn(n = nsim, mu = rep(0, nrow(Vb)), sigma = Vb)
  simDev <- tcrossprod(Xi, buDiff) # Xi %*% t(bu) # simulate deviations from expected
  absDev <- abs(sweep(simDev, 1L, d1_tbl[["se"]], FUN = "/")) # absolute deviations
  masd <- apply(absDev, 2L, max) # & max abs deviation per sim
  ## simultaneous interval critical value
  crit <- quantile(masd, prob = level, type = 8) # type 8 is recommended by Hyndman and Fan (1996)
  adj <- (crit * d1_tbl[["se"]])
  derivative <- tibble::add_column(d1_tbl,
    crit  = rep(crit, nrow(d1_tbl)),
    lower = d1_tbl[["derivative"]] - adj,
    upper = d1_tbl[["derivative"]] + adj
  )
}
