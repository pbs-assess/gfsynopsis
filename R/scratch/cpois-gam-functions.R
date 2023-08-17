# Fit GAMs and use derivative to get pstar -------------------------------------
get_pstar <- function(sp_dat, gam_formula, survey_type = c('hbll_outside', 'hbll_inside', 'iphc'),
  prop_removed_min = NULL, h = 0.005, pstar_cache, save_out = TRUE) {
  species <- unique(sp_dat[['species_common_name']])

  message("\tFitting GAM for ", species)
  gam_fit <- try(
    mgcv::gam(formula = gam_formula, family = 'nb', data = sp_dat)
  )

  # Choose minimum proportion of hooks removed to predict over
  if (is.null(prop_removed_min)) {
    prop_removed_min <- floor(min(sp_dat$prop_removed) * 100) / 100
    assertthat::assert_that(!is.na(prop_removed_min))
  }

  # If the GAM fit, get GAM predictions based on survey type
  survey <- ifelse(survey_type == 'iphc', 'iphc', 'hbll' )
  if (inherits(gam_fit, 'gam')) {
    pred_df <- switch(survey,
      hbll = expand_grid(prop_removed = seq(prop_removed_min, 1, h),
                         fyear = sp_dat$fyear[[1]],
                         hook_count = 1,
                         species = species),
      iphc = expand_grid(prop_removed = seq(prop_removed_min, 1, h),
                         log_eff_skate = 0,
                         fyear = sp_dat$fyear[[1]],
                         fstation = sp_dat$fstation[[1]]),
    )

    pred_mod <- predict.gam(object = gam_fit, newdata = pred_df, se.fit = TRUE,
      type = 'terms', terms = 's(prop_removed)'
    )

    # Caculate first derivative
    message("\tGetting f' for ", species)
    f1 <- get_fderiv(gam_object = gam_fit, newdata = pred_df, terms = "prop_removed", term_columns = "prop_removed",
    x = prop_removed, h = h, nsim = 100, exclude_re = "fyear")

    pred_df$fit <- pred_mod[[1]][, 1]
    pred_df$se  <- pred_mod[[2]][, 1]
    pred_df <- pred_df |>
      mutate(lwr = fit - 1.96 * se, upr = fit + 1.96 * se, species = species)

    # Use f' to get pstar
    pstar_df <- f1 |>
      arrange(desc(prop_removed)) |>
      mutate(sign_change = ifelse(lag(derivative) * derivative > 0, "no", "yes"),
             slope_xh = ifelse(lag(derivative) > 0, "positive", "negative")
      ) |>
      filter(sign_change == "yes" & slope_xh == "negative") |>
      select(prop_removed, derivative, lower, upper, sign_change, slope_xh) |>
      rename(pstar = "prop_removed") |>
      slice(1)

    # This combination of objects is useful for examining pstar outputs
    out <- list(gam_fit = gam_fit, pred_df = pred_df, f1 = f1, pstar_df = pstar_df, species = species)
  } else {
    NA
  }

  if (save_out) {
    saveRDS(out, file.path(pstar_cache, survey_type, paste0(gfsynopsis:::clean_name(species), '.rds')))
  } else {
    out
  }
}

plot_pstar <- function(pstar_object, sp_dat) {
  sp_dat <- filter(sp_dat, present == 1)
  ggplot() +
    geom_line(data = pstar_object$pred_df, aes(x = prop_removed, y = fit)) +
    geom_ribbon(data = pstar_object$pred_df, aes(x = prop_removed, y = fit,
      ymin = lwr, ymax = upr), alpha = 0.3) +
    geom_vline(data = pstar_object$pstar_df, aes(xintercept = pstar)) +
    theme_pbs() +
    ggtitle(pstar_object$species) #+
    # geom_rug(data = sp_dat, aes(x = prop_removed),
    #          sides = 'b', alpha = 0.5)
}

# First derivative
# ------------------------------------------------------------------------------
shift_values <- function(df, column, h, FUN = `+`) {
      i <- grep(column, names(df))

      FUN <- match.fun(FUN)

      result <- df
      result[, i] <- FUN(result[, i], h)

      return(result)
}

central_diff1 <- function(gam_object, newdata, x, h) {
  ndf <- shift_values(df = newdata, column = x, h = h / 2, FUN = `+`)
  ndb <- shift_values(df = newdata, column = x, h = h / 2, FUN = `-`)

  Xf <- predict(gam_object, ndf, type = "lpmatrix")#, exclude = exclude_re)
  Xb <- predict(gam_object, ndb, type = "lpmatrix")#, exclude = exclude_re)
  Xdiff <- (Xf - Xb) / h

  return(Xdiff)
}

get_fderiv <- function(gam_object, newdata, terms, term_columns, x, h, nsim, exclude_re, level = 0.95) {
  message("\tFitting f' for species: ", unique(newdata$species))
  x <- deparse(substitute(x))

  Xdiff <- central_diff1(gam_object, newdata, x, h)

  Vb <- vcov(gam_object, freq = FALSE, unconditional = TRUE)  # Bayesian posterior CV
  betas <- coef(gam_object)

  lpmat_col_ids <- grep(terms, colnames(Xdiff), fixed = TRUE)

  Xi <- Xdiff * 0  # zero out the Xp matrix

  Xi[, lpmat_col_ids] <- Xdiff[, lpmat_col_ids]  # copy bits of Xp we need

  d <- drop(Xi %*% betas)  # drop some NULL dimension that shows up and get derivative
  se <- rowSums(Xi %*% Vb * Xi)^0.5

  d1_tbl <- tibble(smooth = rep(terms, length(d)),
                newdata = newdata[term_columns],
                # fit = fit,
                derivative = d,
                se = se) %>%
           unnest(c('newdata'))

  buDiff <- mvnfast::rmvn(n = nsim, mu = rep(0, nrow(Vb)), sigma = Vb)
  simDev <- tcrossprod(Xi, buDiff) # Xi %*% t(bu) # simulate deviations from expected
  absDev <- abs(sweep(simDev, 1L, d1_tbl[["se"]], FUN = "/")) # absolute deviations
  masd <- apply(absDev, 2L, max)  # & max abs deviation per sim
  ## simultaneous interval critical value
  crit <- quantile(masd, prob = level, type = 8)  # type 8 is recommended by Hyndman and Fan (1996)
  adj <- (crit * d1_tbl[["se"]])
  derivative <- add_column(d1_tbl,
                           crit  = rep(crit, nrow(d1_tbl)),
                           lower = d1_tbl[["derivative"]] - adj,
                           upper = d1_tbl[["derivative"]] + adj)
}