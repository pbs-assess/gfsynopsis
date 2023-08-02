# Functions to deal with GAMs --------------------------------------------------
fit_gam <- function(dat) {
  message("\tFitting GAM for ", unique(dat$species))
  mod <- try(
    mgcv::gam(
    formula = catch ~ -1 + s(prop_removed) + fyear +
                      s(fstation, bs = "re") +
                      offset(log_eff_skate),
    family = 'nb',
    data = dat)
  )
  mod$data <- dat
  return(mod)
}

pred_gam <- function(object, by = 0.005) {
  dat <- object$data
  species <- unique(dat$species)
  prop_removed_min <- 0.15#floor(min(dat$prop_removed) * 100) / 100
  pred_df <- expand_grid(
    prop_removed = seq(from = prop_removed_min, to = 1, by = by),
    log_eff_skate = 0, fyear = dat$fyear[[1]], fstation = dat$fstation[[1]]
    # fyear = 1, fstation = 1 # This is what joe did but it doesn't work in his code, nor here
  )

  pred_mod <- predict.gam(object = object, newdata = pred_df, se.fit = TRUE,
    type = 'terms', terms = 's(prop_removed)'
  )

  pred_df$fit <- pred_mod[[1]][, 1]
  pred_df$se  <- pred_mod[[2]][, 1]
  pred_df <- pred_df |>
    mutate(lwr = fit - 1.96 * se, upr = fit + 1.96 * se, species = species)
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