# A quick script to run the geostatistical index standardization across all species.
# Hopefully this can be cleaned up in the future.

library(ggplot2)
library(dplyr)
library(sdmTMB)
library(future)
library(gfsynopsis)
dir.create(here::here("report/geostat-cache"), showWarnings = FALSE)
is_rstudio <- !is.na(Sys.getenv("RSTUDIO", unset = NA))
is_unix <- .Platform$OS.type == "unix"

.spp <- gfsynopsis::get_spp_names()
.spp <- dplyr::pull(dplyr::filter(.spp, type %in% c("A", "B")), spp_w_hyphens)
survs <- c("SYN QCS", "SYN HS", "SYN WCHG", "SYN WCVI")
all <- expand.grid(spp = .spp, survs = survs, stringsAsFactors = FALSE)

parallel_processing <- TRUE
(cores <- floor(future::availableCores() / 2)) - 1
cores <- 4L

# if (!exists("cores") || !exists("parallel_processing")) {
#   stop(
#     "Please run this script as part of `make.R` or set ",
#     "`cores` and `parallel_processing` to the number of desired cores ",
#     "and a logical value for whether or not to use parallel processing."
#   )
# }

options(future.globals.maxSize = 800 * 1024 ^ 2) # 800 mb
if (parallel_processing) {
  if (!is_rstudio && is_unix) {
    future::plan(multicore, workers = cores)
  } else {
    future::plan(sequential) # much frustration
  }
} else {
  future::plan(sequential)
}

if (!file.exists("report/all-survey-sets.rds")) {
  survey_sets <- furrr::future_map_dfr(unique(all$spp), function(x) {
    cat(x, "\n")
    readRDS(here::here("report", "data-cache", paste0(x, ".rds")))$survey_sets
  })
  saveRDS(survey_sets, file = here::here("report/all-survey-sets.rds"))
} else {
  survey_sets <- readRDS("report/all-survey-sets.rds")
}

survey_sets_syn <- dplyr::filter(survey_sets,
  survey_abbrev %in% c("SYN QCS", "SYN WCVI", "SYN WCHG", "SYN HS"))
survey_sets_syn <- dplyr::filter(survey_sets_syn,
  !(year == 2014 & survey_abbrev == "SYN WCHG")) # not used
survey_sets_syn <- left_join(survey_sets_syn,
  gfsynopsis::get_spp_names() %>% select(species_common_name, spp_w_hyphens),
  by = "species_common_name")

fit_sdmTMB_model <- function(dat, survey,
  species_name = "", cutoff = 15, cell_width = 2,
  anisotropy = FALSE, silent = TRUE, bias_correct = FALSE,
  include_depth = FALSE) {
  d <- dat
  d <- dplyr::filter(d, !(year == 2014 & survey_abbrev == "SYN WCHG")) # not used
  col <- if (grepl("SYN", survey)) "density_kgpm2" else "density_ppkm2"
  dat <- gfplot:::tidy_survey_sets(d, survey, years = seq(1, 1e6),
    density_column = col)
  if (mean(dat$present) < 0.05) stop("Not enough data.")
  .scale <- if (grepl("SYN", survey)) 1000 else 1 # for computational stability
  dat <- dplyr::mutate(dat, density = density * .scale)
  if (any(is.na(dat$depth)))
    dat <- gfplot:::interp_survey_bathymetry(dat)$data
  dat <- gfplot:::scale_survey_predictors(dat)
  if (grepl("SYN", survey)) {
    grid_locs <- gfplot::synoptic_grid %>%
      dplyr::filter(.data$survey == survey) %>%
      dplyr::select(.data$X, .data$Y, .data$depth)
    grid_locs$depth_scaled <-
      (log(grid_locs$depth) - dat$depth_mean[1]) / dat$depth_sd[1]
    grid_locs$depth_scaled2 <- grid_locs$depth_scaled^2
    # Expand the prediction grid to create a slice for each time:
    original_time <- sort(unique(dat$year))
    nd <- do.call("rbind",
      replicate(length(original_time), grid_locs, simplify = FALSE))
    nd[["year"]] <- rep(original_time, each = nrow(grid_locs))
    grid_locs <- nd
  } else {
    stop("Non-synoptic surveys are not implemented yet.")
    # grid_locs <- if (surv == "HBLL OUT N") gfplot::hbll_n_grid$grid else gfplot::hbll_s_grid$grid
  }
  formula <- if (!include_depth) {
    stats::as.formula(density ~ 0 + as.factor(year))
  } else {
    stats::as.formula(density ~ 0 + as.factor(year) + depth_scaled + depth_scaled2)
  }
  spde <- sdmTMB::make_mesh(dat, xy_cols = c("X", "Y"), cutoff = cutoff)
  m <- sdmTMB::sdmTMB(
    formula = formula,
    data = dat, time = "year",
    spde = spde,
    family = sdmTMB::tweedie(link = "log"),
    anisotropy = anisotropy,
    silent = silent
  )
  predictions <- predict(m, newdata = grid_locs, sims = 500L)
  index <- get_index_sims(predictions, est_function = stats::median)
  index <- dplyr::mutate(index, cv = sqrt(exp(se^2) - 1))
  list(
    data = dat,
    model = m,
    index = index,
    scale = .scale,
    survey = survey,
    species_name = species_name
  )
}

fit_sdmTMB_coastwide <- function(dat,
  species_name = "", cutoff = 15,
  anisotropy = FALSE, silent = TRUE,
  include_depth_spline = TRUE) {

  dat <- filter(dat, year < 2020) # only wchg
  dat <- dplyr::filter(dat, !is.na(depth_m))
  dat$depth <- dat$depth_m
  dat$akima_depth <- dat$depth_m
  dat$X <- dat$longitude
  dat$Y <- dat$latitude
  dat <- gfplot:::ll2utm(dat)
  dat <- gfplot:::scale_survey_predictors(dat)
  dat$density_kgp100m2 <- dat$density_kgpm2 * 1000
  dat$density <- dat$density_kgp100m2
  dat$present <- dat$density_kgp100m2 > 0
  if (mean(dat$present) < 0.05) stop("Not enough data.", call. = FALSE)

  dat <- gfplot:::scale_survey_predictors(dat)

  synoptic_grid <- gfplot::synoptic_grid
  synoptic_grid$survey_abbrev <- synoptic_grid$survey
  # expand grid to all years:
  original_time <- sort(unique(dat$year))
  nd <- do.call("rbind",
    replicate(length(original_time), synoptic_grid, simplify = FALSE))
  nd[["year"]] <- rep(original_time, each = nrow(synoptic_grid))
  nd$depth_scaled <- (log(nd$depth) - dat$depth_mean[1]) / dat$depth_sd[1]
  nd$depth_scaled2 <- nd$depth_scaled^2
  grid_locs <- nd

  spde <- sdmTMB::make_mesh(dat, xy_cols = c("X", "Y"), cutoff = cutoff)

  formula <- if (include_depth_spline) {
    as.formula(density ~ 1 + s(log(depth), k = 3))
  } else {
    as.formula(density ~ 1)
  }
  m <- sdmTMB::sdmTMB(
    formula = formula,
    data = dat,
    time = "year",
    spde = spde,
    include_spatial = FALSE,
    ar1_fields = TRUE,
    family = sdmTMB::tweedie(link = "log"),
    anisotropy = anisotropy,
    silent = silent,
    matern_prior_E = c(5, 0.05, 10, 0.05)
  )

  # predict on grid and calculate index:
  index_ar1_survs <- list()

  p_ar1 <- predict(m, newdata = grid_locs, sims = 500L)
  index_ar1 <- get_index_sims(p_ar1)
  p <- p_ar1[grid_locs$survey == "SYN QCS",]
  attr(p, "time") <- "year"
  index_ar1_survs$QCS <- get_index_sims(p, return_sims = FALSE)

  p <- p_ar1[grid_locs$survey == "SYN HS",]
  attr(p, "time") <- "year"
  index_ar1_survs$HS <- get_index_sims(p, return_sims = FALSE)

  p <- p_ar1[grid_locs$survey == "SYN WCVI",]
  attr(p, "time") <- "year"
  index_ar1_survs$WCVI <- get_index_sims(p, return_sims = FALSE)

  p <- p_ar1[grid_locs$survey == "SYN WCHG",]
  attr(p, "time") <- "year"
  index_ar1_survs$WCHG <- get_index_sims(p, return_sims = FALSE)

  list(
    data = dat,
    model = m,
    index_coast = index_ar1,
    index_survs = index_ar1_survs,
    scale = 1000,
    species_name = species_name
  )
}

fit_isotropic <- function(x) {
  dir.create("report/geostat-cache/", showWarnings = FALSE)
  file <- here::here("report", "geostat-cache",
    paste0(x$spp_w_hyphens[1], "-", gsub(" ", "-", x$survey_abbrev[1]), ".rds"))
  if (!file.exists(file)) {
    .out <- tryCatch({fit_sdmTMB_model(
      dat = x,
      species_name = x$species_common_name[1],
      include_depth = FALSE,
      survey = x$survey_abbrev[1],
      cutoff = 10,
      bias_correct = FALSE,
      silent = TRUE,
      anisotropy = FALSE)}, error = function(e) NA)
    saveRDS(.out, file = file)
  }
}

# survey_sets_syn %>%
#   group_by(survey_abbrev, species_common_name) %>%
#   group_split() %>%
#   furrr::future_walk(fit_isotropic)

fit_coastwide <- function(x) {
  dir.create("report/geostat-cache-ar1-coast/", showWarnings = FALSE)
  file <- here::here("report", "geostat-cache-ar1-coast",
    paste0(x$spp_w_hyphens[1], "-", "synoptic-coastwide", ".rds"))
  if (!file.exists(file)) {
    .out <- tryCatch({fit_sdmTMB_coastwide(
      dat = x,
      species_name = x$species_common_name[1],
      cutoff = 15,
      silent = FALSE,
      anisotropy = FALSE)}, error = function(e) print(e))
    saveRDS(.out, file = file)
  }
}

fit_coastwide_nodepth <- function(x) {
  dir.create("report/geostat-cache-ar1-no-depth-coast/", showWarnings = FALSE)
  file <- here::here("report", "geostat-cache-ar1-no-depth-coast",
    paste0(x$spp_w_hyphens[1], "-", "synoptic-coastwide", ".rds"))
  if (!file.exists(file)) {
    .out <- tryCatch({fit_sdmTMB_coastwide(
      dat = x,
      species_name = x$species_common_name[1],
      cutoff = 15,
      silent = FALSE,
      anisotropy = FALSE)}, error = function(e) print(e))
    saveRDS(.out, file = file)
  }
}

# if (any(is.na(dat$depth)))
#   dat <- gfplot:::interp_survey_bathymetry(dat)$data

# xx <- survey_sets_syn %>%
#   group_by(species_common_name) %>%
#   group_split()

# sum(is.na(xx[[4]]$depth_m))

# mm <- fit_coastwide(xx[[4]])

survey_sets_syn %>%
  group_by(species_common_name) %>%
  dplyr::filter(year < 2020) %>% # only wchg
  dplyr::filter(!is.na(depth_m)) %>%
  mutate(prop_present = mean(density_kgpm2 > 0)) %>%
  dplyr::filter(prop_present >= 0.05) %>%
  group_split() %>%
  furrr::future_walk(fit_coastwide)
# purrr::walk(fit_coastwide)

survey_sets_syn %>%
  group_by(species_common_name) %>%
  dplyr::filter(year < 2020) %>% # only wchg
  dplyr::filter(!is.na(depth_m)) %>%
  mutate(prop_present = mean(density_kgpm2 > 0)) %>%
  dplyr::filter(prop_present >= 0.05) %>%
  group_split() %>%
  # furrr::future_walk(fit_coastwide_nodepth)
  purrr::walk(fit_coastwide_nodepth)

f <- list.files("report/geostat-cache-ar1-coast/", full.names = TRUE)
ar1_fits <- purrr::map_dfr(f, function(x) {
  cat(x, "\n")
  fit <- readRDS(x)
  dplyr::bind_rows(fit$index_survs, .id = "survey") %>%
    dplyr::bind_rows(mutate(fit$index_coast, survey = "Coast")) %>%
    mutate(species_name = fit$species_name, scale = fit$scale)
})
saveRDS(ar1_fits, file = "report/ar1-coast-geostat-indexes.rds")
ar1_fits <- readRDS("report/ar1-coast-geostat-indexes.rds")

f <- list.files("report/geostat-cache-ar1-no-depth-coast/", full.names = TRUE)
ar1_nodepth_fits <- purrr::map_dfr(f, function(x) {
  cat(x, "\n")
  fit <- readRDS(x)
  dplyr::bind_rows(fit$index_survs, .id = "survey") %>%
    dplyr::bind_rows(mutate(fit$index_coast, survey = "Coast")) %>%
    mutate(species_name = fit$species_name, scale = fit$scale)
})
saveRDS(ar1_nodepth_fits, file = "report/ar1-coast-no-depth-geostat-indexes.rds")
ar1_nodepth_fits <- readRDS("report/ar1-coast-no-depth-geostat-indexes.rds")

blue <- RColorBrewer::brewer.pal(3, "Blues")[3]
g <- ar1_fits %>%
  filter(survey == "Coast") %>%
  group_by(species_name) %>%
  filter(max(upr) < 1e6) %>%
  mutate(species_name = stringr::str_to_title(species_name)) %>%
  ggplot(aes(year, est, ymin = lwr, ymax = upr)) + geom_line(col = blue) +
  facet_wrap(~species_name, scales = "free_y", ncol = 9) +
  geom_ribbon(alpha = 0.4, fill = blue) +
  gfplot::theme_pbs() +
  scale_x_continuous(breaks = c(seq(2005, 2015, 5), 2019)) +
  coord_cartesian(xlim = c(2003, 2019), expand = FALSE) +
  labs(x = "Year", y = "Biomass density\n(units to be scaled)")
ggsave("report/ar1-geostat-coast.pdf", width = 19, height = 8)

g <- ar1_fits %>%
  filter(survey != "Coast") %>%
  group_by(survey, species_name) %>%
  filter(max(upr) < 1e6) %>%
  mutate(species_name = stringr::str_to_title(species_name)) %>%
  ggplot(aes(year, est, ymin = lwr, ymax = upr)) + geom_line(col = blue) +
  facet_grid(species_name~survey, scales = "free_y") +
  geom_ribbon(alpha = 0.4, fill = blue) +
  gfplot::theme_pbs() +
  scale_x_continuous(breaks = c(seq(2005, 2015, 5), 2019)) +
  coord_cartesian(xlim = c(2003, 2019), expand = FALSE) +
  labs(x = "Year", y = "Biomass density\n(units to be scaled)")
ggsave("report/ar1-geostat-by-survey.pdf", width = 10, height = 40)

f <- list.files("report/geostat-cache/", full.names = TRUE)
iso_fits <- purrr::map_dfr(f, function(x) {
  cat(x, "\n")
  fit <- readRDS(x)
  if (length(fit) > 1) {
    fit$index %>%
      mutate(species_name = fit$species_name, survey = fit$survey)
  }
})
saveRDS(iso_fits, file = "report/iso-iid-geostat-indexes.rds")
iso_fits <- readRDS("report/iso-iid-geostat-indexes.rds")

g <- iso_fits %>%
  group_by(survey, species_name) %>%
  filter(max(upr) < 1e6) %>%
  mutate(species_name = stringr::str_to_title(species_name)) %>%
  ggplot(aes(year, est, ymin = lwr, ymax = upr)) + geom_line(col = blue) +
  facet_grid(species_name~survey, scales = "free_y") +
  geom_ribbon(alpha = 0.4, fill = blue) +
  gfplot::theme_pbs() +
  scale_x_continuous(breaks = c(seq(2005, 2015, 5), 2019)) +
  coord_cartesian(xlim = c(2003, 2019), expand = FALSE) +
  labs(x = "Year", y = "Biomass density\n(units to be scaled)")
ggsave("report/iso-iid-geostat-by-survey.pdf", width = 10, height = 40)


ar1_fits %>%
  filter(species_name == "yellowmouth rockfish") %>%
  mutate(species_name = stringr::str_to_title(species_name)) %>%
  ggplot(aes(year, est, ymin = lwr, ymax = upr)) + geom_line(col = blue) +
  facet_wrap(~survey, scales = "free_y") +
  geom_ribbon(alpha = 0.4, fill = blue) +
  gfplot::theme_pbs() +
  scale_x_continuous(breaks = c(seq(2005, 2015, 5), 2019)) +
  coord_cartesian(xlim = c(2003, 2019), expand = FALSE) +
  labs(x = "Year", y = "Biomass density\n(units to be scaled)")
ggsave("report/ar1-geostat-by-survey.pdf", width = 10, height = 40)


# # options(future.debug = TRUE)
# # out <- future.apply::future_lapply(seq_len(nrow(all)), function(i) {
# out <- purrr::map(seq_len(nrow(all)), function(i) {
# # out <- lapply(1:1, function(i) {
#   file <- here::here("report", "geostat-cache",
#     paste0(all$spp[i], "-", gsub(" ", "-", all$survs[i]), ".rds"))
#   if (!file.exists(file)) {
#     # cat(all$survs[i], all$spp[i], "\n")
#     .out <- tryCatch(gfsynopsis::fit_sdmTMB_westcoast(
#       here::here("report", "data-cache", paste0(all$spp[i], ".rds")),
#       species_name = all$spp[i],
#       include_depth = FALSE,
#       survey = all$survs[i],
#       cutoff = 10,
#       bias_correct = FALSE,
#       silent = FALSE,
#       anisotropy = TRUE
#     ), error = function(e) NA)
#     saveRDS(.out, file = file)
#   } else {
#     .out <- readRDS(file)
#   }
#   .out
# })
#
# saveRDS(out, file = here::here("report/geostat-cache/spt-index-out.rds"))
#
# f <- list.files("report/geostat-cache/", full.names = TRUE)
# f <- f[grepl("\\.rds", f)]
# f <- f[grepl("-SYN-", f)]
# out <- purrr::map(f, function(x) {
#   cat(x, "\n")
#   readRDS(x)
# })
#
# .sum <- purrr::map(out, function(x) {
#   if (length(x) > 1) {
#     if (!is.na(x$model[[1]])) {
#       # browser()
#       .rep <- as.list(x$model$sd_report, "Estimate", report = TRUE)
#       data.frame(range = .rep$range, sigmaO = .rep$sigma_O, sigmaE = .rep$sigma_E)
#     }
#   }
# })
#
# g1 <- .sum %>% filter(sigmaE < 100) %>% ggplot(aes(sigmaE)) + geom_histogram()
# g2 <- .sum %>% filter(sigmaE < 100) %>% ggplot(aes(sigmaO)) + geom_histogram()
# g3 <- .sum %>% filter(sigmaE < 100) %>% ggplot(aes(range)) + geom_histogram()
# cowplot::plot_grid(g1, g2, g3, nrow = 1)
#
#
# get_convergence_criteria <- function(x) {
#   if (length(x) > 1L) {
#     eigval <- try(1 / eigen(x$model$sd_report$cov.fixed)$values, silent = TRUE)
#     bad_eig <- if (is(eigval, "try-error") ||
#       (min(eigval) < .Machine$double.eps * 10)) {
#       TRUE
#     } else {
#       FALSE
#     }
#     max_gradient <- max(x$model$gradients)
#     nlminb_convergence <- if (x$model$model$convergence == 0) TRUE else FALSE
#     hessian <- x$model$sd_report$pdHess
#
#     data.frame(
#       survey_abbrev = x$survey, species = x$species_name,
#       max_gradient = max_gradient, bad_eig = bad_eig,
#       nlminb_convergence = nlminb_convergence, hessian = hessian,
#       stringsAsFactors = FALSE
#     ) %>%
#       tibble::as_tibble()
#   }
# }
#
# convergence_df <- purrr::map_df(out, ~ get_convergence_criteria(.x))
#
# did_not_converge <- dplyr::filter(convergence_df, max_gradient > 0.01 |
#   bad_eig | !nlminb_convergence | !hessian)
# did_not_converge
#
# index <- purrr::map_df(out, function(x) {
#   if (length(x) > 1L) {
#     data.frame(x$index,
#       species = x$species_name, survey = x$survey,
#       stringsAsFactors = FALSE
#     ) %>% tibble::as_tibble()
#   }
# })
#
# dplyr::filter(index, bad_eig | max_gradient > 0.01)
#
# index <- dplyr::filter(index, !bad_eig, max_gradient < 0.01)
#
# saveRDS(index, file = here::here("report/geostat-cache/spt-index-out-no-depth.rds"))
#
# index$survey <- factor(index$survey, levels = survs)
# # ggplot(index, aes(year, est)) + geom_line() +
# #   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
# #   xlab('Year') + ylab('Biomass estimate (metric tonnes)') +
# #   facet_grid(species~survey, scales = "free")
#
# design_based <- purrr::map_df(unique(index$species), function(sp) {
#   message("Getting design based index for ", sp)
#   .d <- readRDS(here::here("report", "data-cache", paste0(sp, ".rds")))
#   .d$survey_index
# })
#
# index <- index %>%
#   group_by(survey, species) %>%
#   mutate(
#     lwr = lwr / exp(mean(log(est))),
#     upr = upr / exp(mean(log(est))),
#     est = est / exp(mean(log(est)))
#   ) %>%
#   ungroup()
#
# des <- design_based %>%
#   group_by(survey_abbrev, species_common_name) %>%
#   mutate(
#     lowerci = lowerci / exp(mean(log(biomass))),
#     upperci = upperci / exp(mean(log(biomass))),
#     biomass = biomass / exp(mean(log(biomass)))
#   ) %>%
#   ungroup() %>%
#   select(year, biomass, lowerci, upperci, survey_abbrev, species_common_name, re) %>%
#   filter(survey_abbrev %in% unique(index$survey)) %>%
#   rename(
#     est = biomass, lwr = lowerci, upr = upperci, survey = survey_abbrev,
#     species = species_common_name, cv = re
#   ) %>%
#   mutate(species = gsub(" ", "-", species)) %>%
#   mutate(species = gsub("/", "-", species)) %>%
#   mutate(type = "Design based")
#
# index$type <- "Spatiotemporal"
# ind <- suppressWarnings(dplyr::bind_rows(index, des))
# inds <- dplyr::group_by(ind, survey, species, type) %>%
#   dplyr::summarise(
#     max_cv = max(cv, na.rm = TRUE) < 1,
#     max_est = max(est) < 50,
#     cv_na = all(!is.na(cv))
#   )
# inds <- inds %>% dplyr::filter(max_cv, max_est, cv_na)
# ind <- dplyr::semi_join(ind, inds)
# ind <- dplyr::filter(ind, species != "pacific-hake")
# ind$survey <- factor(ind$survey, levels = survs)
# saveRDS(ind, file = here::here("report/geostat-cache/geostat-index-estimates.rds"))
#
# g <- ggplot(ind, aes_string("year", "est", fill = "type")) +
#   geom_line(aes_string(colour = "type")) +
#   geom_point(aes_string(colour = "type")) +
#   geom_ribbon(aes_string(ymin = "lwr", ymax = "upr"), alpha = 0.4) +
#   xlab("Year") + ylab("Relative biomass estimate") +
#   facet_grid(species ~ survey, scales = "free_y") +
#   scale_x_continuous(breaks = seq(2000, 2020, 5)) +
#   labs(colour = "Type", fill = "Type")
#
# ggsave(here::here("report/geostat-cache/surv-no-depth-150-knots.pdf"),
#   width = 9.5, height = 65, limitsize = FALSE
# )
#
# g_models <- readRDS(here::here("report/geostat-cache/spt-index-out.rds"))
# g_models_dat <- purrr::map_df(g_models, function(x) {
#   if (length(x) > 1L) {
#     phi <- exp(x$model$tmb_obj$env$last.par[["ln_phi"]])
#     thetaf <- x$model$tmb_obj$env$last.par[["thetaf"]]
#     thetaf <- plogis(thetaf) + 1
#     tau_O <- exp(x$model$tmb_obj$env$last.par[["ln_tau_O"]])
#     tau_E <- exp(x$model$tmb_obj$env$last.par[["ln_tau_E"]])
#     kappa <- exp(x$model$tmb_obj$env$last.par[["ln_kappa"]])
#     tibble::tibble(
#       species_name = gfsynopsis:::first_cap(gsub("-", " ", x$species_name)),
#       survey = x$survey,
#       kappa = kappa,
#       tau_O = tau_O,
#       tau_E = tau_E,
#       phi = phi,
#       thetaf = thetaf
#     )
#   }
# })
#
# saveRDS(g_models_dat, file = here::here("report/geostat-cache/spt-index-out-dat.rds"))
