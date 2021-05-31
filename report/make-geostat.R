# A quick script to run the geostatistical index standardization across all species.
# Hopefully this can be cleaned up in the future.

library(ggplot2)
library(dplyr)
library(sdmTMB)
library(future)
dir.create(here::here("report/geostat-cache"), showWarnings = FALSE)

.spp <- gfsynopsis::get_spp_names()
.spp <- dplyr::pull(dplyr::filter(.spp, type %in% c("A", "B")), spp_w_hyphens)
survs <- c("SYN QCS", "SYN HS", "SYN WCHG", "SYN WCVI")
all <- expand.grid(spp = .spp, survs = survs, stringsAsFactors = FALSE)

# parallel_processing <- TRUE
# cores <- floor(future::availableCores() / 2)

if (!exists("cores") || !exists("parallel_processing")) {
  stop(
    "Please run this script as part of `make.R` or set ",
    "`cores` and `parallel_processing` to the number of desired cores ",
    "and a logical value for whether or not to use parallel processing."
  )
}

if (parallel_processing) {
  future::plan(multiprocess, workers = cores)
} else {
  future::plan(sequential)
}

out <- future.apply::future_lapply(seq_len(nrow(all)), function(i) {
  file <- here::here("report", "geostat-cache",
    paste0(all$spp[i], "-", gsub(" ", "-", all$survs[i]), ".rds"))
  if (!file.exists(file)) {
    .out <- tryCatch(gfsynopsis::fit_sdmTMB_westcoast(
      here::here("report", "data-cache", paste0(all$spp[i], ".rds")),
      species_name = all$spp[i], include_depth = FALSE,
      survey = all$survs[i], n_knots = 150L, bias_correct = FALSE,
      anisotropy = FALSE
    ), error = function(e) NA)
    saveRDS(.out, file = file)
  } else {
    .out <- readRDS(file)
  }
  .out
})

saveRDS(out, file = here::here("report/geostat-cache/spt-index-out.rds"))

f <- list.files("report/geostat-cache/", full.names = TRUE)
f <- f[grepl("\\.rds", f)]
f <- f[grepl("-SYN-", f)]
out <- purrr::map(f, function(x) {
  cat(x, "\n")
  readRDS(x)
})

.sum <- purrr::map(out, function(x) {
  if (length(x) > 1) {
    if (!is.na(x$model[[1]])) {
      # browser()
      .rep <- as.list(x$model$sd_report, "Estimate", report = TRUE)
      data.frame(range = .rep$range, sigmaO = .rep$sigma_O, sigmaE = .rep$sigma_E)
    }
  }
})

g1 <- .sum %>% filter(sigmaE < 100) %>% ggplot(aes(sigmaE)) + geom_histogram()
g2 <- .sum %>% filter(sigmaE < 100) %>% ggplot(aes(sigmaO)) + geom_histogram()
g3 <- .sum %>% filter(sigmaE < 100) %>% ggplot(aes(range)) + geom_histogram()
cowplot::plot_grid(g1, g2, g3, nrow = 1)


get_convergence_criteria <- function(x) {
  if (length(x) > 1L) {
    eigval <- try(1 / eigen(x$model$sd_report$cov.fixed)$values, silent = TRUE)
    bad_eig <- if (is(eigval, "try-error") ||
      (min(eigval) < .Machine$double.eps * 10)) {
      TRUE
    } else {
      FALSE
    }
    max_gradient <- max(x$model$gradients)
    nlminb_convergence <- if (x$model$model$convergence == 0) TRUE else FALSE
    hessian <- x$model$sd_report$pdHess

    data.frame(
      survey_abbrev = x$survey, species = x$species_name,
      max_gradient = max_gradient, bad_eig = bad_eig,
      nlminb_convergence = nlminb_convergence, hessian = hessian,
      stringsAsFactors = FALSE
    ) %>%
      tibble::as_tibble()
  }
}

convergence_df <- purrr::map_df(out, ~ get_convergence_criteria(.x))

did_not_converge <- dplyr::filter(convergence_df, max_gradient > 0.01 |
  bad_eig | !nlminb_convergence | !hessian)
did_not_converge

index <- purrr::map_df(out, function(x) {
  if (length(x) > 1L) {
    data.frame(x$index,
      species = x$species_name, survey = x$survey,
      stringsAsFactors = FALSE
    ) %>% tibble::as_tibble()
  }
})

dplyr::filter(index, bad_eig | max_gradient > 0.01)

index <- dplyr::filter(index, !bad_eig, max_gradient < 0.01)

saveRDS(index, file = here::here("report/geostat-cache/spt-index-out-no-depth.rds"))

index$survey <- factor(index$survey, levels = survs)
# ggplot(index, aes(year, est)) + geom_line() +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
#   xlab('Year') + ylab('Biomass estimate (metric tonnes)') +
#   facet_grid(species~survey, scales = "free")

design_based <- purrr::map_df(unique(index$species), function(sp) {
  message("Getting design based index for ", sp)
  .d <- readRDS(here::here("report", "data-cache", paste0(sp, ".rds")))
  .d$survey_index
})

index <- index %>%
  group_by(survey, species) %>%
  mutate(
    lwr = lwr / exp(mean(log(est))),
    upr = upr / exp(mean(log(est))),
    est = est / exp(mean(log(est)))
  ) %>%
  ungroup()

des <- design_based %>%
  group_by(survey_abbrev, species_common_name) %>%
  mutate(
    lowerci = lowerci / exp(mean(log(biomass))),
    upperci = upperci / exp(mean(log(biomass))),
    biomass = biomass / exp(mean(log(biomass)))
  ) %>%
  ungroup() %>%
  select(year, biomass, lowerci, upperci, survey_abbrev, species_common_name, re) %>%
  filter(survey_abbrev %in% unique(index$survey)) %>%
  rename(
    est = biomass, lwr = lowerci, upr = upperci, survey = survey_abbrev,
    species = species_common_name, cv = re
  ) %>%
  mutate(species = gsub(" ", "-", species)) %>%
  mutate(species = gsub("/", "-", species)) %>%
  mutate(type = "Design based")

index$type <- "Spatiotemporal"
ind <- suppressWarnings(dplyr::bind_rows(index, des))
inds <- dplyr::group_by(ind, survey, species, type) %>%
  dplyr::summarise(
    max_cv = max(cv, na.rm = TRUE) < 1,
    max_est = max(est) < 50,
    cv_na = all(!is.na(cv))
  )
inds <- inds %>% dplyr::filter(max_cv, max_est, cv_na)
ind <- dplyr::semi_join(ind, inds)
ind <- dplyr::filter(ind, species != "pacific-hake")
ind$survey <- factor(ind$survey, levels = survs)
saveRDS(ind, file = here::here("report/geostat-cache/geostat-index-estimates.rds"))

g <- ggplot(ind, aes_string("year", "est", fill = "type")) +
  geom_line(aes_string(colour = "type")) +
  geom_point(aes_string(colour = "type")) +
  geom_ribbon(aes_string(ymin = "lwr", ymax = "upr"), alpha = 0.4) +
  xlab("Year") + ylab("Relative biomass estimate") +
  facet_grid(species ~ survey, scales = "free_y") +
  scale_x_continuous(breaks = seq(2000, 2020, 5)) +
  labs(colour = "Type", fill = "Type")

ggsave(here::here("report/geostat-cache/surv-no-depth-150-knots.pdf"),
  width = 9.5, height = 65, limitsize = FALSE
)

g_models <- readRDS(here::here("report/geostat-cache/spt-index-out.rds"))
g_models_dat <- purrr::map_df(g_models, function(x) {
  if (length(x) > 1L) {
    phi <- exp(x$model$tmb_obj$env$last.par[["ln_phi"]])
    thetaf <- x$model$tmb_obj$env$last.par[["thetaf"]]
    thetaf <- plogis(thetaf) + 1
    tau_O <- exp(x$model$tmb_obj$env$last.par[["ln_tau_O"]])
    tau_E <- exp(x$model$tmb_obj$env$last.par[["ln_tau_E"]])
    kappa <- exp(x$model$tmb_obj$env$last.par[["ln_kappa"]])
    tibble::tibble(
      species_name = gfsynopsis:::first_cap(gsub("-", " ", x$species_name)),
      survey = x$survey,
      kappa = kappa,
      tau_O = tau_O,
      tau_E = tau_E,
      phi = phi,
      thetaf = thetaf
    )
  }
})

saveRDS(g_models_dat, file = here::here("report/geostat-cache/spt-index-out-dat.rds"))
