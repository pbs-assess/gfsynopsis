# A quick script for now to run the geostatistical index standardization across all species
# This will be cleaned up in the future

library(ggplot2)
library(dplyr)
library(sdmTMB)
library(foreach)

spp <- gfsynopsis::get_spp_names()
spp <- dplyr::pull(dplyr::filter(spp, type %in% c("A", "B")), spp_w_hyphens)[[8]]
survs <- c('SYN QCS', 'SYN HS', 'SYN WCHG', 'SYN WCVI')[[4]]
all <- expand.grid(spp = spp, survs = survs,
  stringsAsFactors = FALSE)
cores <- min(nrow(all), parallel::detectCores())
cl <- parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)
dat <- readRDS("report/data-cache/pacific-cod.rds")
dat <- readRDS("report/data-cache/pacific-ocean-perch.rds")

.y <- filter(dat$survey_sets, survey_abbrev  == 'SYN QCS') %>% pull(year) %>% unique()
.y <- filter(dat$survey_sets, survey_abbrev  == 'SYN WCVI') %>% pull(year) %>% unique()
fit <- list()
for (i in seq_along(.y)) {
  fit[[i]] <- gfsynopsis::fit_survey_maps(dat$survey_sets,
  surveys = c("SYN QCS", "SYN HS", "SYN WCHG", "SYN WCVI")[[4]],
  species = "pacific ocean perch",
  # model = "inla", verbose = TRUE, max_edge = c(30, 100), years = synoptic_max_survey_years)
  model = "sdmTMB", silent = F, years = .y[i])
}

surveys <- c("SYN QCS", "SYN HS", "SYN WCHG", "SYN WCVI")[[4]]
# fi <- list.files(here::here("report/map-cache/synoptic"), full.names = FALSE)
out <- purrr::map_df(seq_along(.y), function(i) {
  mm <- fit[[i]]
  # out <- purrr::map_df(1:4, function(ii) {
  # if (length(mm$models) > 1L) {
  rd <- dplyr::filter(mm$raw_dat, survey == surveys[[1]])
  # if (!'depth_mean' %in% names(rd))
  #   stop('Scaling mean and SD are missing.')
  range_d <- -1 * exp(rd$depth_mean[1] +
      range(rd$depth_scaled, na.rm = TRUE) * rd$depth_sd[1])
  x <- seq(-3, 4, length.out = 300)
  x2 <- x^2
  B <- mm$models[[1]]$models$model$par
  if (B[[3]] <= 0) { # quadratic must be :-( shaped
    y <- B[[1]] + x * B[[2]] + x2 * B[[3]]
  } else {
    y <- NA
  }
  out <- data.frame(
    depth = -1*exp(x * rd$depth_sd[1] + rd$depth_mean[1]),
    y = exp(y) * 1000, # convert to kg/km^2 (was scaled by 1000 already)
    survey = surveys[1],
    species = gsub("-", " ", gsub(".rds", "", i)),
    stringsAsFactors = FALSE)
  out <- mutate(out,
    extrapolated = depth < min(range_d) * 1 |
      depth > max(range_d) * 1)
  # out
  data.frame(out, year = .y[i])
}
  # })
)


make_depth_plot <- function(.data,
  ylab = expression(Survey~biomass~density~(kg/km^2)),
  xlim = c(-500, -100)) {
  ggplot(.data, aes_string('depth', 'y', colour = 'as.factor(year)')) +
    geom_line(lty = 2) +
    coord_cartesian(xlim = xlim) +
    labs(x = 'Depth (m)',
      y = ylab, colour = 'Survey') +
    # facet_wrap(~forcats::fct_reorder(species,
    #   mean_mode_depth),
    # facet_wrap(~gfsynopsis:::first_cap(species), scales = "free_y", ncol = 4) +
    geom_line(data = dplyr::filter(.data, !extrapolated), lwd = 0.9) +
    ggplot2::scale_color_viridis_d() +
    gfplot::theme_pbs()
}

dd <- out %>% group_by(species, survey) %>%
  # mutate(y = y / max(y)) %>%
  mutate(max_y = max(y[!extrapolated])) %>%
  mutate(y = ifelse(y < max_y * 1.15, y, NA)) %>%
  mutate(mode_depth = depth[y == max(y)[1]]) %>%
  group_by(species) %>%
  mutate(mean_mode_depth = mean(mode_depth)) %>%
  filter(depth >= -800)

make_depth_plot(dd)
