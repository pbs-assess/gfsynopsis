library(dplyr)
library(ggplot2)
library(sdmTMB) # needs pc-prior branch! `install_github("pbs-assess/sdmTMB", ref = "pc-prior")`
survey_sets <- readRDS("report/data-cache/yellowmouth-rockfish.rds")$survey_sets
dat <- filter(survey_sets, survey_abbrev %in%
    c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG"))
dat <- filter(dat, !(year == 2014 & survey_abbrev == "SYN WCHG"))
dat <- dplyr::filter(dat, !is.na(depth_m))
dat$depth <- dat$depth_m
dat$akima_depth <- dat$depth_m
dat$X <- dat$longitude
dat$Y <- dat$latitude
dat <- gfplot:::ll2utm(dat)
dat2 <- gfplot:::scale_survey_predictors(dat)
range(dat2$density_kgpm2)
dat2$density_kgp100m2 <- dat2$density_kgpm2 * 100 * 100
range(dat2$density_kgp100m2)

dat_surv <- filter(dat2, survey_abbrev == "SYN WCHG")
mesh <- make_mesh(dat_surv, c("X", "Y"), cutoff = 3)
plot(mesh$mesh, main = NA, edge.color = "grey40", asp = 1)
points(mesh$loc_xy, pch = 21, col = "#FF000050", cex = 0.2)
mesh$mesh$n

filter(dat_surv, density_kgpm2 > 0) %>%
  ggplot(aes(longitude, latitude, size = density_kgpm2)) + geom_point(pch = 21, fill = "grey30") +
  facet_wrap(~year) +
  scale_size_area() +
  geom_point(data = filter(dat_surv, density_kgpm2 == 0), pch = 4, size = 2, alpha = 0.2) +
  coord_fixed() +
  gfplot::theme_pbs()
ggsave("~/Desktop/yellowmouth.png", width = 15, height = 15)

m <- sdmTMB(
  # density_kgp100m2 ~ 0 + poly(log(depth), 3) + as.factor(year),
  # density_kgp100m2 ~ 0 + depth_scaled + depth_scaled2 + as.factor(year),
  density_kgp100m2 ~ 0 + as.factor(year),
  family = tweedie(),
  data = dat_surv,
  time = "year",
  silent = FALSE,
  anisotropy = FALSE,
  spde = mesh,
  include_spatial = TRUE,
  matern_prior_E = c(5, 0.05, 5, 0.05)
)
m2

# marginal depth effect?
nd2 <- data.frame(depth =
    seq(min(dat_surv$depth), max(dat_surv$depth), length.out = 300))
nd2$year <- 2020L
p <- predict(m2, newdata = nd2, se_fit = TRUE, re_form = NA)
ggplot(p, aes(depth, exp(est), ymin = exp(est - 1 * est_se), ymax = exp(est + 1 * est_se))) +
  geom_line() +
  geom_ribbon(alpha = 0.4) +
  coord_cartesian(ylim = c(0, max(exp(p$est)) * 2.5), expand = FALSE) +
  geom_point(data = dat_surv, aes(x = depth, y = density_kgp100m2), inherit.aes = FALSE) +
  scale_x_log10()

synoptic_grid <- gfplot::synoptic_grid
synoptic_grid$survey_abbrev <- synoptic_grid$survey
# expand grid to all years:
original_time <- sort(unique(dat_surv$year))
nd <- do.call("rbind",
  replicate(length(original_time), synoptic_grid, simplify = FALSE))
nd[["year"]] <- rep(original_time, each = nrow(synoptic_grid))
# nd$depth_scaled <- (log(nd$depth) - dat_surv$depth_mean[1]) / dat_surv$depth_sd[1]
# nd$depth_scaled2 <- nd$depth_scaled^2

nd <- filter(nd, survey_abbrev == "SYN WCHG")

p <- predict(m, newdata = nd, sims = 1000L)
# ind <- sdmTMB::get_index_sims(p, return_sims = TRUE)
#
# ind %>% ggplot(aes(as.factor(year), .value)) + geom_violin() +
#   scale_y_log10()

ind <- sdmTMB::get_index_sims(p)

design_ind <- readRDS("report/data-cache/yellowmouth-rockfish.rds")$survey_index
design_ind <- design_ind %>%
  mutate(est = biomass, lwr = lowerci, upr = upperci, type = "Design-based bootstrap") %>%
  filter(survey_abbrev == unique(nd$survey))
ind <- mutate(ind, type = "Geostatistical, IID, s(depth)")
scaling <- 10 * 10 * 2 * 2

ind %>%
  mutate(est = est * scaling, lwr = lwr * scaling, upr = upr * scaling) %>%
  bind_rows(design_ind) %>%
  ggplot(aes(year, est, ymin = lwr, ymax = upr, colour = type, fill = type)) +
  geom_line() + geom_ribbon(alpha = 0.25, colour = NA) +
  ylab("Relative biomass") + xlab("Year") +
  coord_cartesian(expand = FALSE, ylim = c(0, NA)) +
  gfplot::theme_pbs() +
  scale_fill_brewer(palette = "Set1") +
  scale_colour_brewer(palette = "Set1") +
  labs(fill = "Type", colour = "Type")
