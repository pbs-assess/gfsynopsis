library(sdmTMB)
library(tictoc)
library(ggplot2)
set.seed(123)

predictor_dat <- expand.grid(
  X = seq(0, 1, length.out = 20),
  Y = seq(0, 1, length.out = 20),
  year = 1:10
)
nrow(predictor_dat)

mesh <- make_mesh(predictor_dat, xy_cols = c("X", "Y"), cutoff = 0.1)
plot(mesh)
mesh$mesh$n

yrs <- rnorm(10)

sim_dat <- sdmTMB_simulate(
  formula = ~ 0 + as.factor(year),
  data = predictor_dat,
  time = "year",
  mesh = mesh,
  family = poisson(),
  range = 0.4,
  sigma_E = 0.2,
  sigma_O = 0.2,
  phi = 1,
  seed = 42,
  B = yrs
)

extra_dispersion <- rnorm(nrow(sim_dat), 0, 0.5)
sim_dat$obs_id <- factor(seq_len(nrow(sim_dat)))
sim_dat$eta <- sim_dat$eta + extra_dispersion
sim_dat$mu <- exp(sim_dat$eta)
sim_dat$observed <- rpois(nrow(sim_dat), sim_dat$mu)

ggplot(sim_dat, aes(X, Y, fill = eta)) +
  geom_raster() +
  facet_wrap(vars(year)) +
  scale_fill_viridis_c()

ggplot(sim_dat, aes(X, Y, fill = mu)) +
  geom_raster() +
  facet_wrap(vars(year)) +
  scale_fill_viridis_c()

ggplot(sim_dat, aes(X, Y, fill = observed)) +
  geom_raster() +
  facet_wrap(vars(year)) +
  scale_fill_viridis_c()

head(sim_dat)
hist(sim_dat$observed)

me <- make_mesh(sim_dat, c("X", "Y"), mesh = mesh$mesh)
tic()
m <- sdmTMB(
  observed ~ 0 + as.factor(year) + (1 | obs_id),
  data = sim_dat,
  mesh = me,
  time = "year",
  family = poisson(),
  silent = FALSE
)
toc()
m
tidy(m, "ran_pars", conf.int = TRUE)
m$sd_report

prop_no_bait <- runif(nrow(sim_dat), 0, 1)
lwr <- sim_dat$observed
upr <- sim_dat$observed

# right bounded:
upr[prop_no_bait > 0.9] <- upr[prop_no_bait > 0.9] + 10
plot(jitter(lwr), jitter(upr))
abline(0, 1)

# or unbounded:
upr[prop_no_bait > 0.9] <- NA

tic()
m1 <- sdmTMB(
  observed ~ 0 + as.factor(year) + (1 | obs_id),
  data = sim_dat,
  mesh = me,
  time = "year",
  family = censored_poisson(),
  silent = FALSE,
  experimental = list(lwr = lwr, upr = upr)
)
toc()

m1
tidy(m1, "ran_pars", conf.int = TRUE)
m1$sd_report
m1$tmb_data$upr
m1$tmb_data$lwr
