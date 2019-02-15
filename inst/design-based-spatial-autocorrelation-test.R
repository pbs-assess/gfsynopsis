# Are design-based bootstrapped survey calculations affected by
# spatial autocorrelation?
#
# Short answer: I think not. Obvious in retrospect.

library(dplyr)
library(ggplot2)
x <- seq(0, 1, 0.1)
y <- seq(0, 1, 0.1)
N <- 10
survey_sample <- seq(1, N)

grid1 <- expand.grid(x = x, y = y) %>% as_tibble()
plot(grid1)

grid <- expand.grid(x = x, y = y, survey_sample = survey_sample) %>%
  group_by(x, y) %>%
  mutate(
    sampling_x = x + runif(N, 0, 0.1),
    sampling_y = y + runif(N, 0, 0.1)
  ) %>%
  mutate(value = rnorm(N, 0, 0.2))

ggplot(grid, aes(sampling_x, sampling_y)) +
  geom_point(aes(colour = value)) +
  scale_color_viridis_c() +
  geom_point(data = grid1, aes(x = x, y = y, colour = NA),
    colour = 'black', pch = 21, size = 3)

grid <- expand.grid(x = x, y = y, survey_sample = survey_sample) %>%
  mutate(grouping_code = paste(x, y, sep = '-')) %>%
  group_by(x, y) %>%
  mutate(
    sampling_x = x + runif(N, 0, 1/N),
    sampling_y = y + runif(N, 0, 1/N)
  ) %>% ungroup()

sigma_O <- 0.3
kappa <- 8
rf_omega <- RandomFields::RMmatern(nu = 1, var = sigma_O^2, scale = 1 / kappa)
grid$omega_s <- suppressMessages(
  RandomFields::RFsimulate(model = rf_omega,
    x = grid$sampling_x, y = grid$sampling_y)$variable1)

grid <- grid %>%
  mutate(value = exp(rnorm(nrow(grid), omega_s, 0.1)))

ggplot(grid, aes(sampling_x, sampling_y)) +
  geom_point(aes(colour = omega_s)) +
  scale_color_gradient2()

ggplot(grid, aes(sampling_x, sampling_y)) +
  geom_point(aes(colour = value)) +
  scale_color_viridis_c()

group_by(grid, grouping_code) %>%
  summarise(density = mean(value)) %>%
  summarise(biomass = sum(density)) %>%
  pull(biomass)

calc_bio <- function(dat, i = seq_len(nrow(dat))) {
  # print(i)
  oo <- dat[i, , drop = FALSE] %>%
    group_by(grouping_code) %>%
    summarise(density = mean(value)) %>%
    summarise(biomass = sum(density)) %>%
    pull(biomass)
  # print(oo)
  oo
}

boot_biomass <- function(dat, reps = 10) {
  dat %>%
    mutate(grouping_code = as.factor(grouping_code)) %>%
    do({
      b <- boot::boot(., statistic = calc_bio, strata = .$grouping_code,
        R = reps)
      suppressWarnings(bci <- boot::boot.ci(b, type = "perc"))
      dplyr::tibble(
        mean_boot = mean(b$t),
        median_boot = median(b$t),
        lwr = bci$percent[[4]],
        upr = bci$percent[[5]],
        cv = sd(b$t) / mean(b$t),
        biomass = calc_bio(.)
      )
    })
}

out <- boot_biomass(grid, reps = 50)
head(as.data.frame(out))

select(grid, -value) %>%
  mutate(omega_s = exp(omega_s)) %>%
  rename(value = omega_s) %>%
  calc_bio()

#######

check_bio <- function() {
  grid <- expand.grid(x = x, y = y, survey_sample = survey_sample) %>%
    mutate(grouping_code = paste(x, y, sep = '-')) %>%
    group_by(x, y) %>%
    mutate(
      sampling_x = x + runif(N, 0, 1/N),
      sampling_y = y + runif(N, 0, 1/N)
    ) %>% ungroup()

  grid$omega_s <- suppressMessages(
    RandomFields::RFsimulate(model = rf_omega,
      x = grid$sampling_x, y = grid$sampling_y)$variable1)

  grid <- grid %>%
    mutate(value = exp(rnorm(nrow(grid), omega_s, 0.3)) - (0.3^2)/2)

  true_biomass <- select(grid, -value) %>%
    mutate(omega_s = exp(omega_s)) %>%
    rename(value = omega_s) %>%
    calc_bio()

  out <- boot_biomass(grid, reps = 25)

  data.frame(out, true_biomass = true_biomass)
}

out <- purrr::map_df(seq(1:20), function(x) check_bio())

out %>%
  mutate(id = seq(1, nrow(out))) %>%
  ggplot(aes(id, true_biomass)) +
  geom_point() +
  geom_linerange(aes(ymin = lwr, ymax = upr))
