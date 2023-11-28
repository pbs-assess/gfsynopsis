library(sdmTMB)
library(dplyr)
library(ggplot2)

d <- readRDS("report/data-cache-nov-2023/pacific-cod.rds")$survey_sets |>
  filter(survey_abbrev %in% "SYN QCS")

d <- gfsynopsis:::drop_duplicated_fe(d)

d <- select(d, year, catch_weight, doorspread_m, speed_mpm, duration_min, tow_length_m, latitude, longitude)

d <- add_utm_columns(d)

d <- mutate(d,
  area_swept1 = doorspread_m * (speed_mpm * duration_min),
  area_swept2 = tow_length_m * doorspread_m,
  area_swept = case_when(
    !is.na(area_swept2) ~ area_swept2,
    is.na(area_swept2) ~ area_swept1
  ))

mesh <- make_mesh(d, c("X", "Y"), cutoff = 10)
plot(mesh)
fit <- sdmTMB(
  catch_weight ~ 1,
  offset = log(d$area_swept),
  data = d,
  time = "year",
  mesh = mesh,
  spatial = "off",
  spatiotemporal = "rw",
  # extra_time = c(2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020),
  silent = FALSE,
  priors = sdmTMBpriors(
    matern_st = pc_matern(range_gt = 15, sigma_lt = 2),
    matern_s = pc_matern(range_gt = 15, sigma_lt = 2)
  ),
  family = tweedie()
)
fit

qcs <- gfplot::synoptic_grid |>
  filter(survey %in% "SYN QCS") |>
  select(X, Y) |>
  replicate_df("year", sort(union(fit$data$year, fit$extra_time)))
p <- predict(fit, newdata = qcs, return_tmb_object = TRUE)
ind <- get_index(p, bias_correct = TRUE, area = 4)

ggplot(ind, aes(year, est, ymin = lwr, ymax = upr)) +
  geom_ribbon() +
  geom_line(colour = "white")

r <- residuals(fit)
qqnorm(r)

# iid ----------------------

fit_iid <- sdmTMB(
  catch_weight ~ 0 + as.factor(year),
  offset = log(d$area_swept),
  data = d,
  time = "year",
  mesh = mesh,
  spatial = "on",
  spatiotemporal = "iid",
  silent = FALSE,
  priors = sdmTMBpriors(
    matern_st = pc_matern(range_gt = 15, sigma_lt = 2),
    matern_s = pc_matern(range_gt = 15, sigma_lt = 2)
  ),
  family = tweedie()
)
fit_iid

p_iid <- predict(fit_iid, newdata = qcs, return_tmb_object = TRUE)
ind_iid <- get_index(p_iid, bias_correct = TRUE, area = 4)

ggplot(ind_iid, aes(year, est, ymin = lwr, ymax = upr)) +
  geom_ribbon() +
  geom_line(colour = "white")

r_iid <- residuals(fit_iid)
qqnorm(r_iid)
