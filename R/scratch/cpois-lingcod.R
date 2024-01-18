#source(here::here('R', 'scratch', 'cpois-gam-functions.R'))
library(sdmTMB)
#devtools::load_all(here::here('..', 'sdmTMB'))
library(tictoc)
library(tidyverse)
devtools::load_all()
# ---------
# Try fitting model
dc <- here::here('report', 'data-cache-july-2023')
# iphc_set_info <- get_iphc_sets_info() # requires VPN connection
# saveRDS(iphc_set_info, 'iphc_set_info.rds')
iphc_set_info <- readRDS(file.path(dc, 'iphc', 'iphc_set_info.rds')) |>
  rename(lon = 'long')
hook_bait <- readRDS(file.path(dc, 'iphc', 'hook-with-bait.rds'))$set_counts |>
  mutate(baited_hooks = ifelse(!is.na(N_it), N_it, N_it20)) |>
  select(year, station, lat, lon, baited_hooks)

iphc_grid <- iphc_set_info |>
  filter(year == 2017) |>
  select(year, station, lon, lat) |>
  add_utm_columns(ll_names = c('lon', 'lat'))

# ----------
# Use the same species as Joe to compare results
# spp_list <- c("yelloweye rockfish",
#             "arrowtooth flounder",
#             "lingcod",
#             "north pacific spiny dogfish",
#             "pacific cod",
#             "pacific halibut",
#             "redbanded rockfish",
#             "sablefish",
#             "big skate",
#             "longnose skate",
#             "shortspine thornyhead")
#spp_list <- "pacific halibut"
#spp_list <- "arrowtooth flounder"
spp_list <- "lingcod"
spp_files <- paste0(clean_name(spp_list), '.rds')

dat <- lapply(spp_list, FUN = function(sp) {
  sp_file <- paste0(clean_name(sp), '.rds')
  readRDS(file.path(dc, 'iphc', sp_file))$set_counts |>
  mutate(species = sp)
}) |>
  enframe() |>
  unnest(cols = value)

dat <- left_join(dat, hook_bait) |>
  left_join(x = _, y = iphc_set_info) |> # get observed hook counts
  mutate(catch = ifelse(!is.na(N_it), N_it, N_it20),
         sample_n = ifelse(!is.na(N_it), 'all', '20'),
         effSkate = ifelse(!is.na(N_it), E_it, E_it20),
         hook_removed = obsHooksPerSet - baited_hooks) |>
  mutate(prop_removed = hook_removed / obsHooksPerSet) |>
  #mutate(effSkate = ifelse(sample_n == 'N_it', E_it, E_it20)) |>
  # @FIXME need to get total observed hooks for 1995 - 2003
  mutate(fyear = factor(year), log_eff_skate = log(effSkate),
         fstation = factor(station)) # mgcv needs factor inputs

dat <- dat |> select(
  year, station, lat, lon, sample_n,
  species, catch, effSkate, hook_removed,
  obsHooksPerSet, iphcUsabilityCode, standard,
  prop_removed, log_eff_skate,
  fyear, fstation
)

# For now, work with N_it20 values and then can compare effect of sampling design
test_dat <- dat %>% filter(year >= 2003) |>
  filter(sample_n == "all") |> # Have to choose an option because Andy has scaled down the
  filter(iphcUsabilityCode %in% c(1, 3), standard == "Y") |> # Match Joe's data use filtering
  sdmTMB::add_utm_columns(ll_names = c("lon", "lat"))

pstar <- 0.823 # lingcod
#pstar <- 0.920 # arrowtooth
#pstar <- 0.99 # halibut

test_dat <- test_dat |>
  mutate(upr = sdmTMB:::get_censored_upper(prop_removed, n_catch = catch,
    n_hooks = obsHooksPerSet, pstar = pstar)) |>
  mutate(pstar = pstar, cens = prop_removed > pstar)
  #mutate(upr = ifelse(upr > catch, floor(upr / 4), upr))
  #mutate(upr = ifelse(prop_removed > pstar, catch + 20, catch))


mesh <- make_mesh(test_dat, xy_cols = c("X", "Y"), cutoff = 15)
missing_years <- sdmTMB:::find_missing_time(test_dat$year)

quantile(test_dat$prop_removed)
stop()

message(spp_list)
#f <- formula(catch ~ 0 + as.factor(year))
f <- formula(catch ~ 1)
st <- 'iid'#'rw'
sp <- 'off'

fit1 <- sdmTMB(
  formula = f,
  family = poisson(),
  time = "year", spatiotemporal = st, spatial = sp,
  mesh = mesh, data = test_dat, offset = 'log_eff_skate',
  extra_time = missing_years,
  silent = FALSE
)

# saveRDS(fit1, here::here('scratch-out', 'fit1.rds'))
#fit1 <- readRDS(here::here('scratch-out', 'fit1.rds'))

fit2 <- sdmTMB(
  formula = f,
  family = censored_poisson(),
  time = "year",
  spatiotemporal = st,
  spatial = sp,
  mesh = mesh,
  data = test_dat,
  offset = 'log_eff_skate',
  #experimental = list(lwr = test_dat$lwr, upr = test_dat$upr),
  control = sdmTMBcontrol(censored_upper = test_dat$upr),
  extra_time = missing_years,
  #control = sdmTMB::sdmTMBcontrol(nlminb_loops = 1L, newton_loops = 1L),
  silent = FALSE)

fit3 <- sdmTMB(
  formula = f,
  family = censored_nbinom2(),
  time = "year",
  spatiotemporal = st,
  spatial = sp,
  mesh = mesh,
  data = test_dat,
  offset = 'log_eff_skate',
  #experimental = list(lwr = test_dat$lwr, upr = test_dat$upr),fit3
  control = sdmTMBcontrol(censored_upper = test_dat$upr),
  extra_time = missing_years,
  #control = sdmTMB::sdmTMBcontrol(nlminb_loops = 1L, newton_loops = 1L),
  silent = FALSE)
beepr::beep()

sanity(fit1)
sanity(fit2)
sanity(fit3)

min_year <- min(test_dat$year)
max_year <- max(test_dat$year)
newdata <- sdmTMB::replicate_df(iphc_grid, "year", unique(test_dat$year)) |>
  mutate(fstation = factor(station))

fit1_pred <- sdmTMB:::predict.sdmTMB(object = fit1, newdata = newdata, type = "link", return_tmb_object = TRUE)
fit2_pred <- sdmTMB:::predict.sdmTMB(object = fit2, newdata = newdata, type = "link", return_tmb_object = TRUE)
beepr::beep()

fit1_ind <- sdmTMB::get_index(obj = fit1_pred, bias_correct = TRUE) |> mutate(dist = 'poisson')
fit2_ind <- sdmTMB::get_index(obj = fit2_pred, bias_correct = TRUE) |> mutate(dist = 'censored')
inds <- bind_rows(fit1_ind, fit2_ind)

ggplot(data = inds, aes(x = year, y = est, fill = dist)) +
  geom_line(aes(colour = dist)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2,) +
  cowplot::theme_cowplot() +
  theme(plot.background = element_rect(fill = 'white')) +
  ggtitle(paste0(spp_list, ' (p* = ', pstar, '): ', format(f)))


ggplot(data = fit1_pred$data, aes(x = X, y = Y)) +
  geom_point(aes(colour = est)) +
  scale_colour_viridis_c() +
  facet_wrap(~ year) +
  ggtitle(paste0(spp_list, ' (p* = ', pstar, '): ', format(f), "; st = ", st, ' sp = ', sp))

# What about using the total number of hooks as the upper limit instead of
# calculating an upper limit
