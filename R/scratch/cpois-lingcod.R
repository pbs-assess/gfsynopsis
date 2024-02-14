#source(here::here('R', 'scratch', 'cpois-gam-functions.R'))
library(sdmTMB)
#devtools::load_all(here::here('..', 'sdmTMB'))
library(tictoc)
library(tidyverse)
devtools::load_all()
# ---------
# Try fitting model
dc <- here::here('report', 'data-cache-nov-2023')
# iphc_set_info <- get_iphc_sets_info() # requires VPN connection
# saveRDS(iphc_set_info, 'iphc_set_info.rds')
iphc_set_info <- readRDS(file.path(dc, 'iphc', 'iphc-set-info.rds')) |>
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
  unnest(cols = value) |>
  # Combine with bait counts
  left_join(hook_bait) |>
  left_join(iphc_set_info) |> # get observed hook counts
  mutate(catch_count = ifelse(!is.na(N_it), N_it, N_it20),
         sample_n = ifelse(!is.na(N_it), 'all', '20'),
         effSkate = ifelse(!is.na(N_it), E_it, E_it20),
         hook_removed = obsHooksPerSet - baited_hooks) |>
  mutate(prop_removed = hook_removed / obsHooksPerSet) |>
  #mutate(effSkate = ifelse(sample_n == 'N_it', E_it, E_it20)) |>
  # @FIXME need to get total observed hooks for 1995 - 2003
  mutate(fyear = factor(year), log_eff_skate = log(effSkate),
         fstation = factor(station)) |> # mgcv needs factor inputs
  drop_na(catch_count, effSkate, prop_removed)

dat <- dat |> select(
  year, station, lat, lon, sample_n,
  species, catch_count, effSkate, hook_removed,
  obsHooksPerSet, iphcUsabilityCode, standard,
  prop_removed, log_eff_skate,
  fyear, fstation
) |>
# For now, work with N_it20 values and then can compare effect of sampling design
  filter(year >= 2003) |>
  filter(sample_n == "all") |> # Have to choose an option because Andy has scaled down the
  filter(iphcUsabilityCode %in% c(1, 3), standard == "Y") |> # Match Joe's data use filtering
  sdmTMB::add_utm_columns(ll_names = c("lon", "lat")) |>
  drop_na(obsHooksPerSet)

        !is.na(data$N_dat) &
          !is.na(data$effSkateIPHC) &
          !is.na(data$region_INLA) & !is.na(data$prop_removed)

pstar <- 0.823 # lingcod
#pstar <- 0.920 # arrowtooth
#pstar <- 0.99 # halibut

d <- dat |>
  mutate(pstar = pstar, cens = prop_removed > pstar) |>
  mutate(obs_id = as.factor(seq(1, n()))) # Account for variance constraint when using Poisson
  #mutate(upr = ifelse(upr > catch_count, floor(upr / 4), upr))
  #mutate(upr = ifelse(prop_removed > pstar, catch_count + 20, catch_count))
d$upr <- sdmTMB:::get_censored_upper(
  prop_removed = d$prop_removed,
  n_catch = d$catch_count,
  n_hooks = d$obsHooksPerSet,
  pstar = pstar)
# Note to self... Joe's event_id is obs_id here...

#mesh <- make_mesh(d, xy_cols = c("X", "Y"), cutoff = 15)
mesh <- make_mesh(d, xy_cols = c("X", "Y"), n_knots = 3)
missing_years <- sdmTMB:::find_missing_time(d$year)

quantile(d$prop_removed)

message(spp_list)
f <- formula(catch_count ~ fyear + (1 | obs_id))
f <- formula('catch_count ~ -1 + year + (1 | obs_id) + (1 | fstation)')
#f <- formula(catch_count ~ 1)
st <- 'iid'#'rw'
sp <- 'off'

missing_years <- 2013L

fit1 <- sdmTMB(
  formula = f,
  family = poisson(),
  time = "year", spatiotemporal = st, spatial = sp,
  mesh = mesh, data = d, offset = 'log_eff_skate',
  extra_time = missing_years,
  silent = FALSE
)

# saveRDS(fit1, here::here('scratch-out', 'fit1.rds'))
#fit1 <- readRDS(here::here('scratch-out', 'fit1.rds'))

fit2 <- sdmTMB(
  formula = f,
  family = censored_poisson(link = 'log'),
  time = "year",
  spatiotemporal = st,
  spatial = sp,
  mesh = mesh,
  data = d,
  offset = 'log_eff_skate',
  control = sdmTMBcontrol(censored_upper = d$upr),
  extra_time = missing_years,
  silent = FALSE)

fit3 <- sdmTMB(
  formula = f,
  family = censored_nbinom2(),
  time = "year",
  spatiotemporal = st,
  spatial = sp,
  mesh = mesh,
  data = d,
  offset = 'log_eff_skate',
  #experimental = list(lwr = d$lwr, upr = d$upr),fit3
  control = sdmTMBcontrol(censored_upper = d$upr),
  extra_time = missing_years,
  #control = sdmTMB::sdmTMBcontrol(nlminb_loops = 1L, newton_loops = 1L),
  silent = FALSE)
beepr::beep()

sanity(fit1)
sanity(fit2)
sanity(fit3)

min_year <- min(d$year)
max_year <- max(d$year)
newdata <- sdmTMB::replicate_df(iphc_grid, "year", unique(d$year)) |>
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
