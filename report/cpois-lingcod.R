#source(here::here('R', 'scratch', 'cpois-gam-functions.R'))
# library(sdmTMB)
devtools::load_all("../sdmTMB") # note path

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

spp_list <- "lingcod"
#spp_list <- "arrowtooth flounder"
spp_files <- paste0(gfsynopsis:::clean_name(spp_list), '.rds')

dat <- lapply(spp_list, FUN = function(sp) {
  sp_file <- paste0(gfsynopsis:::clean_name(sp), '.rds')
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

# For now, work with N_it20 values and then can compare effect of sampling design
test_dat <- dat %>% filter(year >= 2003) |>
  filter(sample_n == "all") %>% # Have to choose an option because Andy has scaled down the
  filter(iphcUsabilityCode %in% c(1, 3), standard == "Y") |> # Match Joe's data use filtering
  mutate(prop_removed = hook_removed / obsHooksPerSet) |>
  add_utm_columns(ll_names = c("lon", "lat"))

mesh <- make_mesh(test_dat, xy_cols = c("X", "Y"), cutoff = 15)
missing_years <- sdmTMB:::find_missing_time(test_dat$year)



fit1 <- sdmTMB(
  formula = catch ~ 0 + as.factor(year), #catch ~ 1,
  family = poisson(),
  # time = "year", spatiotemporal = "rw", spatial = "on",
  mesh = mesh, data = test_dat, offset = 'log_eff_skate',
  #extra_time = missing_years
  silent = FALSE
)
# saveRDS(fit1, here::here('scratch-out', 'fit1.rds'))
#fit1 <- readRDS(here::here('scratch-out', 'fit1.rds'))

pstar <- 0.823 # lingcod
#pstar <- 0.920 # arrowtooth

test <-
test_dat |>
  mutate(upr = sdmTMB:::get_censored_upper(prop_removed, n_catch = catch, n_hooks = obsHooksPerSet, pstar = pstar))
  #mutate(upr = ifelse(upr > catch, floor(upr / 4), upr))
  #mutate(upr = ifelse(prop_removed > pstar, catch + 20, catch))

lwr <- test_dat$catch
upr <- test_dat$catch
pstar <- 0.99 # lingcod
upr[which(test_dat$prop_removed > pstar)] <- upr[which(test_dat$prop_removed > pstar)] + 10
plot(test_dat$catch, upr)
table(test_dat$catch == upr) / nrow(test_dat)

quantile(test_dat$prop_removed)

# b <- sdmTMB::get_pars(fit1)

# lwr <- test_dat$catch
# upr <- test_dat$catch
#
# upr[200]<- test_dat$catch[200] + 10

# upr ==
fit2 <- sdmTMB(
  formula = catch ~ 0 + as.factor(year), #catch ~ 1,
  family = censored_poisson(),
  # family = poisson(),
  time = "year",
  spatiotemporal = "iid", spatial = "on",
  mesh = mesh,
  data = test_dat,
  #offset = 'log_eff_skate',
  experimental = list(lwr = lwr, upr = upr),
  # control = sdmTMBcontrol(start = list(b_j = b$b_j)),
  #control = sdmTMB::sdmTMBcontrol(nlminb_loops = 1L, newton_loops = 1L),
  silent = F
)
fit2
