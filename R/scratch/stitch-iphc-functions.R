load_iphc_sp_dat <- function(species, iphc_data_cache) {
  sp_file <- paste0(gfsynopsis:::clean_name(species), '.rds')
  readRDS(file.path(iphc_data_cache, sp_file))$set_counts |>
  mutate(species = species)
}

prep_iphc_stitch_dat <- function(sp_dat, hook_dat) {
  clean_dat <-
    left_join(sp_dat, hook_dat, by = join_by('year', 'station', 'lat', 'lon')) |> # get observed hook counts
    # @QUESTION: Should we calculate hook values from effective skate for 1995?
    # For now for 1995, multiply effective skate number by 100, since and effective
    # skate of 1 is meant to represent 100 hooks (with a few other caveats).
    mutate(obsHooksPerSet = ifelse(year == 1995 & is.na(obsHooksPerSet), E_it * 100, obsHooksPerSet)) |>
    mutate(catch = ifelse(!is.na(N_it), N_it, N_it20),
           sample_n = ifelse(!is.na(N_it), 'whole_haul', '20_hook'),
           effSkate = ifelse(!is.na(N_it), E_it, E_it20),
           hook_removed = obsHooksPerSet - baited_hooks) |>
    mutate(prop_removed = hook_removed / obsHooksPerSet) |>
    mutate(fyear = factor(year), log_eff_skate = log(effSkate),
           fstation = factor(station)) # mgcv needs factor inputs
}

get_iphc_pos_sets <- function(clean_iphc_dat) {
  clean_iphc_dat |>
  mutate(present = case_when(catch > 0 ~ 1, catch == 0 ~ 0, TRUE ~ NA),
         measured = ifelse(!is.na(present), 1, 0)) |>
  group_by(species, year) |>
  summarise(n_sets = sum(measured), # get sets where we are pretty sure they counted this species
            n_pos  = sum(present, na.rm = TRUE),
            .groups = 'drop_last') |>
  dplyr::summarise(
      mean_n_pos = mean(n_pos), mean_n_sets = mean(n_sets),
      prop_pos = mean_n_pos / mean_n_sets,
      .groups = "drop"
    )
}

# get_pstar <- function(species, gam_cache = file.path('report', 'pstar-cache', 'iphc')) {
#   gam_path <- paste0(gfsynopsis:::clean_name(species), '_gam.rds')
#   gam_file <- file.path()
#   if (file.exists())
# }

# @FIXME: This function needs additional error checks. Or maybe make
# sdmTMB:::get_censored_upper act like `sdmTMB::add_utm_columns()`
add_upr_col <- function(dat, prop_removed_col, n_catch_col, n_hooks_col, pstar_col) {
  pstar <- unique(dat[[pstar_col]])
  na_catch <- sum(is.na(dat[[n_catch_col]]))
  stopifnot("\n\tError: missing catch values, filter before adding cpois upr" = (na_catch == 0))
  if (is.na(pstar) | pstar == 1) upr <- NA
  upr <-
    sdmTMB:::get_censored_upper(dat[[prop_removed_col]], dat[[n_catch_col]],
      dat[[n_hooks_col]], pstar)
}


fit_cpois_sdmtmb <- function(dat, f, st, sp, cutoff = 15, silent = FALSE) {
  species <- unique(dat$species)
  message("\tMaking mesh for species: ", species, " with cutoff: ", cutoff)
  dat <- sdmTMB::add_utm_columns(dat, ll_names = c('lon', 'lat'))
  mesh <- make_mesh(dat, xy_cols = c("X", "Y"), cutoff = cutoff)
  missing_years <- sdmTMB:::find_missing_time(dat$year)

  message("\tFitting censored_poisson for: ", species)
  try(
    fit <- sdmTMB(
      formula = f,
      family = censored_poisson(),
      time = "year",
      spatiotemporal = st,
      spatial = sp,
      mesh = mesh,
      data = dat,
      offset = 'log_eff_skate',
      control = sdmTMBcontrol(censored_upper = dat$upr),
      extra_time = missing_years,
      silent = silent)
  )
}