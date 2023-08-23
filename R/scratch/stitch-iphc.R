library(gfiphc)
library(tidyverse)
library(sdmTMB)
devtools::load_all()

# For now functions for iphc stitching specifically:
source(here::here('R', 'scratch', 'stitch-iphc-functions.R'))
# Load functions used to get pstar
source(here::here('R', 'scratch', 'cpois-gam-functions.R'))

# Set survey type (to follow same style as HBLL)
survey_type <- 'iphc'

# Caches -------------------------
dc <- file.path('report', 'data-cache-aug-2023')
iphc_dc <- file.path(dc, 'iphc')

# Stitch output cache
stitch_cache <- file.path('report', 'stitch-cache', survey_type)
dir.create(file.path(stitch_cache, 'cpois'), showWarnings = FALSE)

# Pstar cache
pstar_cache <- file.path('report', 'pstar-cache', survey_type)
dir.create(pstar_cache, showWarnings = FALSE)

# Prepare data --------------------
# hook data created in get-iphc-hook-data.R
iphc_hook <- readRDS(file.path(iphc_dc, 'iphc-hook-counts_1998-2022.rds')) |>
  select(-usable, -standard)

# Use 2017 grid for predictions (can be changed)
iphc_grid <- iphc_hook |>
  filter(year == 2017) |>
  select(year, station, lon, lat) |>
  sdmTMB::add_utm_columns(ll_names = c('lon', 'lat'))

#spp_list <- gfsynopsis::get_spp_names()$species_common_name |> sort()
spp_list <- 'pacific halibut'

# Clean data --------------------
spp_dat  <-
  spp_list |>
  map(\(sp) readRDS(file.path(iphc_dc, paste0(gfsynopsis:::clean_name(sp), ".rds")))$set_counts |>
              mutate(species_common_name = sp)) |>
  map(\(dat) prep_iphc_stitch_dat(sp_dat = dat, hook_dat = iphc_hook)) |>
  setNames(spp_list)

# Use only species with proportion of positive sets >= 5%
iphc_stitch_lu <- bind_rows(spp_dat) |>
  get_iphc_pos_sets() |>
  filter(mean_prop_pos >= 0.05) |>
  filter(species_common_name != 'tope shark') |> # GAM fails to fit, too few observations
  pull('species_common_name')


# Get pstar --------------------------------------------------------------------
gam_formula <- formula(catch ~ -1 + s(prop_removed) + fyear +
  s(fstation, bs = "re") +
  offset(log_eff_skate)
)

# This is slow
# pstar_list <-
#   spp_dat[iphc_stitch_lu] |> # Fit GAMs only for stitching species
#   map(\(dat) filter(dat, usable == "Y" & standard == "Y")) |>
#   map(\(dat) get_pstar(dat, gam_formula, survey_type = 'iphc', pstar_cache = pstar_cache))
# #beepr::beep()

# pstar_list <- iphc_stitch_lu |>
#   map(\(sp) readRDS(file.path(pstar_cache, paste0(gfsynopsis:::clean_name(sp), '.rds')))) |>
#   setNames(iphc_stitch_lu)

# pstar_plots <- names(pstar_list) |>
#   map(\(name) plot_pstar(pstar_list[[name]], sp_dat = spp_dat[[name]]))

# # Save pstar_df as it is easier to work with for stitching
# pstar_df <- pstar_list |>
#   map(\(x) mutate(x$pstar_df, species = x$species)) |>
#   bind_rows(.id = "species_common_name")
# saveRDS(pstar_df, file.path(pstar_cache, 'iphc', 'pstar-df.rds'))

# Plot pstar ---------------------
# save_plots_to_pdf(iphc_pstar_plots, file.path('report', 'pstar-cache',
#   'iphc', 'pstar-plots.pdf'), width = 5.6, height = 3.2)

# ------------------------------------------------------------------------------
# Stitch IPHC index using censored poisson where applicable
model_type <- "st-rw" # 'custom', 'st-rw-tv-rw'
pstar_df <- readRDS(file.path(pstar_cache, 'pstar-df.rds')) |>
  right_join(tibble(species_common_name = spp_list))

# # @NOTE: If species were not observed at a station in 2020 - 2022, they
# # Will have an NA value rather than 0. This may not matter given these are the
# # rare species and probably will not be stitched anyway.
# bind_rows(spp_dat) |>
#   filter(is.na(catch) | is.na(obsHooksPerSet) | is.na(prop_removed) | is.na(fyear), ) |>
#   distinct(year, station, lat, lon, .keep_all = TRUE) #%>% view()

# # @QUESTION: Can we use these data? "SOME/ALL SKATES LOST - DATA UNUSABLE FOR CPUE EST"
# # Looks like we have effective skate information?
# bind_rows(spp_dat) |> filter(usable == "N")

iphc_stitch_dat <-
  spp_dat |>
  map(\(dat) filter(dat, usable == "Y", standard == "Y", !is.na(catch))) |> # some species weren't measured at different points in time series
  keep(~ !is.null(.x) && nrow(.x) > 0) |> # After filtering some species df are empty
  map(\(dat) mutate(dat, obs_id = factor(row_number()))) |> # use (1 | obs_id) for poisson
  map(\(dat) left_join(dat, pstar_df, by = 'species_common_name')) |>
  map(\(dat) mutate(dat, pstar = ifelse(is.na(pstar), 1, pstar))) |>
  map(\(dat) upr = add_upr(dat, 'prop_removed', 'catch',
    'obsHooksPerSet', 'pstar'))

# GET INDEX
# ------------------------------------------------------------------------------
nbin2_index <-
  names(iphc_stitch_dat) |>
  map(\(sp) get_iphc_stitched_index(survey_dat = iphc_stitch_dat[[sp]], species = sp,
    form = 'catch ~ 1',
    family = sdmTMB::nbinom2(link = "log"),
    time = 'year',
    spatial = 'on',
    spatiotemporal = 'rw',
    model_type = 'st-rw',
    offset = 'log_eff_skate',
    gradient_thresh = 0.001,
    ctrl = sdmTMB::sdmTMBcontrol(),
    cutoff = 30,
    grid = iphc_grid, silent = FALSE,
    cache = file.path(stitch_cache, 'nbin2-no-hook'))
  )

index_list <-
  names(iphc_stitch_dat) |>
  map(\(sp) get_iphc_stitched_index(survey_dat = iphc_stitch_dat[[sp]], species = sp,
    form = 'catch ~ 1 + (1 | obs_id)',
    family = poisson(link = "log"),
    #family = sdmTMB::censored_poisson(link = "log"),
    time = 'year',
    spatial = 'on',
    spatiotemporal = 'rw',
    model_type = 'st-rw', # naming purposes only right now
    offset = 'log_eff_skate',
    gradient_thresh = 0.001,
    #ctrl = sdmTMB::sdmTMBcontrol(censored_upper = iphc_stitch_dat[[sp]]$upr),
    cutoff = 20,
    grid = iphc_grid, silent = FALSE,
    cache = file.path(stitch_cache, 'pois'))
  )

# ------------------------------------------------------------------------------

stats_family_lu <- iphc_stitch_dat |>
  map(\(dat) mutate(dat, family = ifelse(pstar == 1, 'poisson', 'censored poisson'))) |>
  bind_rows() |>
  distinct(species_common_name, pstar, family)

hfit_nb <- readRDS(file.path(stitch_cache, 'nbin2-no-hook/fits', paste0(('pacific-halibut'), '_', model_type, '.rds')))

hfit_cpois <- readRDS(file.path(stitch_cache, 'cpois/fits', paste0(('pacific-halibut'), '_', model_type, '.rds')))

nb_index_list <-
  names(iphc_stitch_dat) |>
  map(\(sp) readRDS(
      file.path(stitch_cache, 'nbin2-no-hook', paste0(clean_name(sp), '_', model_type, '.rds')))) |>
  setNames(names(iphc_stitch_dat)) |>
  keep(is.data.frame) |>
  imap(\(x, idx) mutate(x, survey_abbrev = survey_type, species_common_name = idx) |>
    mutate(family = 'nb2')
  )


index_list <-
  names(iphc_stitch_dat) |>
  map(\(sp) readRDS(
      file.path(stitch_cache, 'cpois', paste0(clean_name(sp), '_', model_type, '.rds')))) |>
  setNames(names(iphc_stitch_dat)) |>
  keep(is.data.frame) |>
  imap(\(x, idx) mutate(x, survey_abbrev = survey_type, species_common_name = idx) |>
    left_join(stats_family_lu) |>
  )

survey_cols <- c(
  RColorBrewer::brewer.pal(5L, "Set1"),
  RColorBrewer::brewer.pal(8L, "Set1")[7:8],
  "#303030", "#a8a8a8", "#a8a8a8", "#a8a8a8",
  "#a8a8a8", "#a8a8a8", "#a8a8a8", "#a8a8a8"
)
survey_col_names <- c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI",
    "HBLL OUT N", "HBLL OUT S", "IPHC FISS", "Commercial",
    "HBLL INS N", "HBLL INS S", "MSA HS",
    "SYN HS/QCS/WCHG/WCVI", "SYN HS/QCS/WCVI",
    "HBLL OUT N/S", "HBLL INS N/S"
    )

cpois_plots <- index_list |>
  map(\(index)
    gfplot::plot_survey_index(index,
          col = c("grey60", "grey20"), survey_cols = survey_cols,
          xlim = c(1984 - 0.2, 2022 + 0.2), french = FALSE) +
          scale_x_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE))
  ) |>
  imap(\(x, idx) x + ggtitle(paste0(index_list[[idx]]$species_common_name, '\n',
    #index_list[[idx]]$family)))

cpois_plots

nb2_plots <- nb_index_list |>
  map(\(index)
    gfplot::plot_survey_index(index,
          col = c("grey60", "grey20"), survey_cols = survey_cols,
          xlim = c(1984 - 0.2, 2022 + 0.2), french = FALSE) +
          scale_x_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE))
  ) |>
  imap(\(x, idx) x + ggtitle(paste0(nb_index_list[[idx]]$species_common_name, '\n',
    nb_index_list[[idx]]$family)))

nb2_plots
