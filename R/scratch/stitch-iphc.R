library(gfiphc)
library(tidyverse)
library(sdmTMB)
devtools::load_all()

theme_set(theme_minimal())

# For now:
source(here::here('R', 'scratch', 'stitch-iphc-functions.R'))

dc <- file.path('report', 'data-cache-aug-2023')
iphc_data <- file.path(dc, 'iphc')
# hook data created in get-iphc-hook-data.R
iphc_hook <- readRDS(file.path(iphc_data, 'iphc-hook-counts_1998-2022.rds')) |>
  select(-usable, -standard)

pstar_df <- readRDS(file.path('report', 'pstar-cache', 'iphc', 'derivative-pstar-df.rds'))

# Choose grid for now
iphc_grid <- iphc_hook |>
  filter(year == 2017) |>
  select(year, station, lon, lat) |>
  sdmTMB::add_utm_columns(ll_names = c('lon', 'lat'))

spp_list <- gfsynopsis::get_spp_names()$species_common_name |> sort()
#spp_list <- "lingcod"

sp_dat <- spp_list |>
  map(\(species) load_iphc_sp_dat(species = species, iphc_data_cache = iphc_data))

clean_dat <- sp_dat |>
  map(\(dat) prep_iphc_stitch_dat(sp_dat = dat, hook_dat = iphc_hook)) |>
  enframe() |>
  unnest(col = 'value')

iphc_pos_sets <- clean_dat |>
  get_iphc_pos_sets() #|>
  #map(\(x) get_iphc_pos_sets(x)) |>
  #enframe() |>
  #unnest(col = 'value')

# Use only species with proportion of positive sets >= 5%
iphc_pos_sets_0.05 <- filter(iphc_pos_sets, prop_pos >= 0.05)

# @QUESTION: do we need to include sample type as factor in model?
# I.e., WHOLEHAUL vs 20HOOK?

# @FIXME/@NOTE: If species were not observed at a station in 2020 - 2022, they
# Will have an NA value rather than 0. This may not matter given these are the
# rare species and probably will not be stitched anyway.
clean_dat |>
  filter(is.na(catch) | is.na(obsHooksPerSet) | is.na(prop_removed) | is.na(fyear), ) |>
  distinct(year, station, lat, lon, .keep_all = TRUE) #%>% view()

# @QUESTION: Can we use these data? "SOME/ALL SKATES LOST - DATA UNUSABLE FOR CPUE EST"
# Looks like we have effective skate information?
clean_dat |> filter(usable == "N")

test_dat <-
  clean_dat |>
  filter(usable == "Y") |>
  filter(!is.na(catch)) |> # some species weren't measured at different points in time series
  filter(species %in% iphc_pos_sets_0.05$species) |>
  group_by(species) |>
  group_split() |>
  map(\(dat) left_join(dat, pstar_df, by = 'species')) |>
  map(\(dat) mutate(dat, upr = add_upr_col(dat, 'prop_removed', 'catch', 'obsHooksPerSet', 'pstar')))

test_dat[[1]]

# Fit
f <- formula(catch ~ 1 + (1 | fstation))
st <- 'rw'
sp <- 'on'

iphc_stitch <- test_dat[1] |>
  map(\(dat) fit_cpois_sdmtmb(dat, f = f, st, sp, cutoff = 20, silent = FALSE))

beepr::beep()