library(tidyverse)
library(gfiphc)
devtools::load_all()

dc <- here::here('report', 'data-cache-aug-2023')
iphc_data <- file.path(dc, 'iphc')

# Load species data (from `gfsynopsis::get_data()`)
sp <- 'pacific halibut'
sp_file <- paste0(gfsynopsis:::clean_name(sp), '.rds')
sp_dat <- readRDS(file.path(dc, 'iphc', sp_file))$set_counts |>
  mutate(species = sp)
# Get hook_bait counts matching GFBio species counts: 1995:2022
hook_bait <- readRDS(file.path(iphc_data, 'hook-with-bait.rds'))$set_counts |>
  mutate(baited_hooks = ifelse(!is.na(N_it), N_it, N_it20)) |>
  select(year, station, lat, lon, baited_hooks)
sp_dat <- left_join(sp_dat, hook_bait)

# Need total observed hook counts to calculate prop_removed
# ---------------------------------------------------------
# Get set information for years 2003:2012; 2014:2019; 2022
# iphc_set_info <- get_iphc_sets_info() # requires VPN connection
# saveRDS(iphc_set_info, 'iphc_set_info.rds')
iphc_set_info <- readRDS(here::here(dc, 'iphc', 'iphc_set_info.rds')) |>
  rename(lon = 'long')

# Missing hook data years:
unique(sp_dat$year)[!unique(sp_dat$year) %in% unique(iphc_set_info$year)]

# Need hook counts for 1996 - 2002
set_1996_2002 <-
  gfiphc::data1996to2002 |>
    mutate(species = tolower(spNameIPHC), station = as.character(station)) |>
    rename(N_it = 'catchCount', obsHooksPerSet = hooksObserved) |>
    select(year, station, lat, lon, obsHooksPerSet, usable)
# Add hook counts for 1996 - 2002
set_info <- bind_rows(iphc_set_info, set_1996_2002)

# What years do we have left to get
unique(sp_dat$year)[!unique(sp_dat$year) %in% unique(c(set_info$year))]

# No data for hook counts in 2013
gfiphc::setData2013

# No data for hook counts in 1995
gfiphc::setData1995

# Add hook counts for 2020 and 2021
set_2020_2021 <- bind_rows(gfiphc::setData2020, gfiphc::setData2021) |>
  rename(obsHooksPerSet = "hooksObs") |>
  select(year, station, lat, lon, obsHooksPerSet, usable, standard)

set_info <-
  bind_rows(iphc_set_info, set_1996_2002, set_2020_2021) %>%
  select(year, setID, station, obsHooksPerSet, effSkateIPHC, iphcUsabilityCode, iphcUsabilityDesc) %>%
  distinct(year, setID, station, .keep_all = TRUE)

# Add 2013 hook counts from raw IPHC FISS data download
  # Query data from: https://www.iphc.int/data/fiss-data-query with following parameters
#       # 1. Year Range: 1995 - 2022
#       # 2. IPHC Regulatory Areas: 2B & NULL (in case this field is NULL)
#       # 3. Purpose Codes: (All)
#       # 4. IPHC Charter Regions: (All)
#       # 5. NA
#       # 6. Select non-Pacific halibut species: (All)
iphc_raw_dat <- read_tsv('iphc-data/Non-Pacific halibut data_2B_NULL.tsv',
  locale = locale(encoding = "UTF-16LE"), guess_max = 10000)
iphc_raw_hal <- read_tsv('iphc-data/Set and Pacific halibut data_2B_NULL.tsv',
  locale = locale(encoding = "UTF-16LE")) # watchout for encoding!!!!
iphc_raw_hal <- iphc_raw_hal |>
  distinct(Date, Stlkey, `Effective skates hauled`, `Purpose Code`, Eff)

iphc_raw_2013 <- left_join(iphc_raw_dat, iphc_raw_hal) |>
  filter(Year == 2013) |>
  mutate(standard = ifelse(`Purpose Code` == "Standard Grid", "Y", "N"),
    usable = Eff) |>
  distinct(Year, Stlkey, Station, Setno, SampleType,
    HooksFished, HooksRetrieved, HooksObserved, `Effective skates hauled`,
    usable, standard) |>
  rename(year = "Year", station = "Station", obsHooksPerSet = "HooksObserved",
    effSkateIPHC = "Effective skates hauled") |>
  mutate(E_it20 = effSkateIPHC * obsHooksPerSet / HooksRetrieved) |>
  select(year, station, obsHooksPerSet, effSkateIPHC) |>
  mutate(station = as.character(station))

set_info <- bind_rows(iphc_set_info, set_1996_2002, set_2020_2021, iphc_raw_2013) |>
  select(year, setID, station, lat, lon, obsHooksPerSet, effSkateIPHC,
    iphcUsabilityCode, iphcUsabilityDesc) |>
  distinct() |>
  arrange(year, station)

unique(sp_dat$year)[!unique(sp_dat$year) %in% unique(c(set_info$year))]

# Combine set information with species count data
sp_with_hooks <- left_join(sp_dat,
  set_info |> select(-lat, -lon) # Not all datasets have this information for the join
  ) |>
 select(-species, -(E_it:C_it20))

# test1 <- slice(sp_dat, 4051)
# slice(sp_dat, 4051)
# filter(sp_dat, year == test1$year, station == test1$station) |> data.frame()

# test2 <- slice(set_info, 3958)
# slice(set_info, 3958)
# filter(set_info, year == test2$year, station == test2$station) |> data.frame()

# Resolve the many-to-many by using lat/lon as additional key values
year2019_stations <-
  left_join(
    filter(sp_dat, (year == 2019 & station %in% c("2099", "2107"))),
    filter(set_info, (year == 2019 & station %in% c("2099", "2107")))
  ) |>
  select(all_of(colnames(sp_with_hooks)))

iphc_hook_out <-
  sp_with_hooks |>
  # simplest to remove unresolved many-to-many and add proper values in
  filter(!(year == 2019 & station %in% c("2099", "2107"))) |>
  bind_rows(year2019_stations) |>
  arrange(year, station)

saveRDS(iphc_hook_out, file.path(iphc_data, 'iphc-hook-counts_1998-2022.rds'))

stop()



# # Start exploring use of raw data to get hook counts for 1995 and 2013
# # ------------------------------------------------------------------------------
# # Originally downloaded all of the halibut and non-halibut species data from
# # IPHC FISS.
# # Andy did a lot of work to make sure the recent data (well all the data really)
# # jive with the pre 2019 data. So we'l
# # --------
# # Query data from: https://www.iphc.int/data/fiss-data-query with following parameters
#       # 1. Year Range: 1995 - 2022
#       # 2. IPHC Regulatory Areas: 2B & NULL (1996 does not have area associated)
#       # 3. Purpose Codes: (All)
#       # 4. IPHC Charter Regions: (All)
#       # 5. NA
#       # 6. Select non-Pacific halibut species: (All)
# # Wrangle halibut so it can be added to the non-halibut data
# iphc_raw_hal <- read_tsv('iphc-data/Set and Pacific halibut data_2B_NULL.tsv',
#   locale = locale(encoding = "UTF-16LE")) # watchout for encoding!!!!
# iphc_raw_hal <- iphc_raw_hal |>
#   mutate(halibut_count = `O32 Pacific halibut count` + `U32 Pacific halibut count`) |>
#   select(Year, Date, Stlkey, `Vessel code`, Eff, Ineffcde, `IPHC Reg Area`,
#     halibut_count, # keep this for quality control for now
#     `Effective skates hauled`,
#     BeginLat, BeginLon, EndLat, EndLon, `MidLat fished`, `MidLon fished`,
#     `Lat - Grid target`, `Lon - Grid target`) # `MidLat fished`/`MidLon fished` is equivalent to gfiphc lat/lon

# # Combine the wrangled halibut data with non-halibut species
# iphc_raw_dat <- read_tsv('iphc-data/Non-Pacific halibut data_2B_NULL.tsv',
#   locale = locale(encoding = "UTF-16LE"), guess_max = 10000)
# range(iphc_raw_dat$Year)
# # Add halibut data in same format as other species
# raw_hal_counts <- iphc_raw_hal |>
#   mutate(`Species Name` = "Pacific Halibut") |>
#   rename(`Number Observed` = "halibut_count") |>
#   select(Year, Stlkey, #Date, Stlkey, `Vessel code`, Eff, Ineffcde, `IPHC Reg Area`,
#     `Species Name`, `Number Observed`) |>#,
#     # BeginLat, BeginLon, EndLat, EndLon, `MidLat fished`, `MidLon fished`,
#     # `Lat - Grid target`, `Lon - Grid target`) |>
#   left_join(distinct(iphc_raw_dat, Year, Stlkey, Station, Setno, SampleType, HooksFished, HooksRetrieved, HooksObserved, `Effective skates hauled`))

# iphc_raw_dat <- iphc_raw_dat |>
#   bind_rows(raw_hal_counts) |>
#   arrange(Year, Stlkey)

# iphc_raw_dat <-
#   left_join(iphc_raw_dat, iphc_raw_hal, by = c('Year', 'Stlkey')) |>
#   filter(Year >= 1998) # No useful hook data before this




# # ----
# # Select hook counts and other identifying columns
# iphc_raw_subset <- iphc_raw_dat |>
#   select(Year, Station, Stlkey, SampleType, Date, `Vessel code`,
#     `MidLat fished`, `MidLon fished`, HooksObserved, HooksRetrieved, Eff, Ineffcde,
#     halibut_count, `Effective skates hauled`) |>
#   distinct() |>
#   rename(year = "Year", station = "Station", date = "Date") |>
#   mutate(station = as.character(station))

# # Calculate hook counts based on sum of observed fish/hooks/bait
# raw_group_obs_counts <- iphc_raw_dat |>
#   group_by(Stlkey) |>
#   summarise(Stlkey_spp_counts = sum(`Number Observed`), .groups = "drop")

# iphc_raw_subset <- left_join(iphc_raw_subset, raw_group_obs_counts)

# iphc_raw_2013 <- filter(iphc_raw_subset, year == 2013)

# # Find many-to-many relationships (should not be many-to-many)
# sp_with_hooks_raw <- left_join(sp_dat, iphc_raw_subset)
# dim(sp_with_hooks_raw)

# # Investigate
# # Turns out two stations were sampled twice in 1 year
# # ℹ Row 4051 of `x` matches multiple rows in `y`.
# slice(sp_dat, 4051)
# filter(sp_with_hooks_raw, year == 2019, station == "2099")
# filter(sp_dat, year == 2019, station == "2099")

# #ℹ Row 613 of `y` matches multiple rows in `x`.
# slice(sp_with_hooks_raw, 3620)
# filter(sp_with_hooks_raw, year == 2017, station == "2073")
# filter(sp_dat, year == 2017, station == "2073")

# # Use date to keep unique fishing events that match data in GFBio
# sp_with_hooks_raw <- sp_with_hooks_raw %>%
#   distinct(year, station, date, .keep_all = TRUE)

# nrow(sp_with_hooks_raw) == nrow(sp_dat)


# sp_with_hooks_raw %>% names()

# sp_with_hooks %>% names()

# # Compare observed hook counts between GFBio and gfiphc data with raw data from
# # the IPHC FISS data portal
# test <-
# sp_with_hooks %>%
#   filter(year >= 1998) %>%
#   select(year, station, N_it, N_it20,
#     obsHooksPerSet, effSkateIPHC, iphcUsabilityCode, iphcUsabilityDesc, usable)

# test2 <-
#   sp_with_hooks_raw %>%
#   filter(year >= 1998) %>%
#     select(year, station, SampleType, date, HooksObserved, HooksRetrieved, Eff, Ineffcde, Stlkey_spp_counts, halibut_count, `Effective skates hauled`)

# test3 <-
# left_join(test, test2, by = c('year', 'station')) %>%
#   mutate(diff = obsHooksPerSet - HooksObserved) %>%
#   mutate(diff2 = obsHooksPerSet - Stlkey_spp_counts) %>%
#   select(year, station, date, SampleType, obsHooksPerSet, HooksObserved, Stlkey_spp_counts,
#          diff, diff2, halibut_count, N_it, N_it20, Eff, Ineffcde, usable, iphcUsabilityDesc,
#          effSkateIPHC, `Effective skates hauled`, HooksRetrieved) %>%
#   arrange(desc(abs(diff)))
# test3 %>%
#   write_csv('scratch-out/test.csv')

# filter(test3, year == 2013) %>% view()

# # More many to many
# slice(test, 3931)
# slice(test2, 3931)


# filter(sp_dat, year == 1998, station == "2070")

# filter(sp_with_hooks, year == 2004, station == "2166") %>% as.data.frame()

# test

# iphc_raw_dat %>% filter(Year == 2006, Station == 2034) |> view()

# iphc_raw_dat %>% filter(Stlkey == 20060114) |> view()
# iphc_raw_hal %>% filter(Stlkey == 20060114) |> view()

# sp_dat %>% filter(station == '2034', year == 2006)