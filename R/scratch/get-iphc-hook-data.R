library(tidyverse)
library(gfiphc)
devtools::load_all()

dc <- here::here('report', 'data-cache-aug-2023')
iphc_data <- file.path(dc, 'iphc')

# Load species data (from `gfsynopsis::get_data()`)
sp <- 'pacific halibut' # Use single example species
#sp <- 'north pacific spiny dogfish' # Use single example species (as expected this does not matter)
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
  rename(lon = 'long') |>
  filter(year != 2022) # these hook counts are wrong from GFBio

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
set_2020_2021_2022 <- bind_rows(gfiphc::setData2020, gfiphc::setData2021, gfiphc::setData2022) |>
  rename(obsHooksPerSet = "hooksObs") |>
  select(year, station, lat, lon, obsHooksPerSet, usable, standard)

set_info <-
  bind_rows(iphc_set_info, set_1996_2002, set_2020_2021_2022) %>%
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
iphc_raw_dat <- read_tsv(file.path(iphc_data, 'Non-Pacific halibut data_2B_NULL.tsv'),
  locale = locale(encoding = "UTF-16LE"), guess_max = 10000)
iphc_raw_hal <- read_tsv(file.path(iphc_data, 'Set and Pacific halibut data_2B_NULL.tsv'),
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

set_info <- bind_rows(iphc_set_info, set_1996_2002, set_2020_2021_2022, iphc_raw_2013) |>
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
iphc_raw_hal <- read_tsv(file.path(iphc_data, 'Set and Pacific halibut data_2B_NULL.tsv'),
  locale = locale(encoding = "UTF-16LE")) |>
  select(-`Row number`) |>
  mutate(halibut_count = `O32 Pacific halibut count` + `U32 Pacific halibut count`)
# # Combine the wrangled halibut data with non-halibut species
iphc_raw_dat <- read_tsv(file.path(iphc_data, 'Non-Pacific halibut data_2B_NULL.tsv'),
  locale = locale(encoding = "UTF-16LE"), guess_max = 10000)
range(iphc_raw_dat$Year)

raw_dat <- left_join(iphc_raw_dat, iphc_raw_hal)

# Add halibut data in same format as other species
raw_hal_counts <-
  iphc_raw_hal |>
  mutate(`Species Name` = "Pacific Halibut") |>
  mutate(`Number Observed` = halibut_count) # keep this for quality control for now

dat <- bind_rows(raw_dat, raw_hal_counts) |>
  filter(Year >= 1998) |>
  arrange(Year, Date, Stlkey, SampleType, `Species Name`) |>
  select(-c(`IPHC Charter Region`,
            `O32 Pacific halibut count`, `U32 Pacific halibut count`,
            `O32 Pacific halibut weight`, `U32 Pacific halibut weight`)) |>
  select(-(`Profiler Lat`:Oxygen_sat))

total_obs <-
  dat |>
    group_by(Stlkey, Station, Setno, Year) |>
    summarise(total_obs = sum(`Number Observed`),
      SampleType = first(SampleType),
      HooksFished = first(HooksFished),
      HooksRetrieved = first(HooksRetrieved),
      HooksObserved = first(HooksObserved),
      SkatesHauled = first(`No. skates hauled`),
      EffSkates = first(`Effective skates hauled`),
      halibut_count = first(halibut_count),
      .groups = "drop") |>
    mutate(obs_diff = HooksObserved - total_obs) |>
    arrange(desc(abs(obs_diff))) |>
    mutate(Station = as.character(Station))

view(slice(total_obs %>% filter(SampleType != '20Hook'), 1:200))

gfiphc_df <- readRDS(file.path(iphc_data, 'iphc-hook-counts_1998-2022.rds')) |>
  filter(year >= 1998)

# Ignore many to many problem for now. Probably that 2019 business
test <- left_join(total_obs, gfiphc_df, by = c('Year' = 'year', 'Station' = 'station'))

# Use dogfish as the base dataset (this doesn't matter)
test2 <- left_join(total_obs, iphc_hook_out, by = c('Year' = 'year', 'Station' = 'station'))
test2 <- left_join(total_obs, iphc_set_info, by = c('Year' = 'year', 'Station' = 'station'))


#test |>
test2 |>
  mutate(iphc_minus_gf_obs = HooksObserved - obsHooksPerSet) |>
  arrange(desc(abs(iphc_minus_gf_obs))) |>
  filter(Year != 2012) |>
  slice(1:100) |>
  select(-iphcUsabilityCode, -iphcUsabilityDesc, -tripID, -setID) |>
  view()

view(slice(total_obs %>% filter(SampleType != '20Hook' & Year != 2012), 1:200))

# Questions for Andy
# - I think there is 2022 data in GFBio that shouldn't be there and the observed
#   hook counts there are wrong
# - There are 50 stations where the raw FISS data and GFBio hook count data
#   do not match by ~100 hooks
# - There are quite a few random cases where there is a diff of 10 or more.
#   7 of these are diffs > 20; 3 of these are 56, 74, and 75.