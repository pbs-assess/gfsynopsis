# Goal:
# Create a code pipeline from IPHC website .xlsx file downloads for
# all years combined into a dataset for non-halibut species that can be used
# for spatiotemporal index standardization and matches the PBS database
# species names.

# Instructions for updating in future years:
# - head to <https://www.iphc.int/data/fiss-survey-raw-survey-data/>
# - expand the year range from 1998 to current year
# - from the dropdown, select '2B' from 'IPHC Regulatory Areas'
# - leave everything else at defaults
# - click the little square with a down arrow towards the bottom
# - select 'Crosstab'
# - leave the default Excel format
# - click on 'Non-Pacific halibut data' and wait for the download
# - repeat for 'Set and Pacific halibut data'
# - move these 2 .xlsx files into this folder
# - Set the date you did this download here:
DOWNLOAD_DATE <- "2024-05-23"
# - run the following code

# There shouldn't be any tweaks needed unless the IPHC makes changes to their
# survey design (which I'm sure they will!).
# Inspect the plots at the end and make sure you're happy with the data.

library(dplyr)
library(ggplot2)
theme_set(theme_light())


# read in and clean up IPHC .xlsx downloads ---------------------------------

# 2023-05-23 download (2B):
d <- readxl::read_xlsx("report/iphc-simple/Non-Pacific halibut data.xlsx")
names(d) <- tolower(names(d))
names(d) <- gsub(" ", "_", names(d))
d$scientific_name <- tolower(d$scientific_name)
d$species_name <- tolower(d$species_name)
d$scientific_name[d$scientific_name == "raja binoculata"] <- "beringraja binoculata"
d$scientific_name[d$scientific_name == "bathyraja kincaida"] <- "bathyraja interrupta"

spp <- gfsynopsis::get_spp_names()
missing <- filter(d, !scientific_name %in% spp$species_science_name)
sort(unique(missing$species_name))
# - blackspotted rockfish [ignore]
# - rougheye rockfish [ignore]

count_dat <- filter(d, scientific_name %in% spp$species_science_name)
sort(unique(count_dat$species_name))

if (FALSE) {
  # a helper to find scientific names to fix above
  TEST <- "sandpaper skate"
  filter(d, species_name == TEST) |>
    select(scientific_name) |>
    unique()
  filter(spp, species_common_name == TEST) |> select(species_science_name)
}

count_dat <- transmute(count_dat,
  year = as.integer(year),
  species_common_name = species_name,
  species_science_name = scientific_name,
  # hooks_fished = as.integer(hooksfished), # excluding for now
  # hooks_retrieved = as.integer(hooksretrieved), # excluding for now
  hooks_observed = as.integer(hooksobserved),
  number_observed = as.integer(number_observed),
  sample_type = if_else(sampletype == "20Hook", "20 hooks", "all hooks"),
  # set_number = as.integer(setno), # excluding for now
  station = as.integer(station),
  station_key = as.integer(stlkey)
)

# need to collapse bering skate and sandpaper skate into one species:
# bathyraja interrupta
count_dat <- count_dat |>
  group_by(year, species_science_name, sample_type, station, station_key) |>
  summarise(
    hooks_observed = sum(hooks_observed),
    number_observed = sum(number_observed),
    species_common_name = species_common_name[1], # pick one for now; the DFO ones get joined anyways
    .groups = "drop"
  )

# all stations; 2B
set <- readxl::read_xlsx("report/iphc-simple/Set and Pacific halibut data.xlsx")
names(set) <- tolower(names(set))
names(set) <- gsub(" ", "_", names(set))
names(set) <- gsub("\\(", "", names(set))
names(set) <- gsub("\\)", "", names(set))
names(set) <- gsub("\\-", "_", names(set))
names(set) <- gsub("\\/", "_per_", names(set))
names(set) <- gsub("\\.", "", names(set))
names(set) <- gsub("___", "_", names(set))
set$date <- lubridate::dmy(set$date)

set |>
  ggplot(aes(midlon_fished, midlat_fished, colour = purpose_code)) +
  geom_point() +
  facet_wrap(~year)

table(set$purpose_code)
# set <- filter(set, purpose_code == "Standard Grid")
set_dat <- transmute(set,
  year = as.integer(year),
  station_key = as.integer(stlkey),
  station = as.integer(station),
  longitude = midlon_fished,
  latitude = midlat_fished,
  depth_m = 1.8288 * avgdepth_fm,
  usable = eff,
  soak_time_min,
  temp_c
)
set_dat |>
  ggplot(aes(longitude, latitude)) +
  geom_point() +
  facet_wrap(~year)

dat <- inner_join(count_dat, set_dat, by = join_by(year, station, station_key))

# need to fill in the zeros -------------------------------------------------

full <- select(
  dat, year, station, station_key, longitude, latitude,
  hooks_observed, sample_type, usable, soak_time_min, temp_c, depth_m
) |> distinct()
full <- purrr::map_dfr(
  sort(unique(count_dat$species_science_name)),
  \(x) mutate(full, species_science_name = x)
)

missing <- anti_join(full, dat)
missing$number_observed <- 0L
dat <- bind_rows(dat, missing)
dat <- arrange(dat, year, species_common_name, station)

# replace common name with DFO common name:
dat$species_common_name <- NULL
dat <- left_join(dat, select(spp, species_science_name, species_common_name),
  by = join_by(species_science_name)
)
dat <- filter(dat, usable == "Y")
dat$usable <- NULL

# plotting helpers ----------------------------------------------------------

coast <- rnaturalearth::ne_countries(scale = 10, continent = "north america", returnclass = "sf") |>
  sf::st_crop(xmin = -135, xmax = -121, ymin = 46, ymax = 55.6) |>
  sf::st_transform(crs = 32609)

plot_map <- function(data, colour_column = NULL, species = "north pacific spiny dogfish") {
  data |>
    filter(species_common_name == species) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_transform(crs = 32609) |>
    ggplot() +
    geom_sf(data = coast) +
    geom_sf(pch = 19, size = 0.8, mapping = aes(colour = {{ colour_column }})) +
    facet_wrap(~year) +
    coord_sf(xlim = c(192152, 932873), ylim = c(5357711, 6132290))
}

# checks --------------------------------------------------------------------

# all should be have same number of rows, since zeros have been added:
stopifnot(length(unique(table(dat$species_common_name))) == 1L)

plot_map(dat, number_observed/hooks_observed) + scale_colour_viridis_c(trans = "sqrt")
plot_map(dat, depth_m) + scale_colour_viridis_c(trans = "sqrt")

# bring in the 'standard' stations as defined in gfiphc ---------------------

# find 'Standard' grid within gfiphic:
# gfiphc_dat <- readRDS("report/data-cache-2024-05/iphc/north-pacific-spiny-dogfish.rds")$set_counts
# saveRDS(gfiphc_dat, file = "data-raw/gfiphc-dogfish-setcounts.rds")
gfiphc_dat <- readRDS("data-raw/gfiphc-dogfish-setcounts.rds")

pbs_stations <- select(gfiphc_dat, pbs_standard_grid = standard, pbs_usable = usable, station, year) |>
  distinct()
pbs_stations$pbs_standard_grid <- as.character(pbs_stations$pbs_standard_grid)

iphc_stations <- select(dat, year, station, station_key) |>
  mutate(station = as.character(station)) |> distinct()

# find duplicate stations in gfiphc that are not in the IPHC download:
dup_in_gfiphc <- group_by(pbs_stations, year, station) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n > 1)

dup_in_iphc <- group_by(iphc_stations, year, station) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n > 1)

dup_in_gfiphc
dup_in_iphc

dups_in_gfiphc_but_not_iphc <- anti_join(select(dup_in_gfiphc, -n), select(dup_in_iphc, -n),
  by = join_by(year, station))

# drop these extra duplicates in 2022 that shouldn't be there!?
pbs_stations <- anti_join(pbs_stations, dups_in_gfiphc_but_not_iphc, by = join_by(station, year))

u_stations <- pbs_stations |> select(station, pbs_standard_grid) |> distinct()
dups <- u_stations |> group_by(station) |> summarise(n = n(), .groups = "drop") |>
  filter(n > 1)

# so, these were chosen as 'standard' in one year but not another:
filter(pbs_stations, station %in% dups$station) |>
  arrange(station, year)

# let's turn them to non-standard since they only appear in 2021:
pbs_stations$pbs_standard_grid[pbs_stations$station %in% dups$station]
pbs_stations$pbs_standard_grid[pbs_stations$station %in% dups$station] <- "N"

# and remove those now duplicated stations:
pbs_stations <- select(pbs_stations, station, pbs_standard_grid) |> distinct()

joined_stations <- left_join(
  iphc_stations,
  pbs_stations,
  by = join_by(station)
)

joined_stations <- joined_stations |>
  mutate(
    pbs_standard_grid = pbs_standard_grid == "Y",
    station = as.integer(station)
  )

# any stations get lost? check:
missing <- anti_join(dat, joined_stations, by = join_by(year, station, station_key))
stopifnot(nrow(missing) == 0L)

dat_pbs <- left_join(dat, joined_stations, by = join_by(year, station, station_key))

# the gfiphc data was up to 2022, from 2023 onwards, need to make some manual decisions:
filter(dat_pbs, year > 2022) |> plot_map(pbs_standard_grid)

# is anything `NA`; if so, fix it! choose TRUE or FALSE for 'standard grid'
# likely FALSE
# look at it:
filter(dat_pbs, species_common_name == "north pacific spiny dogfish") |>
  filter(is.na(pbs_standard_grid)) |>
  select(station, year)

# action happens here:
dat_pbs$pbs_standard_grid[is.na(dat_pbs$pbs_standard_grid)] <- FALSE

# visualize what we have:
plot_map(dat_pbs, pbs_standard_grid)

# manually enter those in inside WCVI waters --------------------------------

filter(dat_pbs, year == 2018) |>
  ggplot() +
  geom_text(aes(x = longitude, y = latitude, label = station), size = 2)
inside_stations <- c(
  2207, 2204, 2203, 2201, 2212, 2211, 2215, 2216, 2219,  2220, 2222, 2223,
  2224, 2225, 2227, 2228, 2229, 2230, 2231, 2234, 2235, 2236, 2238, 2239, 2243,
  2244, 2249, 2259, 2245, 2246
)

dat_pbs <- mutate(dat_pbs, inside_wcvi = station %in% inside_stations)

filter(dat_pbs, year == 2018, !inside_wcvi) |> plot_map(pbs_standard_grid)
filter(dat_pbs, year == 2018, inside_wcvi) |> plot_map(pbs_standard_grid)

dat_pbs |> plot_map(paste(pbs_standard_grid, inside_wcvi))

# bring in pre 1998 data from gfiphc ----------------------------------------

old <- gfiphc::data1996to2002 |> filter(year < 1998) |> filter(usable == "Y")

# check to make sure stations don't clash:
stopifnot(sum(old$station %in% dat_pbs$station) == 0L)

glimpse(dat_pbs)

old <- select(old, year, station, longitude = lon, latitude = lat, depth_m = depthAvge, species_common_name = spNameIPHC, number_observed = catchCount, hooks_observed = hooksObserved)
old <- mutate(old, species_common_name = tolower(species_common_name))

old_sp <- sort(unique(old$species_common_name))
old_sp[!old_sp %in% old$species_common_name]

# most can be dropped, but fix spiny dogfish and sixgill shark
old$species_common_name[old$species_common_name == "spiny dogfish"] <- "north pacific spiny dogfish"
old$species_common_name[old$species_common_name == "sixgill shark"] <- "bluntnose sixgill shark"

# join on the scientific names from PBS:
old <- inner_join(old, select(spp, species_common_name, species_science_name),
  by = join_by(species_common_name))

# make a fake `station_key` to match modern data:
old <- mutate(old,
  station_key = as.integer(paste0(year, station)),
  inside_wcvi = FALSE,
  pbs_standard_grid = TRUE # confirmed to match gfiphc decisions
)

dat_all <- bind_rows(old, dat_pbs) |>
  arrange(year, species_common_name, station)

# visualize what we have:
plot_map(dat_all, paste(pbs_standard_grid, inside_wcvi)) + scale_colour_brewer(palette = "Dark2")

plot_map(dat_all, number_observed/hooks_observed) + scale_colour_viridis_c(trans = "sqrt")

# save it! ------------------------------------------------------------------

attr(dat_all, "iphc_download_date") <- DOWNLOAD_DATE
attr(dat_all, "data_preparation_date") <- lubridate::today()
saveRDS(dat_all, file = "report/iphc-simple/iphc-pbs.rds")
