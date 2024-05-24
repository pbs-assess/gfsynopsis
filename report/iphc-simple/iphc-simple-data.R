library(ggplot2)
library(dplyr)

# 2023-05-23 download:
# standard grid 2B
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
  # hooks_fished = as.integer(hooksfished),
  # hooks_retrieved = as.integer(hooksretrieved),
  hooks_observed = as.integer(hooksobserved),
  number_observed = as.integer(number_observed),
  sample_type = if_else(sampletype == "20Hook", "20 hooks", "all hooks"),
  # set_number = as.integer(setno),
  station = as.integer(station),
  station_key = as.integer(stlkey)
)

# need to collapse bering skate and sandpaper skate into one species!
# bathyraja interrupta
count_dat <- count_dat |>
  group_by(year, species_science_name, sample_type, station, station_key) |>
  summarise(
    hooks_observed = sum(hooks_observed),
    number_observed = sum(number_observed),
    species_common_name = species_common_name[1], # pick one for now; the DFO ones get joined anyways
    .groups = "drop"
  )

# standard grid 2B
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
set <- filter(set, purpose_code == "Standard Grid")
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


dat <- dplyr::inner_join(count_dat, set_dat, by = join_by(year, station, station_key))

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

# diagnostics ---------------------------------------------------------------

theme_set(theme_light())

# all should be have same number of rows, since zeros have been added:
unique(table(dat$species_common_name))
stopifnot(length(unique(table(dat$species_common_name))) == 1L)

dat_sf <- sf::st_as_sf(dat, coords = c("longitude", "latitude"), crs = 4326) |>
  sf::st_transform(crs = 32609)

coast <- rnaturalearth::ne_countries(scale = 10, continent = "north america", returnclass = "sf") |>
  sf::st_crop(xmin = -135, xmax = -121, ymin = 46, ymax = 55.6) |>
  sf::st_transform(crs = 32609)

dat_sf |>
  # filter(year == 2018) |>
  filter(species_common_name == "north pacific spiny dogfish") |>
  ggplot() +
  geom_sf(data = coast) +
  geom_sf(pch = 19, size = 1, mapping = aes(colour = number_observed / hooks_observed)) +
  facet_wrap(~year) +
  scale_colour_viridis_c(trans = "sqrt") +
  coord_sf(xlim = c(192152, 932873), ylim = c(5357711, 6132290)) +
  labs(colour = "count/hook")
