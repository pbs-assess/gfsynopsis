library(tidyverse)
library(zoo)
library(patchwork)
devtools::load_all()
theme_set(theme_pbs())
library(sf)

cores <- floor(unname(future::availableCores()/2))
is_rstudio <- function() Sys.getenv("RSTUDIO") == "1"
.future <- if (is_rstudio()) future::multisession else future::multicore
future::plan(.future, workers = cores)
options(future.rng.onMisuse = "ignore")

# Load data
data_cache <- here::here('report', 'data-cache-nov-2023')
grid_dir <- here::here(data_cache, 'grids')
mssm_dir <- here::here('report', 'mssm-tech-report')
mssm_data <- here::here(mssm_dir, 'data')
mssm_data_out <- here::here(mssm_dir, 'data-outputs')
cpue_cache <- here::here('report', 'cpue-cache')

stitch_cache <- here::here(mssm_dir, 'stitch-cache')
syn_sc <- here::here(mssm_dir, 'stitch-cache', 'SYN-WCVI')
mssm_sc <- here::here(mssm_dir, 'stitch-cache', 'mssm')

mssm_figs <- here::here(mssm_dir, 'figure')

survey_cols <- c("MSSM WCVI" = "#1b9e77",
  "MSSM Model" = "#1b9e77",
  "MSSM Design" = "#e7298a",
  "SYN WCVI" = "#7570b3",
  "SYN WCVI on MSSM Grid" = "#7570b3",
  "CPUE 3CD" = "#d95f02")

grid_colours <- c(
  "GFBioField" = "#ff7f00",
  "1975 (Loran A)" = "#deebf7",
  "1979 (Loran C)" = "#9ecae1",
  "1998 (GPS)" = "#4292c6",
  "2009 (GFBioField)" = "#084594",
  "2009" = "#084594",
  "2009-2021" = "#084594",
  "2022" = "#ff7f00"#"#08306b"
)

spp_vector  <- gfsynopsis::get_spp_names()$species_common_name
spp_name_lu <- gfsynopsis::get_spp_names() |> select(species_common_name, spp_w_hyphens)

spp_levels <- gfsynopsis::get_spp_names() |> arrange(species_code) |>
  pluck('species_common_name')

mssm_survey_changes <- readr::read_csv(file.path(mssm_data, 'mssm-survey-changes.csv'))

mssm_survey_changes |>
  filter(stringr::str_detect(Change, "Navigation|Data")) |>
  select(Year, Change, Details)

order_spp <- function(df) {
  mutate(df, species = factor(stringr::str_to_title(species), levels = stringr::str_to_title(spp_levels)))
}

# Load raw data for each survey so we can calculate overlapping years
if (!file.exists(file.path(mssm_data, 'spp_dat.rds'))) {
  spp_dat <- spp_vector |>
    map(\(sp) readRDS(file.path(data_cache, paste0(gfsynopsis:::clean_name(sp), ".rds")))$survey_sets) |>
    bind_rows() |>
    filter(survey_abbrev %in% c('MSSM WCVI', 'SYN WCVI'))
  beepr::beep()
  # Slow to load all the data
  saveRDS(spp_dat, file = file.path(mssm_data, 'spp_dat.rds'))
} else {
  spp_dat <- readRDS(file = file.path(mssm_data, 'spp_dat.rds'))
}

mssm_no_doorspread <- spp_dat |>
  filter(survey_abbrev == 'MSSM WCVI', year == 2022) |>
  distinct(fishing_event_id, .keep_all = TRUE) |>
  summarise(n = sum(doorspread_m == 0), percent = round(100 * n / n()))

# --- SYN WCVI survey data ---
sw_dat <- spp_dat |>
  select(-survey_series_id.x) |>
  filter(survey_abbrev == 'SYN WCVI') |>
  prep_stitch_dat()

# --- MSSM survey data ---
# Exclude species that have only zero values (not sure why some of these are in the data and some are not....)
# I double checked that all(catch_count == 0) as well...
mssm_dat <- spp_dat |>
  select(-survey_series_id.x) |>
  filter(survey_abbrev == 'MSSM WCVI') |>
  prep_mssm_dat() |>
  group_by(species_common_name) |>
  filter(!all(catch == 0)) |> # some species are in there with all zeros
  ungroup() |>
  filter(!(year %in% 1977:1978 & month == 9)) |> # Filter out the extra sampling
  mutate(gear = case_when( # add gear type change
    year < 1977 ~ 'shrimp balloon',
    year < 2006 & year > 1977 ~ 'NMFS',
    year > 2006 ~ 'American',
    year == 2006 & fishing_event_id %in% c(1158541, 1158542) ~ 'American',
    year == 2006 & fishing_event_id %in% c(1158559, 1158560) ~ 'NMFS',
    year == 2006 & !(fishing_event_id %in% c(1158541, 1158542, 1158559, 1158560)) ~ 'American'
    )
)

# Check the two 'calibration tows' done in 2006
comp_trawls <- c('American-1' = 1158541, 'NMFS-1' = 1158559,
  'American-2' = 1158542, 'NMFS-2' = 1158560) |>
  enframe(name = 'net_tow', value = 'fishing_event_id') |>
  separate(net_tow, into = c("net", "tow"), sep = "-")


# --- Prepare grid ----
# Get data up to 2021 to build the grid
pcod_dat <-
  mssm_dat |>
  dplyr::filter(species_common_name == 'pacific cod') |>
  dplyr::filter(!is.na(longitude)) |>
  dplyr::mutate(row_id = dplyr::row_number())

pcod_sf <-
    pcod_dat |>
    dplyr::select(year, longitude, latitude) |>
    sf::st_as_sf(coords = c('longitude', 'latitude'), crs = 'WGS84')

pcod_years <- dplyr::select(pcod_dat, row_id, year)

mssm_grid_sf <- gfdata::mssm_grid_sf

# --- Make the grids ---
mk_mssm_grid <- function(dat_wgs84, grid_spacing) {
  # Use equal distance projection
  pcod_sf <-
    dat_wgs84 |>
    dplyr::select(year, longitude, latitude) |>
    sf::st_as_sf(coords = c('longitude', 'latitude'), crs = 'WGS84') |>
    sf::st_transform(crs = 32609)

  # Create grid over the bounding box of the polygon
  full_grid <- pcod_sf |>
    sf::st_make_grid(cellsize = c(grid_spacing, grid_spacing)) |>
    sf::st_as_sf() |>
    dplyr::rename(geometry = x)

  # Get grid cells that overlap with at least one sampling point
  intersected <- sf::st_intersects(full_grid, pcod_sf)

  id_intersect <- intersected |> purrr::map_dbl(length) > 0
  # last_year <- intersected |>
  #   purrr::map_dbl(\(row) if (length(row) > 0) {max(pcod_years[row, ]$year)} else {NA})

  sampling_years <- intersected |>
      purrr::map(\(row) if (length(row) > 0) {pcod_years[row, ]$year} else {NA})

  #full_grid$last_samp_year <- last_year

  mssm_grid_sf <- full_grid |>
    mutate(year = sampling_years) |>
    unnest(cols = year) |>
    filter(!is.na(year)) |>
    dplyr::mutate(survey = "MSSM WCVI")
  #full_grid$year <- sampling_years

  # mssm_grid_sf <- full_grid[id_intersect, ] |>
  #   dplyr::mutate(survey = "MSSM WCVI")

  mssm_grid <- mssm_grid_sf |>
    sf::st_centroid() %>%
    dplyr::mutate(survey = "MSSM WCVI",
                  #ssid = 7,
                  X = sf::st_coordinates(.)[,1] / 1000, # match sdmTMB coordinate system
                  Y = sf::st_coordinates(.)[,2] / 1000, # match sdmTMB coordinate system
                  area = grid_spacing / 1000 * grid_spacing / 1000) |>
    sf::st_drop_geometry() |>
    dplyr::as_tibble() |>
    dplyr::select(survey, X, Y, area, year)
    #dplyr::select(survey, X, Y, area, last_samp_year)

  mssm_grid_sf <- mssm_grid_sf |>
    sf::st_transform(crs = "WGS84")

  list(mssm_grid = mssm_grid, mssm_grid_sf = mssm_grid_sf)
  }

mssm_loaded <- TRUE