library(tidyverse)
library(zoo)
library(patchwork)
theme_set(gfplot::theme_pbs(base_size = 12))
library(sf)

cores <- floor(unname(future::availableCores()/2))
is_rstudio <- function() Sys.getenv("RSTUDIO") == "1"
.future <- if (is_rstudio()) future::multisession else future::multicore
future::plan(.future, workers = cores)
options(future.rng.onMisuse = "ignore")

source(here::here('report', 'mssm-tech-report', 'R', '00-utils.R'))

# Load data
data_cache <- here::here('report', 'data-cache-nov-2023')
grid_dir <- here::here(data_cache, 'grids')
mssm_dir <- here::here('report', 'mssm-tech-report')
mssm_data <- here::here(mssm_dir, 'data')
mssm_data_out <- here::here(mssm_dir, 'data-outputs')
cpue_cache <- here::here('report', 'cpue-cache')

stitch_cache <- here::here('report', 'stitch-cache')
# syn_sc <- here::here(mssm_dir, 'stitch-cache', 'SYN-WCVI')
# mssm_sc <- here::here(mssm_dir, 'stitch-cache', 'mssm')

mssm_figs <- here::here(mssm_dir, 'figure')

survey_cols <- c("SMMS WCVI" = "#1b9e77",
  "SMMS Model" = "#1b9e77",
  "SMMS" = "#1b9e77",
  "SMMS Design" = "#e7298a",
  "SYN WCVI" = "#4f4a8c",
  "SYN WCVI on SMMS Grid" = "#4f4a8c",
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

spp_hyphens  <- gfsynopsis::get_spp_names()$spp_w_hyphens

spp_name_lu <- gfsynopsis::get_spp_names() |>
  select(species_common_name, spp_w_hyphens, species_code,
         species_science_name, spp_w_hyphens, type, itis_tsn, worms_id) |>
  mutate(species_common_name = gsub("north pacific spiny dogfish", "pacific spiny dogfish", species_common_name))
spp_levels <- spp_name_lu |> arrange(species_code) |> pluck('species_common_name')

order_spp <- function(df) {
  mutate(df, species = factor(stringr::str_to_title(species), levels = stringr::str_to_title(spp_levels)))
}

# Load raw data for each survey so we can calculate overlapping years
if (!file.exists(file.path(mssm_data, 'spp_dat.rds'))) {
  spp_dat <- spp_hyphens |>
    map(\(sp) readRDS(file.path(data_cache, paste0(sp, ".rds")))$survey_sets) |>
    bind_rows() |>
    filter(survey_abbrev %in% c('MSSM WCVI', 'SYN WCVI')) |>
    mutate(survey_abbrev = gsub("MSSM", "SMMS", survey_abbrev)) |>
    mutate(species_common_name = gsub("north pacific spiny dogfish", "pacific spiny dogfish", species_common_name))
  beepr::beep()
  # Slow to load all the data
  saveRDS(spp_dat, file = file.path(mssm_data, 'spp_dat.rds'))
} else {
  spp_dat <- readRDS(file = file.path(mssm_data, 'spp_dat.rds'))
}

mssm_no_doorspread <- spp_dat |>
  filter(survey_abbrev == 'SMMS WCVI', year == 2022) |>
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
  filter(survey_abbrev == 'SMMS WCVI') |>
  prep_mssm_dat() |>
  group_by(species_common_name) |>
  filter(!all(catch == 0)) |> # some species are in there with all zeros
  ungroup()

mssm_spp <- spp_name_lu |>
  filter(species_common_name %in% unique(mssm_dat$species_common_name)) |>
  mutate(worms_id = as.numeric(ifelse(species_common_name == "rougheye/blackspotted rockfish complex", 274771, worms_id))) # itis_tsn is just rougheye lookup

# ------------------------------------------------------------------------------
# Gather and arrange some metadata
# WoRMS seems to have more up to date taxonomies
if (!file.exists(here::here("report", "mssm-tech-report", "data", "mssm-worms.rds"))) {
  cls <- taxize::classification(mssm_spp$worms_id, db = 'worms')
  saveRDS(cls, file = here::here("report", "mssm-tech-report", "data", "mssm-worms.rds"))
} else {
  cls <- readRDS(here::here("report", "mssm-tech-report", "data", "mssm-worms.rds"))
}

mssm_spp <- taxize:::rbind.classification(cls) |>
  mutate(worms_id = as.numeric(query),
         rank = tolower(rank)) |>
  select(name, rank, worms_id) |>
  filter(!(rank %in% c("kingdom", "subkingdom", "infrakingdom", "phylum", "subphylum"))) |>
  pivot_wider(id_cols = c("worms_id"), names_from = rank, values_from = name) |>
  left_join(mssm_spp)


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

# Get pandalus jordani catches that match with the fish fishing event ids used in the analysis
pj <- readRDS(file.path(mssm_data, 'pink-shrimp.rds'))

mssm_loaded <- TRUE
