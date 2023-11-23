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
data_cache <- here::here('report', 'data-cache-oct-2023')
grid_dir <- here::here(data_cache, 'grids')
mssm_dir <- here::here('report', 'mssm-tech-report')
mssm_data <- here::here(mssm_dir, 'data')
cpue_cache <- here::here('report', 'cpue-cache')

syn_sc <- here::here(mssm_dir, 'stitch-cache', 'SYN-WCVI')
mssm_sc <- here::here(mssm_dir, 'stitch-cache', 'mssm')


mssm_3km_grid_sc <- here::here(mssm_dir, 'stitch-cache', 'mssm', '3km-grid')
mssm_year_sc <- here::here(mssm_dir 'stitch-cache', 'mssm', 'year-bin')

mssm_figs <- here::here(mssm_dir, 'figure')

survey_cols <- c("SYN WCVI" = "#7570b3", "SYN WCVI on MSSM Grid" = "#7570b3",
  "MSSM WCVI" = "#1b9e77", "MSSM Model" = "#1b9e77",
  "MSSM Design" = "#e7298a", "CPUE 3CD" = "#d95f02")

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
  mutate(df, species = factor(species, levels = spp_levels))
}

# Load raw data for each survey so we can calculate overlapping years
spp_dat <- spp_vector |>
  map(\(sp) readRDS(file.path(data_cache, paste0(gfsynopsis:::clean_name(sp), ".rds")))$survey_sets) |>
  bind_rows()
beepr::beep()

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

# mssm_dat |>
#   filter(year == 2006, fishing_event_id %in% comp_trawls$fishing_event_id) |>
#   select(year, species_common_name, fishing_event_id, catch_weight, gear) |>
#   arrange(fishing_event_id, -catch_weight)

# --- Gear change ----
net_comp_df <- mssm_dat |>
  filter(fishing_event_id %in% comp_trawls$fishing_event_id) |>
  left_join(comp_trawls) |>
  group_by(net, species_common_name) |>
  summarise(mean_catch = mean(catch)) |>
  ungroup() |>
  mutate(net = factor(net, levels = c('NMFS', 'American'))) |>
  group_by(species_common_name) |>
  filter(sum(mean_catch) > 0) |>
  ungroup()

tow_plot <-
  ggplot(data = net_comp_df, aes(x = net, y = mean_catch, colour = species_common_name, group = species_common_name)) +
    geom_point() +
    geom_line() +
    scale_y_continuous(trans = 'log10', labels = scales::label_number(accuracy = 0.01), limits = c(0.001, NA)) +
    scale_x_discrete() +
    guides(colour = 'none') +
    coord_cartesian(clip = "off", xlim = c(1, 1.7)) +
    ggrepel::geom_text_repel(
      data = net_comp_df %>% filter(net == 'NMFS'),
      aes(label = species_common_name, x = net, y = mean_catch, colour = species_common_name),
      size = 3.5, hjust = 0, segment.color = 'grey85',
      nudge_x = -0.55, box.padding = 0.1, point.padding = 0.8,
      direction = "y"
    ) +
    labs(y = 'Catch (kg)', x = 'Net')
tow_plot

ggsave(filename = file.path(mssm_figs, 'net-comp.png'), width = 6.5, height = 6)

# --- Prepare and compare grid cell size ----
# Get data up to 2021 to build the grid
pcod_dat <-
  mssm_dat |>
  #dplyr::filter(year <= 2021) |>
  dplyr::filter(species_common_name == 'pacific cod') |>
  dplyr::filter(!is.na(longitude)) |>
  dplyr::mutate(row_id = dplyr::row_number())

pcod_sf <-
    pcod_dat |>
    dplyr::select(year, longitude, latitude) |>
    sf::st_as_sf(coords = c('longitude', 'latitude'), crs = 'WGS84')

pcod_years <- dplyr::select(pcod_dat, row_id, year)

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

# Make 3x3 km grid ---
mssm_grid_3km <- pcod_dat |>
  mk_mssm_grid(grid_spacing = 3000)

mssm_grid_3km[[1]] |>
  dplyr::filter(year >= 2009 & year <= 2021) |>
  dplyr::distinct(X, Y, .keep_all = TRUE)

mssm_grid_3km[[2]] |>
  filter(year >= 2009 & year <= 2021) |>
  distinct(geometry, .keep_all = TRUE) |>
  ggplot() +
  geom_sf(aes(fill = year)) +
  geom_sf(data = mssm_grid_3km[[2]] |> filter(year == 2021), fill = 'pink') +
  geom_sf(data = pcod_sf |> filter(year == 2021), shape = 21, size = 3, fill = 'white') +
  geom_sf(data = mssm_grid_3km[[2]] |> filter(year == 2019), colour = 'purple', fill = NA) +
  geom_sf(data = pcod_sf |> filter(year == 2019), shape = 21, size = 3, fill = 'white')

mssm_grid_3km[[1]] |>
  filter(year >= 2009 & year <= 2021) |>
  dplyr::distinct(X, Y, .keep_all = TRUE)
saveRDS(file.path(grid_dir, 'mssm-grid-3km_2009-2021.rds'))

# Make 2x2 km grid
mssm_grid_2km <- pcod_dat |>
  mk_mssm_grid(grid_spacing = 2000)

km2 <-
  ggplot(data = mssm_grid_2km[[2]] |> filter(year >= 2009 & year <= 2021)) +
  geom_sf(data = pcod_sf, shape = 1, colour = 'grey50', alpha = 0.8, size = 0.1) +
  geom_sf(alpha = 0.5) +
  scale_fill_manual(values = grid_colours) +
  scale_x_continuous(breaks = seq(-127.4, -126.0, by = 0.4))

km3 <-
  ggplot(data = mssm_grid_3km[[2]] |> filter(year >= 2009 & year <= 2021)) +
  geom_sf(data = pcod_sf, shape = 1, colour = 'grey50', alpha = 0.8, size = 0.1) +
  geom_sf(alpha = 0.5) +
  theme(axis.text.y = element_blank()) +
  scale_fill_manual(values = grid_colours) +
  scale_x_continuous(breaks = seq(-127.4, -126.0, by = 0.4))

km2 + km3
ggsave(filename = file.path(mssm_figs, '2km-3km-grid-comp.png'), width = 6.7, height = 4.6)
# ------------------------------------------------------------------------------
# --- Look at spatial distribution of sampling ------------
# Set default grid for plotting
#mssm_grid_sf <- readRDS(file.path(grid_dir, 'mssm-grid-3km_2009-2021.rds'))
mssm_grid_sf <- gfdata::mssm_grid_sf

pcod_dat <- mssm_dat |>
  filter(species_common_name == 'pacific cod')

pcod_sf <- sf::st_as_sf(pcod_dat, coords = c('longitude', 'latitude'), crs = "WGS84")

# Prep grid data for plotting -----
# Grid used in analysis
# Will need to fix the coordinate system this grid uses in gfdata

# Grid from GFBioField
sgrid <- sf::st_read(file.path(mssm_data, "SMMS-grid/SMMS_Survey_Blocks.shp"))

gfbio_grid <- sgrid |>
  # Select only sites off WCVI (since there is no ssid corresponding to 7 or 'MSSM WCVI')
  filter(GROUPING_C %in% c(112, 113)) |>
  sf::st_crop(sgrid, sf::st_bbox(c(xmin = -128, ymin = 48.5, xmax = -126, ymax = 50))) %>%
  sf::st_set_crs('WGS84') %>%
  mutate(area = units::set_units(sf::st_area(.), km^2))

# --- Grid used in synopsis

grid_plot <- mssm_grid_sf |>
  filter(year >= 2009 & year <= 2019) |>
  distinct(geometry) |>
  ggplot() +
    geom_sf(data = pcod_sf, colour = 'grey50', shape = 1, alpha = 0, size = 0.1) +
    geom_sf(aes(fill = '2009'), alpha = 0.5) +
    scale_fill_manual(values = grid_colours) +
    labs(fill = "Grid") +
    theme(legend.position = c(0.8, 0.9)) +
    scale_x_continuous(breaks = seq(-127.4, -126.0, by = 0.4))
grid_plot + geom_sf(data = gfbio_grid, alpha = 0, colour = NA) +
    theme(axis.text = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.title = element_blank())

ggsave(file.path(mssm_figs, 'grid-prediction-2009_no-points.png'), width = 3.5, height = 3.7)

grid_plot_2009_points <- mssm_grid_sf |>
  filter(year >= 2009 & year <= 2019) |>
  distinct(geometry) |>
  ggplot() +
    geom_sf(data = pcod_sf |> filter(year < 2009), shape = 1, colour = 'grey50', alpha = 0.5, size = 0.1) +
    geom_sf(aes(fill = '2009'), alpha = 0.5) +
    geom_sf(data = pcod_sf |> filter(year >= 2009 & year <= 2019), shape = 1, colour = 'black', alpha = 0.5, size = 0.1) +
    scale_fill_manual(values = grid_colours) +
    labs(fill = "Grid") +
    theme(legend.position = c(0.8, 0.9))
grid_plot_2009_points + geom_sf(data = gfbio_grid, alpha = 0, colour = NA) +
    theme(axis.text = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.title = element_blank()) +
    scale_x_continuous(breaks = seq(-127.4, -126.0, by = 0.4)) +
    guides(fill = "none")

ggsave(file.path(mssm_figs, 'grid-prediction-2009.png'), width = 3.5, height = 3.7)
ggsave(file.path(here::here('report', 'tech-report', 'figure'), 'grid-prediction-2009.png'),
  width = 3.5, height = 3.7)

# --- Overlay blocks shown/used in GFBioField
gfbio_field_grid_plot1 <- mssm_grid_sf |>
  filter(year >= 2009 & year <= 2019) |>
  distinct(geometry) |>
  ggplot() +
  geom_sf(data = pcod_sf, shape = 1, colour = 'grey50', alpha = 0.5, size = 0.1) +
  geom_sf(aes(fill = '2009'), alpha = 0.7) +
  geom_sf(data = gfbio_grid, aes(fill = 'GFBioField'), alpha = 0.7) +
  scale_fill_manual(values = grid_colours) +
    labs(fill = "Grid") +
    theme(legend.position = c(0.8, 0.9)) +
    scale_x_continuous(breaks = seq(-127.4, -126.0, by = 0.4))
gfbio_field_grid_plot1 +
  theme(axis.text = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.title = element_blank())

ggsave(file.path(mssm_figs, 'grid-prediction-gfbiofield-1.png'), width = 3.5, height = 3.7)

# --- Historical survey domain
# The grid was created as any 3x3 km grid cell, that overlapped with at least one
# sampling location.
# The overlay grid covered the bounding box of all sampling locations
df_2022 <- mssm_grid_sf |> filter(year == 2022)
df_2009_2021 <- mssm_grid_sf |> filter(year >= 2009 & year < 2022)

grid_historical_plot <-
  ggplot(data = mssm_grid_sf |> distinct(geometry)) +
    geom_sf(aes(fill = "1975 (Loran A)"), alpha = 1) +
    geom_sf(data = mssm_grid_sf |> filter(year >= 1979) |> distinct(geometry),
      aes(fill = "1979 (Loran C)"), alpha = 1) +
    geom_sf(data = mssm_grid_sf |> filter(year >= 1998) |> distinct(geometry),
      aes(fill = "1998 (GPS)"), alpha = 1) +
    geom_sf(data = mssm_grid_sf |> filter(year >= 2009 & year < 2022) |> distinct(geometry),
      aes(fill = "2009-2021"), alpha = 1) +
    geom_sf(data = df_2022[(!df_2022$geometry %in% df_2009_2021$geometry), ],
      aes(colour = "2022"), linewidth = 1, fill = NA) +
      #aes(fill = "2022"), alpha = 1) +
    geom_point(data = pcod_dat, aes(x = longitude, y = latitude), shape = 1, size = 0.5, alpha = 0.2) +
    scale_fill_manual(values = grid_colours) +
    labs(fill = "Last year sampled") +
    theme(legend.position = c(0.77, 0.85)) +
    theme(axis.title = element_blank()) +
    scale_x_continuous(breaks = seq(-127.4, -126.0, by = 0.4))
grid_historical_plot + geom_sf(data = gfbio_grid, alpha = 0, colour = NA) +
  theme(axis.text = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6))

ggsave(file.path(mssm_figs, 'grid-historical-nav-changes.png'), width = 3.5, height = 3.7)
#ggsave(filename = file.path(mssm_figs, 'grid-historical-nav-changes_no-points.png'), width = 3.5, height = 3.7)

spatial_shift_plot <-
  ggplot() +
    geom_sf(data = mssm_grid_sf |> dplyr::filter(year >= 2009 & year <= 2021),
      aes(fill = "2009"), alpha = 0.8, colour = 'grey50') +
    geom_point(data = pcod_dat |>
      filter(year %in% c(1975, 1976, 1977, 1978, 1979, 1985, 1995, 1998, 2003, 2013, 2021, 2022)),
      aes(x = longitude, y = latitude), alpha = 1, size = 1, stroke = 0.5, shape = 21, fill = 'white') +
    scale_fill_manual(values = grid_colours) +
    facet_wrap(~ year, nrow = 2) +
    guides(fill = "none") +
    theme(legend.position = c(0.95, 0.95),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 6))
spatial_shift_plot

ggsave(file.path(mssm_figs, 'grid-spatial-sampling-changes.png'), plot = spatial_shift_plot,
  width = 7.8, height = 8.5)

ggsave(file.path(mssm_figs, 'grid-spatial-sampling-changes_wide.png'), plot = spatial_shift_plot,
  width = 11, height = 7.2)


# ------------------------------------------------------------------------------
# --- Look at effect of 2003 sampling protocol on mean annual catch ------------
yearbin_catch <- mssm_dat |>
  filter(year < 2003) |>
  group_by(species_common_name) |>
  summarise(mean_catch = mean(catch, na.rm = TRUE)) |>
  mutate(yearbin_catch = ifelse(mean_catch == 0, 0, 1)) |>
  distinct(species_common_name, yearbin_catch)

post_2003_spp <- filter(yearbin_catch, yearbin_catch == 0)$species_common_name
saveRDS(post_2003_spp, file.path(mssm_dir, 'post-2003-spp.rds'))

sampling_2003 <-
  mssm_dat |>
    group_by(species_common_name, species_code, year) |>
    summarise(mean_catch = mean(catch, na.rm = TRUE), .groups = 'drop') |>
    left_join(yearbin_catch) |>
    mutate(mean_catch = ifelse((species_common_name %in% post_2003_spp & year < 2003), NA, mean_catch))

pre_2003_spp_plot <- sampling_2003 |>
  filter(!(species_common_name %in% post_2003_spp)) |>
  mutate(species_common_name = stringr::str_to_title(species_common_name)) |>
  mutate(species_common_name = forcats::fct_reorder(species_common_name, species_code)) |>
  ggplot(data = _, aes(x = year, y = mean_catch)) +
    geom_rect(aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf),
              fill = "gray85", alpha = 0.2) +
    geom_point() +
    geom_line(alpha = 0.5) +
    geom_vline(xintercept = 2001, colour = 'grey50') +
    theme(axis.text = element_blank()) +
    facet_wrap(~ species_common_name, scales = 'free_y', ncol = 4) +
    labs(x =  "Year", y = "Mean annual catch (kg)")
pre_2003_spp_plot

ggsave(file.path(mssm_figs, 'sampling-2003.png'), plot = pre_2003_spp_plot,
  width = 17, height = 9)

# Comparison of pcod, pollock, tomcod and possible misidentification
cod_comparison <-
  mssm_dat |>
    filter(species_common_name %in% c('pacific cod', 'walleye pollock', 'pacific tomcod')) |>
    group_by(species_common_name, species_code, year) |>
    summarise(mean_catch = mean(catch, na.rm = TRUE), .groups = 'drop') |>
    left_join(yearbin_catch) |>
    mutate(species_common_name = stringr::str_to_title(species_common_name)) |>
    mutate(species_common_name = forcats::fct_reorder(species_common_name, species_code)) |>
  ggplot(data = _, aes(x = year, y = mean_catch)) +
    geom_rect(aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = "grey90", colour = NA) +
    geom_point() +
    geom_line() +
    scale_colour_brewer(palette = "Dark2", type = 'qual') +
    guides(colour = "none") +
    #facet_wrap(~ species_common_name, scales = 'free_y', nrow = 3) +
    facet_wrap(~ species_common_name, nrow = 3) +
    labs(x =  "Year", y = "Mean annual catch (kg)")
cod_comparison

ggsave(file.path(mssm_figs, 'sampling-cod.png'), plot = cod_comparison,
  width = 5.5, height = 5.5)

# Calculate indices ------------------------------------------------------------
# catch ~ 1 model is in mssm_sc and is what is in the report

## Get model where pre and post 2003 is added as factor
# Use 3km grid
# All species ID'd to lowest taxonomic level in 2003 onward
future::plan(future::multicore, workers = 8)
furrr::future_walk(spp_vector, function(.sp) {
#purrr::walk(spp_vector, function(.sp) {
  spp_filename <- paste0(gfsynopsis:::clean_name(.sp), "_st-rw.rds")
    survey_dat <- readRDS(file.path(data_cache, paste0(gfsynopsis:::clean_name(.sp), ".rds")))$survey_sets |>
      filter(survey_abbrev == "MSSM WCVI")
    # Some species not included in survey_set data frame at all, so we need to skip these
    if (nrow(survey_dat) == 0) {
      out <- "No MSSM survey data"
      message(out)
      #saveRDS(out, file.path(file.path(mssm_year_sc, 'year-bin'), spp_filename))
      saveRDS(out, file.path(file.path(mssm_sc, '3km-grid'), spp_filename))
    } else {
      survey_dat <- prep_mssm_dat(survey_dat)

      get_stitched_index(
        #form = 'catch ~ 1 + year_bin',
        form = 'catch ~ 1',
        survey_dat = survey_dat, species = .sp,
        family = sdmTMB::tweedie(),
        #survey_type = "mssm", model_type = 'st-rw', cache = file.path(mssm_year_sc, 'year-bin'),
        survey_type = "mssm", model_type = 'st-rw', cache = file.path(mssm_sc, '3km-grid'),
        cutoff = 5, silent = FALSE,
        grid_dir = NULL, check_cache = TRUE,
        survey_grid = NULL
      )
    }
})

future::plan(future::sequential)
beepr::beep()

# Use 2km grid
furrr::future_walk(spp_vector, function(.sp) {
#purrr::walk(spp_vector, function(.sp) {
  spp_filename <- paste0(gfsynopsis:::clean_name(.sp), "_st-rw.rds")
    survey_dat <- readRDS(file.path(data_cache, paste0(gfsynopsis:::clean_name(.sp), ".rds")))$survey_sets |>
      filter(survey_abbrev == "MSSM WCVI")
    # Some species not included in survey_set data frame at all, so we need to skip these
    if (nrow(survey_dat) == 0) {
      out <- "No MSSM survey data"
      message(out)
      #saveRDS(out, file.path(file.path(mssm_year_sc, 'year-bin'), spp_filename))
      saveRDS(out, file.path(file.path(mssm_sc), spp_filename))
    } else {
      survey_dat <- prep_mssm_dat(survey_dat)

      get_stitched_index(
        form = 'catch ~ 1',
        survey_dat = survey_dat, species = .sp,
        family = sdmTMB::tweedie(),
        survey_type = "mssm", model_type = 'st-rw', cache = file.path(mssm_sc, '2km-grid'),
        cutoff = 5, silent = FALSE,
        grid_dir = NULL, check_cache = TRUE,
        survey_grid = mssm_grid_2km |>
          filter(year >= 2009 & year < 2022) |>
          distinct(X, Y, survey, area)
      )
    }
})
future::plan(future::sequential)
beepr::beep()



# # Fit SYN WCVI ------------------
furrr::future_walk(spp_vector, function(.sp) {
# purrr::walk(spp_vector, function(.sp) {
  spp_filename <- paste0(gfsynopsis:::clean_name(.sp), "_st-rw.rds")
    survey_dat <- readRDS(file.path(data_cache, paste0(gfsynopsis:::clean_name(.sp), ".rds")))$survey_sets |>
      filter(survey_abbrev == "SYN WCVI")
      survey_dat <- prep_stitch_dat(survey_dat)

      get_stitched_index(
        form = 'catch ~ 1',
        survey_dat = survey_dat, species = .sp,
        family = sdmTMB::tweedie(),
        survey_type = "SYN WCVI", model_type = 'st-rw', cache = file.path('report','stitch-cache', 'SYN-WCVI', 'mssm-grid-3km'),
        cutoff = 20, silent = FALSE,
        survey_grid = gfdata::mssm_grid |> mutate(survey = 'SYN WCVI'),
        grid_dir = NULL,
        check_cache = TRUE
        #grid_dir = grid_dir, check_cache = TRUE
      )
})
beepr::beep()

# Load index dataframes ----
# MSSM geostat without year bin

future::plan(future::sequential) # avoid RStudio crash if restarting R

mssm_2km_inds <- spp_vector |>
  map(\(sp) readRDS(file.path(mssm_sc, paste0(gfsynopsis:::clean_name(sp), '_st-rw.rds')))) |>
  setNames(spp_vector) |>
  keep(\(x) inherits(x, 'data.frame')) |>
  bind_rows(.id = 'species') |>
  as_tibble() |>
  mutate(year_bins = "~ 1") |>
  mutate(grid = '2km') |>
  group_by(species) |>
  mutate(extreme_uci = max(upperci) > 10 * max(biomass)) |>
  ungroup()

# MSSM geostat 2003 year bin
mssm_year_inds <- spp_vector |>
  map(\(sp) readRDS(file.path(mssm_year_sc, paste0(gfsynopsis:::clean_name(sp), '_st-rw.rds')))) |>
  setNames(spp_vector) |>
  keep(\(x) inherits(x, 'data.frame')) |>
  bind_rows(.id = 'species') |>
  as_tibble() |>
  mutate(year_bins = "~ 1 + f(year_bin)") |>
  group_by(species) |>
  mutate(extreme_uci = max(upperci) > 10 * max(biomass)) |>
  ungroup()

# MSSM geostat 2003 year bin
mssm_3km_inds <- spp_vector |>
  map(\(sp) readRDS(file.path(mssm_3km_grid_sc, paste0(gfsynopsis:::clean_name(sp), '_st-rw.rds')))) |>
  setNames(spp_vector) |>
  keep(\(x) inherits(x, 'data.frame')) |>
  bind_rows(.id = 'species') |>
  as_tibble() |>
  mutate(grid = "3km") |>
  group_by(species) |>
  mutate(extreme_uci = max(upperci) > 10 * max(biomass)) |>
  ungroup()

# MSSM design index
mssm_d_inds <- spp_vector |>
  map(\(sp) readRDS(file.path(data_cache, paste0(gfsynopsis:::clean_name(sp), '.rds')))$survey_index) |>
  setNames(spp_vector) |>
  bind_rows(.id = 'species') |>
  as_tibble() |>
  filter(survey_abbrev == 'MSSM WCVI') |>
  mutate(survey_abbrev = "MSSM Design")

# SYN WCVI index
syn_inds <-
  spp_vector |>
    map(\(sp) readRDS(file.path(syn_sc, paste0(gfsynopsis:::clean_name(sp), '_st-rw.rds')))) |>
    setNames(spp_vector) |>
    keep(\(x) inherits(x, 'data.frame')) |>
    bind_rows(.id = 'species') |>
    as_tibble()

syn_mssm_grid_inds <-
  spp_vector |>
    map(\(sp) readRDS(file.path(syn_sc, 'mssm-grid-3km', paste0(gfsynopsis:::clean_name(sp), '_st-rw.rds')))) |>
    setNames(spp_vector) |>
    keep(\(x) inherits(x, 'data.frame')) |>
    bind_rows(.id = 'species') |>
    as_tibble() |>
    mutate(survey_abbrev = 'SYN WCVI on MSSM Grid')

# --- CPUE index ---
cpue_ind <- spp_vector |>
  map(\(sp) readRDS(file.path(cpue_cache, paste0(gfsynopsis:::clean_name(sp), '.rds')))) |>
  setNames(spp_vector) |>
  keep(\(x) inherits(x, 'data.frame')) |>
  bind_rows(.id = 'species') |>
  filter(area == '3CD') |>
  rename(biomass = est, lowerci = lwr, upperci = upr) |>
  mutate(survey_abbrev = 'CPUE 3CD')

# ------------------------------------------------------------------------------

# One fewer models converges when we include year bin (blackbelly eelpout -
# a species that only shows up after 2003)

setdiff(mssm_3km_inds$species, mssm_year_inds$species)
setdiff(mssm_year_inds$species, mssm_3km_inds$species)


# Compare models that inclued and exclude the year 2003 break point
year_bin_ind_plot <- bind_rows(mssm_2km_inds, mssm_year_inds) |>
  order_spp() |>
  ggplot(data = _, aes(x = year, y = biomass)) +
    geom_line(aes(colour = year_bins)) +
    geom_point(aes(colour = year_bins)) +
    geom_ribbon(aes(ymin = lowerci, ymax = upperci, fill = year_bins), alpha = 0.3) +
    geom_rect(data = . %>% filter(extreme_uci == TRUE | mean_cv > 2) %>%
      distinct(species, year_bins, .keep_all = TRUE),
      mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
      fill = "gray85", alpha = 0.3) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2" ) +
    facet_wrap(~ species, scales = "free_y", nrow = 6) +
    ggtitle("Comparing catch ~ 1 and catch ~ 1 + pre/post 2003 year bin") +
    theme(legend.position = c(0.6, 1.03),
          axis.text.y = element_blank()) +
    guides(color = guide_legend(direction = "horizontal"), fill = guide_legend(direction = "horizontal")) +
    labs(colour = "Model", fill = "Model")
year_bin_ind_plot

ggsave(file.path(mssm_figs, 'sampling-year-effect.png'), plot = year_bin_ind_plot,
  width = 10.5, height = 8)

grid_bin_ind_plot <- bind_rows(mssm_2km_inds, mssm_3km_inds) |>
  order_spp() |>
  ggplot(data = _, aes(x = year, y = biomass)) +
    geom_line(aes(colour = grid)) +
    geom_point(aes(colour = grid)) +
    geom_ribbon(aes(ymin = lowerci, ymax = upperci, fill = grid), alpha = 0.3) +
    geom_rect(data = . %>% filter(extreme_uci == TRUE | mean_cv > 2) %>%
      distinct(species, grid, .keep_all = TRUE),
      mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
      fill = "gray85", alpha = 0.3) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2" ) +
    facet_wrap(~ species, scales = "free_y", nrow = 6) +
    #ggtitle("Comparing modeled index from 2x2 km and 3x3 km grids") +
    theme(legend.position = c(0.5, 1.03),
          axis.text.y = element_blank()) +
    guides(color = guide_legend(direction = "horizontal"), fill = guide_legend(direction = "horizontal")) +
    labs(colour = "Model", fill = "Model") +
    labs(x = 'Year', y = 'Relative biomass index')
grid_bin_ind_plot

ggsave(file.path(mssm_figs, '2km-3km-grid-model-comp.png'), plot = grid_bin_ind_plot,
  width = 10.5, height = 8)


# cod_year_bin <- bind_rows(mssm_3km_inds, mssm_year_inds) |>
#   filter(species == "pacific cod") |>
#   ggplot(data = _, aes(x = year, y = biomass)) +
#     geom_line(aes(colour = year_bins)) +
#     geom_point(aes(colour = year_bins)) +
#     geom_ribbon(aes(ymin = lowerci, ymax = upperci, fill = year_bins), alpha = 0.3) +
#     geom_rect(data = . %>% filter(extreme_uci == TRUE | mean_cv > 2) %>%
#       distinct(species, year_bins, .keep_all = TRUE),
#       mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
#       fill = "gray85", alpha = 0.3) +
#     scale_color_brewer(palette = "Dark2") +
#     scale_fill_brewer(palette = "Dark2" ) +
#     ggtitle("Pacific Cod") +
#     theme(legend.position = c(0.7, 0.9),
#           axis.text.y = element_blank()) +
#     guides(color = guide_legend(direction = "horizontal"), fill = guide_legend(direction = "horizontal")) +
#     labs(colour = "Model", fill = "Model")

# pcod_year_effect <- cod_comparison / cod_year_bin + plot_layout(heights = c(2, 1))

# mssm_fig_list$pcod_year_effect <- pcod_year_effect

# ggsave(file.path(mssm_figs, 'sampling-pcod-year-effect.png'), plot = pcod_year_effect,
#   width = 5.5, height = 7)

# Compare MSSM and SYN WCVI and 3CD CPUE --------------------------------------
# Get overlapping years to scale based on geometric means of indexes
syn_years <- unique(sw_dat$year)
mssm_years <- unique(mssm_dat$year)
cpue_years <- unique(cpue_ind$year)


#spp_in_mssm <- unique(mssm_3km_inds$species)
spp_in_mssm <- mssm_3km_inds |>
  filter(!extreme_uci | is.na(extreme_uci)) |>
  filter(mean_cv < 4 | is.na(mean_cv)) |>
  distinct(species) |>
  pluck('species')

spp_in_mssm_design <- mssm_d_inds |>
  count(species) |>
  filter(n > 2) |>
  distinct(species) |>
  pluck('species')

spp_in_mssm_design_only <- setdiff(spp_in_mssm_design, spp_in_mssm)

syn_mssm_overlap <- intersect(syn_years, mssm_years)
cpue_mssm_overlap <- intersect(cpue_years, mssm_years)

# MSSM with SYN WCVI - Use 3km grid
inds <- bind_rows(mssm_3km_inds, syn_inds, syn_mssm_grid_inds, cpue_ind, mssm_d_inds) |>
  mutate(syn_overlap = ifelse(year %in% syn_mssm_overlap, TRUE, FALSE),
         cpue_overlap = ifelse(year %in% cpue_mssm_overlap, TRUE, FALSE))

mssm_geomeans <- inds |>
  filter(survey_abbrev %in% c("MSSM WCVI", "MSSM Design")) |>
  group_by(species, survey_abbrev) |>
  summarise(mssm_geomean = exp(mean(log(biomass))), .groups = 'drop')

syn_overlap_geomeans <- inds |>
  filter(syn_overlap, survey_abbrev %in% c("MSSM WCVI", "SYN WCVI", "SYN WCVI on MSSM Grid")) |>
  group_by(species, survey_abbrev) |>
  summarise(syn_overlap_geomean = exp(mean(log(biomass))), .groups = 'drop')

cpue_overlap_geomeans <- inds |>
  filter(cpue_overlap, survey_abbrev %in% c("MSSM WCVI", "CPUE 3CD")) |>
  group_by(species, survey_abbrev) |>
  summarise(cpue_overlap_geomean = exp(mean(log(biomass))), .groups = 'drop')

scaled_inds <- left_join(inds, syn_overlap_geomeans) |>
  left_join(cpue_overlap_geomeans) |>
  left_join(mssm_geomeans) |>
  mutate(syn_scaled_biomass = biomass / syn_overlap_geomean,
         syn_scaled_lowerci = lowerci / syn_overlap_geomean,
         syn_scaled_upperci = upperci / syn_overlap_geomean,
         cpue_scaled_biomass = biomass / cpue_overlap_geomean,
         cpue_scaled_lowerci = lowerci / cpue_overlap_geomean,
         cpue_scaled_upperci = upperci / cpue_overlap_geomean,
         mssm_scaled_biomass = biomass / mssm_geomean,
         mssm_scaled_lowerci = lowerci / mssm_geomean,
         mssm_scaled_upperci = upperci / mssm_geomean,
       ) |>
  mutate(survey_abbrev = gsub("MSSM WCVI", "MSSM Model", survey_abbrev)) |>
  order_spp()

ind_layers <- function(colours = survey_cols, ncol = 5) {
  layers <- list(
      geom_line(),
      geom_point(),
      scale_colour_manual(values = colours),
      scale_fill_manual(values = colours),
      facet_wrap(~ species, scale = 'free_y', ncol = ncol),
      theme(legend.position = 'top',
            legend.title = element_blank(),
            axis.text.y = element_blank()),
      guides(color = guide_legend(direction = "horizontal"), fill = guide_legend(direction = "horizontal")),
      labs(x = 'Year', y = 'Relative biomass index')
    )
}

mssm_design_only_inds <-
  scaled_inds |>
  filter(species %in% spp_in_mssm_design_only,
         survey_abbrev %in% c("MSSM Design")) |>
ggplot(data = _, aes(x = year, y = mssm_scaled_biomass, colour = survey_abbrev, fill = survey_abbrev)) +
  geom_pointrange(aes(ymin = mssm_scaled_lowerci, ymax = mssm_scaled_upperci), size = 0.2) +
  geom_ribbon(aes(ymin = mssm_scaled_lowerci, ymax = mssm_scaled_upperci), size = 0.2, alpha = 0.1, colour = NA) +
  scale_colour_manual(values = survey_cols) +
  scale_fill_manual(values = survey_cols) +
  facet_wrap(~ species, scale = 'free_y', ncol = 4) +
  theme(legend.position = c(0.5, 1.055),
        legend.title = element_blank(),
        axis.text.y = element_blank()) +
  # geom_rect(data = . %>% distinct(species, .keep_all = TRUE),
  #     mapping = aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf),
  #     fill = "gray85", alpha = 0.3) +
  guides(colour = 'none', fill = 'none')
  #ggtitle("Design based index only") +
mssm_design_only_inds

ggsave(file.path(mssm_figs, 'index-mssm-design.png'), plot = mssm_design_only_inds,
  width = 10.5, height = 8)

mssm_model_design_inds <- scaled_inds |>
  filter(species %in% spp_in_mssm,
         survey_abbrev %in% c("MSSM Model")) |>
ggplot(data = _, aes(x = year, y = mssm_scaled_biomass, colour = survey_abbrev, fill = survey_abbrev)) +
  geom_ribbon(aes(ymin = mssm_scaled_lowerci, ymax = mssm_scaled_upperci), colour = NA, alpha = 0.3) +
  ind_layers() +
  geom_pointrange(data = scaled_inds |> filter(species %in% spp_in_mssm, survey_abbrev == "MSSM Design"),
    aes(ymin = mssm_scaled_lowerci, ymax = mssm_scaled_upperci), size = 0.2, alpha = 0.7)
  #ggtitle("Comparison of Modelled and Design based index")
mssm_model_design_inds

ggsave(file.path(mssm_figs, 'index-model-design.png'), plot = mssm_model_design_inds,
  width = 10.5, height = 10)

mssm_syn_inds <- scaled_inds |>
  filter(species %in% spp_in_mssm,
         survey_abbrev %in% c("MSSM Model", "SYN WCVI")) |>
ggplot(data = _, aes(x = year, y = syn_scaled_biomass, colour = survey_abbrev, fill = survey_abbrev)) +
  geom_ribbon(aes(ymin = syn_scaled_lowerci, ymax = syn_scaled_upperci), colour = NA, alpha = 0.3) +
  ind_layers() +
geom_rect(data = . %>% filter(!(species %in% unique(syn_inds$species))) %>%
    distinct(species, .keep_all = TRUE),
    mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill = "white", colour = NA)
mssm_syn_inds

ggsave(file.path(mssm_figs, 'index-mssm-model-syn-wcvi-model.png'), plot = mssm_syn_inds,
  width = 10.5, height = 10)

mssm_syn_inds_mssm_grid <- scaled_inds |>
  filter(species %in% spp_in_mssm,
         survey_abbrev %in% c("MSSM Model", "SYN WCVI on MSSM Grid")) |>
ggplot(data = _, aes(x = year, y = syn_scaled_biomass, colour = survey_abbrev, fill = survey_abbrev)) +
  geom_ribbon(aes(ymin = syn_scaled_lowerci, ymax = syn_scaled_upperci), colour = NA, alpha = 0.3) +
  ind_layers() +
  geom_rect(data = . %>% filter(!(species %in% unique(syn_inds$species))) %>%
    distinct(species, .keep_all = TRUE),
    mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill = "white", colour = NA) +
  labs(x = 'Year', y = 'Relative biomass index')
mssm_syn_inds_mssm_grid

ggsave(file.path(mssm_figs, 'index-mssm-model-syn-wcvi-model-mssm-grid.png'), plot = mssm_syn_inds_mssm_grid,
  width = 10.5, height = 10)


mssm_syn_grid_zoom_in <- scaled_inds |>
  filter(species %in% spp_in_mssm,
         survey_abbrev %in% c("MSSM Model", "SYN WCVI", "SYN WCVI on MSSM Grid")) |>
  filter(year > 2003) |>
  filter(species != 'shiner perch') |>
ggplot(data = _, aes(x = year, y = syn_scaled_biomass, colour = survey_abbrev, fill = survey_abbrev)) +
  geom_ribbon(aes(ymin = syn_scaled_lowerci, ymax = syn_scaled_upperci), colour = NA, alpha = 0.15) +
  ind_layers(colours = c("SYN WCVI" = "#7570b3", "SYN WCVI on MSSM Grid" = "#a6761d",
    "MSSM Model" = "#1b9e77"),
    ncol = 4)
mssm_syn_grid_zoom_in
ggsave(file.path(mssm_figs, 'index-mssm-syn-wcvi-mssm-grid-zoom-in.png'), plot = mssm_syn_grid_zoom_in,
  width = 9.5, height = 11)

mssm_cpue_inds <- scaled_inds |>
  filter(species %in% spp_in_mssm,
         survey_abbrev %in% c("MSSM Model", "CPUE 3CD")) |>
ggplot(data = _, aes(x = year, y = cpue_scaled_biomass, colour = survey_abbrev, fill = survey_abbrev)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = cpue_scaled_lowerci, ymax = cpue_scaled_upperci), colour = NA, alpha = 0.3) +
  geom_vline(xintercept = c(2003), colour = 'grey80') +
  scale_colour_manual(values = survey_cols) +
  scale_fill_manual(values = survey_cols) +
  facet_wrap(~ species, scale = 'free_y') +
  #ggtitle("Comparison of Modelled MSSM and CPUE 3CD") +
  ind_layers() +
  geom_rect(data = . %>% filter(!(species %in% unique(cpue_ind$species))) %>%
    distinct(species, .keep_all = TRUE),
    mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill = "white", colour = NA)
mssm_cpue_inds

ggsave(file.path(mssm_figs, 'index-mssm-model-cpue3CD.png'), plot = mssm_cpue_inds,
  width = 10.5, height = 8)


# --- Rolling window correlation ---
mssm_index_type <- "MSSM Design"
mssm_index_type <- "MSSM Model"
comp_index_type <- "CPUE 3CD"
comp_index_type <- "SYN WCVI"
comp_index_type <- "SYN WCVI on MSSM Grid"

get_mssm_cor <- function(mssm_index_type, comp_index_type, cor_thresh = 0.5, window = 10) {
  year_cutoff <- ifelse(grepl("SYN WCVI", comp_index_type), 2004, 1996)

  if ((comp_index_type == "CPUE 3CD")) {
    spp_intersect <- intersect(unique(mssm_3km_inds$species), unique(cpue_ind$species))
  } else {
    spp_intersect <- intersect(unique(mssm_3km_inds$species), unique(syn_inds$species))

  }

  mssm_cor_df <- scaled_inds |>
    filter(species %in% spp_intersect,
           survey_abbrev %in% c(mssm_index_type, comp_index_type),
           year >= year_cutoff) |>
    select(species, year, biomass, survey_abbrev) |>
    tidyr::pivot_wider(names_from = survey_abbrev, values_from = biomass)
  mssm_cor_list <- split(x = mssm_cor_df, mssm_cor_df$species) |>
    map(\(x) select(x, -species) |> arrange(year)) %>%
    keep(~ nrow(.x) > 0)

  # Correlation for full time series
  if (window == 0) {
    cor_log_biomass <- mssm_cor_list |>
      imap_dfr(\(dat, i) {
        cor_vals <- cor(log(dat[, mssm_index_type]), log(dat[, comp_index_type]))
        cor_df <- tibble(species = i, cor_val = cor_vals[[1]], input = 'log(biomass)')
      })
    cor_df <- cor_log_biomass |>
     mutate(comp = paste0(mssm_index_type, ' ~ ', comp_index_type))
  } else {
  # Rolling window correlation
    cor_biomass <- mssm_cor_list |>
      imap_dfr(\(dat, i) {
        cor_vals <- rollapply(dat, width = window, FUN = function(x) cor(x[, mssm_index_type], x[, comp_index_type]),
          by.column = FALSE)
        cor_df <- tibble(species = i, cor_vals = cor_vals, input = 'biomass', start_year = dat$year[1:length(cor_vals)])
      }
    )

    cor_log_biomass <- mssm_cor_list |>
      imap_dfr(\(dat, i) {
        cor_vals <- rollapply(dat, width = window, FUN = function(x) cor(log(x[, mssm_index_type]), log(x[, comp_index_type])),
          by.column = FALSE)
        cor_df <- tibble(species = i, cor_vals = cor_vals, input = 'log(biomass)', start_year = dat$year[1:length(cor_vals)])
      }
    )

    cor_df <- bind_rows(cor_biomass, cor_log_biomass) |>
      group_by(species, input) |>
      order_spp() |>
      drop_na()

    good_spp <- unique(cor_df$species)

    if (!is.null(cor_thresh)) {
      good_spp <- cor_df |>
        mutate(max_year = max(start_year)) |>
        filter(start_year > max_year - 5) |>
        group_by(species) |>
        summarise(mean_cor = mean(cor_vals), .groups = 'drop') |>
        filter(mean_cor >= cor_thresh) |>
        pluck('species')
    }

    cor_df |>
     filter(input == 'log(biomass)') |>
     filter(species %in% good_spp) |>
     mutate(comp = paste0(mssm_index_type, ' ~ ', comp_index_type)) |>
     ungroup()
   }
}

# cor_thresh = 0.25
#cor_thresh = 0.50
cor_thresh = NULL
cor1 <- get_mssm_cor(mssm_index_type = "MSSM Model", comp_index_type = "CPUE 3CD", cor_thresh = cor_thresh)
cor2 <- get_mssm_cor(mssm_index_type = "MSSM Design", comp_index_type = "CPUE 3CD", cor_thresh = cor_thresh)
cor3 <- get_mssm_cor(mssm_index_type = "MSSM Model", comp_index_type = "SYN WCVI", cor_thresh = cor_thresh)
cor4 <- get_mssm_cor(mssm_index_type = "MSSM Design", comp_index_type = "SYN WCVI", cor_thresh = cor_thresh)
cor5 <- get_mssm_cor(mssm_index_type = "MSSM Model", comp_index_type = "SYN WCVI on MSSM Grid", cor_thresh = cor_thresh)

full_cor1 <- get_mssm_cor(mssm_index_type = "MSSM Model", comp_index_type = "CPUE 3CD", cor_thresh = NULL, window = 0)
full_cor2 <- get_mssm_cor(mssm_index_type = "MSSM Model", comp_index_type = "SYN WCVI", cor_thresh = NULL, window = 0)
full_cor3 <- get_mssm_cor(mssm_index_type = "MSSM Model", comp_index_type = "SYN WCVI on MSSM Grid", cor_thresh = NULL, window = 0)

full_cor <- bind_rows(full_cor1, full_cor2, full_cor3)

cor_df <- bind_rows(cor1, cor2, cor3, cor4, cor5) |>
  mutate(comp = factor(comp, levels = c(
    "MSSM Model ~ CPUE 3CD",
    'MSSM Model ~ SYN WCVI',
    "MSSM Design ~ CPUE 3CD",
    'MSSM Design ~ SYN WCVI',
    "MSSM Model ~ SYN WCVI on MSSM Grid"))) |>
  filter(comp %in% c("MSSM Model ~ CPUE 3CD", "MSSM Model ~ SYN WCVI", "MSSM Model ~ SYN WCVI on MSSM Grid")) |>
  left_join(full_cor |>
      rename(mean_cor = 'cor_val') |>
      group_by(comp) |>
      ungroup()) |>
  filter(mean_cor >= 0.5)

cor_plot <-
  ggplot(cor_df, aes(x = start_year, y = cor_vals)) +
      geom_point(data = cor_df |> group_by(comp) |> slice(which.max(start_year)) |>
        mutate(start_year = start_year + 6), alpha = 0) +  # variable x limit increaser
      geom_hline(yintercept = 0, colour = 'grey50') +
      geom_line(aes(colour = species)) +
      geom_smooth(se = FALSE) +
      guides(colour = "none") +
      facet_wrap(~ comp, scale = 'free_x') +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      coord_cartesian(clip = "off") +
        ggrepel::geom_text_repel(
        data = cor_df %>% group_by(comp, species) %>% slice(which.max(start_year)),
        aes(label = species, x = start_year, colour = species),
        size = 3.2, hjust = 'left', segment.color = 'grey85',
        nudge_x = 0.3, box.padding = 0.1, point.padding = 0.5,
        direction = "y"
      ) +
      labs(x = "Start year of 10-year rolling window",
           y = "Correlation")
cor_plot

ggsave(file.path(mssm_figs, 'index-correlation.png'), plot = cor_plot,
  width = 10.5, height = 4.3)

# ------------------------------------------------------------------------------

# Get length and age distributions
size_dat <- spp_vector |>
  map(\(sp) readRDS(file.path(data_cache, paste0(gfsynopsis:::clean_name(sp), ".rds")))$survey_samples) |>
  bind_rows() |>
  filter(survey_abbrev %in% c('MSSM WCVI', 'SYN WCVI')) |>
  select(species_common_name, year, survey_abbrev, specimen_id, sample_id, sex, age,
        length, weight, length_type)


size_summary <- size_dat |>
  #filter(species_common_name %in% spp_in_mssm) |>
  filter(survey_abbrev %in% c('MSSM WCVI', 'SYN WCVI')) |>
  group_by(species_common_name, survey_abbrev) |>
  summarise(q50 = quantile(length, 0.5, na.rm = TRUE),
            q25 = quantile(length, 0.25, na.rm = TRUE),
            q75 = quantile(length, 0.75, na.rm = TRUE),
            n = sum(!is.na(length))) |>
  mutate(mean_length = mean(q50)) |>
  ungroup()

size_diff_lu <-
  size_summary |>
  pivot_wider(id_cols = species_common_name, names_from = survey_abbrev, values_from = q50) |>
  mutate(syn_minus_mssm_q50 = `SYN WCVI` - `MSSM WCVI`,
         abs_diff = abs(syn_minus_mssm_q50)) |>
  select(species_common_name, syn_minus_mssm_q50, abs_diff) |>
  mutate(bigger = case_when(syn_minus_mssm_q50 > 0 ~ 'SYN',
                          syn_minus_mssm_q50 < 0 ~ 'MSSM',
                          TRUE ~ "Equal")) |>
  arrange(bigger, abs_diff) |>
  mutate(bg = ifelse(row_number() %% 2 == 1, 'grey95', NA))

size_comp <-
  left_join(size_summary, size_diff_lu) |>
  group_by(species_common_name) |>
  filter(n() == 2) |>
  ungroup() |>
  mutate(species_common_name = factor(species_common_name, levels = pluck(size_diff_lu, 'species_common_name'))) |>
    arrange(species_common_name) |>
  ggplot(aes(x = species_common_name, y = q50, colour = survey_abbrev)) +
    geom_tile(aes(height = Inf, width = 1, fill = bg), colour = NA, alpha = 0.3) +
    geom_pointrange(mapping = aes(ymin = q25, ymax = q75),
       position = position_dodge(width = 0.4)) +
    scale_colour_manual(values = survey_cols) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank()) +
    scale_fill_identity() +
    facet_grid(~ bigger, drop = TRUE, scales = 'free_x', space = 'free') +
    labs(colour = "Survey", y = 'Length (cm)') +
    theme(legend.position = c(0.5, -0.5), legend.direction = "horizontal")
size_comp

ggsave(file.path(mssm_figs, 'size-comp.png'), plot = size_comp,
  width = 13, height = 4.5)

age_summary <- size_dat |>
  #filter(species_common_name %in% spp_in_mssm) |>
  filter(survey_abbrev %in% c('MSSM WCVI', 'SYN WCVI')) |>
  group_by(species_common_name, survey_abbrev) |>
  filter(!is.na(age)) |>
  summarise(q50 = quantile(age, 0.5, na.rm = TRUE),
            q25 = quantile(age, 0.25, na.rm = TRUE),
            q75 = quantile(age, 0.75, na.rm = TRUE),
            n = n()) |>
  #mutate(mean_age = mean(q50)) |>
  ungroup() |>
  filter(n != 0)


age_diff_lu <-
  age_summary |>
  pivot_wider(id_cols = species_common_name, names_from = survey_abbrev, values_from = q50) |>
  mutate(syn_minus_mssm_q50 = `SYN WCVI` - `MSSM WCVI`,
         abs_diff = abs(syn_minus_mssm_q50)) |>
  select(species_common_name, syn_minus_mssm_q50, abs_diff) |>
  mutate(bigger = case_when(syn_minus_mssm_q50 > 0 ~ 'SYN',
                          syn_minus_mssm_q50 < 0 ~ 'MSSM',
                          TRUE ~ "Equal")) |>
  arrange(bigger, abs_diff) |>
  mutate(bg = ifelse(row_number() %% 2 == 1, 'grey95', NA)) |>
  filter(!is.na(abs_diff))  # ignore species that are not measured in MSSM

age_comp <-
  left_join(age_summary, age_diff_lu) |>
    mutate(species_common_name = factor(species_common_name, levels = pluck(age_diff_lu, 'species_common_name'))) |>
    arrange(species_common_name) |>
  ggplot(aes(x = species_common_name, y = q50, colour = survey_abbrev)) +
    geom_tile(aes(height = Inf, width = 1, fill = bg), colour = NA, alpha = 0.3) +
    geom_pointrange(mapping = aes(ymin = q25, ymax = q75),
       position = position_dodge(width = 0.4)) +
    scale_colour_manual(values = survey_cols) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank()) +
    scale_fill_identity() +
    facet_grid(~ bigger, drop = TRUE, scales = 'free_x', space = 'free') +
    labs(colour = 'Survey', y = 'Age') +
    theme(legend.position = "top")
age_comp

ggsave(file.path(mssm_figs, 'age-comp.png'), plot = age_comp,
  width = 4, height = 4)


# Mean correlation ~ size ---
full_cor |>
  left_join(size_diff_lu, by = c('species' = 'species_common_name')) |>
ggplot(data = _, aes(x = abs_diff, y = cor_val)) +
  geom_point() +
  geom_hline(yintercept = 0, colour = 'grey70') +
  facet_wrap(~ comp) +
  labs(x = "Absolute size difference (cm)", y = "Correlation")
ggsave(file.path(mssm_figs, 'size-survey-correlation.png'), width = 7.6, height = 2.7)

# Depth overlap
# No real difference in depth ranges surveyed over time
depth_comp <- bind_rows(sw_dat, mssm_dat) |>
 distinct(survey_series_id, fishing_event_id, .keep_all =TRUE)

depth_comp |> filter(year > 2003) |>
  ggplot(aes(x = depth_m, fill = year)) +
  #geom_histogram(alpha = 0.8) +
  geom_density(aes(group = year), alpha = 0.5) +
  #scale_fill_manual(values = survey_cols) +
  facet_wrap(~ survey_abbrev, scales = 'free_y', ncol = 1) +
  # geom_vline(xintercept = c(min(mssm_dat$depth_m, na.rm = TRUE), max(mssm_dat$depth_m, na.rm = TRUE)))
  # geom_rect(data = tibble(survey_abbrev = 'SYN WCVI', depth_m = min(mssm_dat$depth_m, na.rm = TRUE)), aes(xmin = -Inf, xmax = depth_m, ymin = -Inf, ymax = Inf), fill = 'grey50', alpha = 0.1) +
  # geom_rect(data = tibble(survey_abbrev = 'SYN WCVI', depth_m = max(mssm_dat$depth_m, na.rm = TRUE)), aes(xmax = Inf, xmin = depth_m, ymin = -Inf, ymax = Inf), fill = 'grey50', alpha = 0.1) +
  theme(axis.text.y = element_blank(),
        legend.position = c(0.7, 0.9), legend.direction = 'horizontal',
        legend.key.width=unit(0.05,"npc")) +
  labs(x = "Depth (m)", y = "Sampling frequency", fill = "Year")

ggsave(file.path(mssm_figs, 'depth-ranges-mssm-syn-wcvi.png'), width = 6.5, height = 4)



# Fish aggregates -----
# ---------------------
# How I got to more-mssm-spp.rds:
#fish <- gfdata::run_sql('GFBioSQL', query = "SELECT * FROM SPECIES")
# saveRDS(fish, file.path(mssm_appendix, 'gfbio_fish_list.rds'))
# fish <- readRDS(file.path(mssm_appendix, 'gfbio_fish_list.rds') |> as_tibble()

# # Look at higher order species classifications and choose species aggregations
# # to look for in the MSSM data
# distinct(fish, SPECIES_DESC, .keep_all = TRUE) |>
#   filter(TAXONOMIC_RANK != 'Species') |>
#   arrange(SPECIES_DESC) |>
#   select(SPECIES_CODE, SPECIES_DESC, SPECIES_COMMON_NAME, SPECIES_SCIENCE_NAME)

# spp_string <- textConnection("01P\tSANDDABS
# 042\tDOGFISH SHARKS
# 051\tSKATES
# 060\tCARTILAGINOUS FISH (SHARKS, SKATES, RAYS, RATFISH)
# 065\tRATFISHES
# 221\tCODFISHES
# 227\tCODS/HAKES/GRENADIERS
# 231\tEELPOUTS
# 234\tEELPOUT
# 388\tSCORPIONFISHES
# 389\tROCKFISHES
# 402\tROCKFISHES
# 465\tLINGCOD
# 472\tSCULPINS
# 477\tSCULPINS/POACHERS/SCORPIONFISH
# 595\tLEFTEYE FLOUNDERS
# 597\tFLATFISHES
# 599\tRIGHTEYE FLOUNDERS
# 618\tPLEURONECTES"
# )

# more_spp_codes <- as_tibble(read.table(spp_string, col.names = c("species_code", "species_common_name"), header = FALSE, sep = "\t"))

# GFBIO query to get additional species
# more_spp <- get_survey_sets2(species = more_spp_codes$species_code, ssid = 7)
# saveRDS(more_spp, file.path(mssm_appendix, 'higher-taxonomic-mssm-spp.rds')
more_spp <- readRDS(file.path(mssm_appendix, 'higher-taxonomic-mssm-spp.rds')) |>
  filter(grouping_desc %in% c('WCVI Shrimp Survey Area 124', 'WCVI Shrimp Survey Area 125'))

# Check non-zero catches
more_spp %>% filter(catch_weight > 0 | catch_count > 0) |>
  #filter(species_common_name == 'cartilaginous fish (sharks, skates, rays, ratfish)') |>
  count(species_common_name, year) |>
  arrange(species_common_name, year) |>
  distinct(species_common_name)

more_spp %>% filter(catch_weight == 0 & catch_count == 0) |>
  #filter(species_common_name == 'cartilaginous fish (sharks, skates, rays, ratfish)') |>
  count(species_common_name, year) |>
  arrange(species_common_name, year) |>
  distinct(species_common_name)

# What is going on in 2002???
# - Does not look like there are any duplicate fishing events
more_spp |>
  #filter(species_common_name == 'skates') |>
  filter(year == 2002) |>
  arrange(-catch_weight) |>
  ggplot(aes(x = species_common_name, y = catch_weight)) +
  geom_point() +
  ggrepel::geom_text_repel(
      aes(x = species_common_name, y = catch_weight, label = fishing_event_id),
      size = 3.5, segment.color = 'grey85',
      nudge_x = 0.1, box.padding = 0.1, point.padding = 0.8,
      direction = "y"
    ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

more_spp |>
  #filter(year == 2002) |>
  arrange(-catch_weight) |>
  slice(1:30) |>
  distinct(fishing_event_id, .keep_all = TRUE) |>
    ggplot(aes(x = species_common_name, y = catch_weight)) +
  geom_point() +
  ggrepel::geom_text_repel(
      aes(x = species_common_name, y = catch_weight, label = paste(fishing_event_id, year, sep = "-")),
      size = 3.5, segment.color = 'grey85',
      nudge_x = 0.1, box.padding = 0.1, point.padding = 0.8,
      direction = "y", hjust = 0
    ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Likely a decimal point error for fishing_event_id %in% c()
hist(((more_spp |> filter(species_common_name == 'skates', catch_weight > 5))$catch_weight))

test <- bind_rows(more_spp |> filter(year == 2002), mssm_dat |> filter(year == 2002))

filter(test, fishing_event_id %in% c(902190, 902182, 902187)) %>% view()

big_catches <-
  test |>
    group_by(fishing_event_id) |>
    mutate(total_catch = sum(catch_weight)) |>
    arrange(-total_catch, fishing_event_id) |>
    ungroup() |>
    distinct(fishing_event_id, total_catch) |>
    slice(1:5)

test |> filter(fishing_event_id %in% pluck(big_catches, 'fishing_event_id')) |>
filter(catch_weight > 10) |>
ggplot(aes(x = species_common_name, y = catch_weight)) +
  geom_point() +
  facet_wrap(~fishing_event_id, scales = 'free_x', ncol = 5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#mssm_dat |>
test |>
  #filter(species_common_name == 'skates') |>
  filter(year == 2002) |>
  filter(catch_weight != 0) |>
  group_by(species_common_name) |>
  mutate(max_catch = max(catch_weight), mean_catch = mean(catch_weight)) |>
  #filter(max_catch > 3 * mean_catch) |>
  ungroup() |>
  arrange(-catch_weight) |>
  ggplot(aes(x = species_common_name, y = catch_weight)) +
  geom_point() +
  ggrepel::geom_text_repel(
      aes(x = species_common_name, y = catch_weight, label = fishing_event_id),
      size = 3.5, segment.color = 'grey85',
      nudge_x = 0.1, box.padding = 0.1, point.padding = 0.8,
      direction = "y"
    ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


more_spp_summ <- more_spp |>
  group_by(species_common_name, species_science_name, year) |>
  summarise(mean_catch = mean(catch_weight, na.rm = TRUE), .groups = 'drop') |>
  mutate(parent_taxonomic_unit = species_science_name, parent_taxonomic_unit)


spp_group_df <- mssm_dat |>
  group_by(species_common_name, species_science_name, year) |>
  summarise(mean_catch = mean(catch_weight, na.rm = TRUE), .groups = 'drop') |>
  left_join(gfsynopsis::get_spp_names()) |>
  mutate(parent_taxonomic_unit = ifelse(species_common_name == "pacific halibut", "pleuronectidae(righteye flounders)", parent_taxonomic_unit)) |>
  mutate(parent_taxonomic_unit = ifelse(species_common_name == "pacific hake", "merlucciidae", parent_taxonomic_unit)) |>
  mutate(parent_taxonomic_unit = ifelse(is.na(parent_taxonomic_unit), species_science_name, parent_taxonomic_unit))

# Baseplot
agg_plot <- function(df, ncol = 1, scales = 'free_y') {
  ggplot(data = df) +
    geom_rect(aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf),
            fill = "gray85", alpha = 0.2) +
    geom_vline(xintercept = 2001, colour = 'grey50') +
    geom_point(aes(x = year, y = mean_catch)) +
    labs(x =  "Year", y = "Mean annual catch (kg)") +
    facet_wrap(~ species_common_name, scales = scales, ncol = ncol)
  }

# All higher order species aggregations f families caught in MSSM
spp_group_plot <-
  more_spp_summ |>
  filter(species_common_name %in% c('eelpouts', 'flatfishes', 'rockfishes', 'sculpins', 'skates')) |>
  mutate(species_common_name = str_to_title(species_common_name)) |>
agg_plot(ncol = 3)
spp_group_plot

ggsave(spp_group_plot, filename = file.path(mssm_figs, 'aggregated-spp-plot.png'), width = 8, height = 4)

#
plot_combined_spp <- function(family, common_name) {
  df1 <- spp_group_df |>
    filter(str_detect(parent_taxonomic_unit, family)) |>
    group_by(year) |>
    summarise(mean_catch = mean(mean_catch), .groups = 'drop') |>
    mutate(species_common_name = 'species combined')

  df2 <- more_spp_summ |>
    filter(str_detect(parent_taxonomic_unit, family)) |>
    group_by(year) |>
    summarise(mean_catch = mean(mean_catch), .groups = 'drop') |>
    mutate(species_common_name = common_name)

  dat <- bind_rows(df1, df2)

  ggplot(data = dat) +
    geom_rect(aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf),
            fill = "gray90", alpha = 0.1) +
    geom_vline(xintercept = 2001, colour = 'grey50') +
    geom_point(aes(x = year, y = mean_catch, colour = species_common_name)) +
    scale_colour_brewer(palette = "Dark2", type = 'qual') +
    labs(x =  "Year", y = "Mean annual catch (kg)")
}

add_group_common_name <- function(df) {
  df |>
  mutate(group_common_name = case_when(
      str_detect(parent_taxonomic_unit, "cottidae") ~ 'sculpins',
      str_detect(parent_taxonomic_unit, "pleuronect|paralich") ~ 'flatfishes',
      str_detect(parent_taxonomic_unit, "rajidae") ~ 'skates',
      str_detect(parent_taxonomic_unit, "sebastes") ~ 'rockfishes',
      str_detect(parent_taxonomic_unit, "zoarcidae") ~ 'eelpouts'))
}

plot_combined_spp('cottidae', "'sculpins'")
plot_combined_spp('pleuronect|paralich', "'flatfishes'")
plot_combined_spp('rajidae', "'skates'")
plot_combined_spp('sebastes', "'rockfishes'")
plot_combined_spp('zoarcidae', "'eelpouts'")

df1 <- spp_group_df |>
  filter(str_detect(parent_taxonomic_unit, c("cottidae|pleuronect|paralich|rajidae|sebastes|zoarcidae"))) |>
  add_group_common_name() |>
  group_by(group_common_name, year) |>
  summarise(mean_catch = mean(mean_catch), .groups = 'drop') |>
  mutate(species_common_name = 'Species')

df2 <- more_spp_summ |>
  filter(str_detect(parent_taxonomic_unit, c("cottidae|pleuronect|paralich|rajidae|sebastes|zoarcidae"))) |>
  add_group_common_name() |>
  group_by(group_common_name, year) |>
  summarise(mean_catch = mean(mean_catch), .groups = 'drop') |>
  mutate(species_common_name = 'Order/Family')

dat <- bind_rows(df1, df2) |>
  mutate(group_common_name = str_to_title(group_common_name))

spp_group_plot2 <-
  ggplot(data = dat, aes(x = year, y = mean_catch, colour = species_common_name, shape = species_common_name)) +
    geom_rect(aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf),
            fill = "gray90", colour = NA, alpha = 0.1) +
    geom_vline(xintercept = 2001, colour = 'grey50') +
    geom_point() +
    geom_line(linewidth = 0.3) +
    scale_colour_brewer(palette = "Dark2", type = 'qual') +
    labs(x =  "Year", y = "Mean annual catch (kg)", shape = "Identification level", colour = 'Identification level') +
    facet_wrap(~ group_common_name, scales = "free_y", ncol = 2) +
    theme(legend.position = c(0.7, 0.15))
spp_group_plot2

ggsave(filename = file.path(mssm_figs, 'aggregated-id-level-plot.png'), width = 7.7, height = 5.2)



# Sculpins ---
sculpins <- filter(spp_group_df, str_detect(parent_taxonomic_unit, 'cottidae'))
sculpin_levels <- c('sculpins', unique(sculpins$species_common_name)[unique(sculpins$species_common_name) != 'sculpins'], 'scuplins combined')
sculpins_all <- sculpins |>
  group_by(year) |>
  summarise(mean_catch = sum(mean_catch)) |>
  mutate(species_common_name = factor('scuplins combined', levels = sculpin_levels))
#sculpins <- bind_rows(sculpins, sculpins_all)

p_sculpin <- sculpins |>
  mutate(species_common_name = factor(species_common_name, levels = sculpin_levels)) |>
  agg_plot(ncol = 1, scales = 'fixed')
p_sculpin

# Flatfish ---
flatfish <- filter(spp_group_df, str_detect(parent_taxonomic_unit, 'pleuronect|paralich'))
flatfish_levels <- c('flatfishes', unique(flatfish$species_common_name)[unique(flatfish$species_common_name) != 'flatfishes'], 'flatfishes combined')
flatfish_all <- flatfish |>
  group_by(year) |>
  summarise(mean_catch = sum(mean_catch)) |>
  mutate(species_common_name = factor('flatfishes combined', levels = flatfish_levels))
#flatfish <- bind_rows(flatfish, flatfish_all)

p_flatfish <- flatfish |>
  mutate(species_common_name = factor(species_common_name, levels = flatfish_levels)) |>
  agg_plot(ncol = 2)
p_flatfish

# Skates ---
skates <- filter(spp_group_df, str_detect(parent_taxonomic_unit, 'rajidae'))
skates_levels <- c('skates', unique(skates$species_common_name)[unique(skates$species_common_name) != 'skates'], 'skates combined')
skates_all <- skates |>
  group_by(year) |>
  summarise(mean_catch = sum(mean_catch)) |>
  mutate(species_common_name = factor('skates combined', levels = skates_levels))
#skates <- bind_rows(skates, skates_all)

p_skate <- skates |>
  mutate(species_common_name = factor(species_common_name, levels = skates_levels)) |>
  agg_plot(ncol = 1, scales = 'fixed') +
  theme(axis.title.y = element_blank())

# Rockfish ---
rockfish <- filter(spp_group_df, str_detect(parent_taxonomic_unit, 'sebastes'))
rockfish_levels <- c('rockfishes', unique(rockfish$species_common_name)[unique(rockfish$species_common_name) != 'rockfishes'], 'rockfishes combined')
rockfish_all <- rockfish |>
  group_by(year) |>
  summarise(mean_catch = sum(mean_catch)) |>
  mutate(species_common_name = factor('rockfishes combined', levels = rockfish_levels))
#rockfish <- bind_rows(rockfish, rockfish_all)

p_rockfish <- rockfish |>
  mutate(species_common_name = factor(species_common_name, levels = rockfish_levels)) |>
  agg_plot(ncol = 2)

# Eelpouts ---
eelpouts <- filter(spp_group_df, str_detect(parent_taxonomic_unit, 'zoarcidae'))
eelpouts_levels <- c('all eelpouts', 'eelpouts', unique(eelpouts$species_common_name)[unique(eelpouts$species_common_name) != 'eelpouts'])
eelpouts_all <- eelpouts |>
  group_by(year) |>
  summarise(mean_catch = sum(mean_catch)) |>
  mutate(species_common_name = factor('all eelpouts', levels = eelpouts_levels))
eelpouts <- bind_rows(eelpouts, eelpouts_all)

p_eelpout <- eelpouts |>
  mutate(species_common_name = factor(species_common_name, levels = eelpouts_levels)) |>
  agg_plot(ncol = 1)

design <- "
  12
  12
  12
  1#
  1#
"
p_sculpin + p_skate + plot_layout(design = design)
ggsave(filename = file.path(mssm_figs, 'agg-sculpin-skate.png'), width = 7, height = 7)

optimize_png <- TRUE
if (optimize_png) {
  cores <- parallel::detectCores()
  files_per_core <- 4L
  wd <- getwd()
  setwd(here::here("report", "mssm-appendix", "figures"))
  if (!gfplot:::is_windows()) {
    system(paste0(
      "find -X . -name '*.png' -print0 | xargs -0 -n ",
      files_per_core, " -P ", cores, " optipng -strip all"
    ))
  }
  setwd(wd)
}

