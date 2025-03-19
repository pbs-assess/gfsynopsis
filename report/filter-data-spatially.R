library(dplyr)
library(ggplot2)

d0 <- readRDS(here::here("report", "data-cache-2025-03", "north-pacific-spiny-dogfish.rds"))

shp <- sf::st_read(here::here("report/spatial-filtering/shape-files/haida/"))

# ---------------------------
# Filter the data
# ---------------------------
# --- survey_sets ---
filtered_survey_sets <- d0$survey_sets |>
  subset_spatial(sf_poly = shp, xy_coords = c("longitude", "latitude"), dat_crs = 4326, return_sf = TRUE)

# --- survey_samples --- # actually I think let's go with the old version of the
# data call to get filtered version running
filtered_survey_samples <- filtered_survey_sets |>
  dplyr::select(survey_series_id = survey_series_id.x, fishing_event_id, latitude, longitude) |>
  inner_join(d0$survey_samples)

# compare with get_all survey - this one has more, will use for now as this is
# what I think we're going to switch to with gfsynopsis
# filtered_survey_samples <- survey_samples |> subset_spatial(sf_poly = shp, xy_coords = c("longitude", "latitude"), dat_crs = 4326, return_sf = TRUE)

# --- commercial_samples ---
pmfc_areas <- paste(c("5b", "5c", "5d", "5e"), collapse = "|^")
filtered_comm_samps_alltime <- d0$commercial_samples |>
  filter(stringr::str_detect(
    major_stat_area_name,
    pattern = stringr::regex(paste0("(^", pmfc_areas, ")"), ignore_case = TRUE)
    )
  )
filtered_comm_samps_latlon <- d0$commercial_samples |>
  filter(!is.na(latitude) & !is.na(longitude)) |> # this will result in only records from 2006 onwards
  subset_spatial(sf_poly = shp, xy_coords = c("longitude", "latitude"), dat_crs = 4326, return_sf = TRUE)
table(filtered_comm_samps_latlon$year)

# --- catch ---
pmfc_areas <- paste(c("5b", "5c", "5d", "5e"), collapse = "|^")
filtered_catch_alltime <- d0$catch |>
  filter(stringr::str_detect(
    major_stat_area_name,
    pattern = stringr::regex(paste0("(^", pmfc_areas, ")"), ignore_case = TRUE)
    )
  )

filtered_catch_latlon <- d0$catch |>
  filter(!is.na(lat) & !is.na(lon)) |> # this will result in only records from 2006 onwards
  subset_spatial(sf_poly = shp, xy_coords = c("lon", "lat"), dat_crs = 4326, return_sf = TRUE)
table(filtered_catch_latlon$year)

# --- cpue spatial ---
filtered_trawl_cpue <- d0$cpue_spatial |>
  subset_spatial(sf_poly = shp, xy_coords = c("lon", "lat"), dat_crs = 4326, return_sf = TRUE)

# --- cpue spatial_ll ---
filtered_ll_cpue <- d0$cpue_spatial_ll |>
  subset_spatial(sf_poly = shp, xy_coords = c("lon", "lat"), dat_crs = 4326, return_sf = TRUE)

# --- catch_spatial ---
filtered_catch_spatial <- d0$catch_spatial |>
  subset_spatial(sf_poly = shp, xy_coords = c("lon", "lat"), dat_crs = 4326, return_sf = TRUE)

# --- survey_index ---
# not using

# --- age_precision ---
filtered_age_samples <- filtered_survey_samples |>
  dplyr::select(survey_series_id, fishing_event_id, specimen_id, latitude, longitude) |>
  inner_join(d0$age_precision)

spatially_filtered <- list(
  "survey_sets" = filtered_survey_sets,
  "survey_samples" = filtered_survey_samples,
  "commercial_samples" = filtered_comm_samps_latlon, # FIXME: will need to update to deal with pre 2006
  "catch" = filtered_catch_alltime,
  "cpue_spatial" = filtered_trawl_cpue,
  "cpue_spatial_ll" = filtered_ll_cpue,
  "catch_spatial" = filtered_catch_spatial,
  "survey_index" = ling0$survey_index, # TODO: update with spatially filtered after adding new strata grid lookup
  "age_precision" = filtered_age_samples
)
