# set_data <- readRDS("~/src/gfdata/data-raw/pcod-set-data.rds")
# spp_name <- set_data$species_common_name[1]

set_data <- readRDS("report/data-cache-2026-07/survey-sets-synopsis.rds")
set_data <- dplyr::filter(set_data, survey_abbrev != "IPHC FISS")
spp_name <- sort(unique(set_data$species_common_name))

# Match gfdata::get_all_survey_sets()'s doorspread_m mean-fill (added in
# trawl_area_swept()) and recompute density_kgpm2 from it, since this cached
# data was pulled before that fix existed.
set_data <- gfdata:::trawl_area_swept(set_data)

# remove_false_zeros in get_all_survey_sets() blanks catch_weight to NA when
# catch_count > 0 but catch_weight == 0 (untrustworthy "false zero"). Treat
# those as zero weight for now rather than dropping the tow to NA density.
set_data$catch_weight <- ifelse(
  is.na(set_data$catch_weight) & set_data$catch_count > 0,
  0,
  set_data$catch_weight
)

set_data$density_kgpm2 <- set_data$catch_weight / set_data$area_swept
set_data$density_kgpm2 <- ifelse(!is.na(set_data$area_swept), set_data$density_kgpm2, NA)

hbll_out_lu <- structure(list(grouping_code = c(448, 449, 450, 451, 452, 453,
  454, 455, 456, 457, 458, 459), fe_grouping_code = c(321, 322,
    323, 321, 322, 323, 321, 322, 323, 321, 322, 323)), row.names = c(NA,
      -12L), class = "data.frame")

hbll_ins_lu <- structure(list(grouping_code = c(279, 280, 281, 282), fe_grouping_code = c(317,
  318, 317, 318)), row.names = c(NA, -4L), class = "data.frame")

# temp fix:
set_data <- set_data |>
  dplyr::left_join(hbll_out_lu, by = "grouping_code") |>
  dplyr::mutate(
    grouping_code = dplyr::if_else(!is.na(fe_grouping_code), fe_grouping_code, grouping_code)
  ) |>
  dplyr::select(-fe_grouping_code) |>
  dplyr::group_by(survey_series_id, grouping_code) |>
  dplyr::mutate(
    grouping_area_km2 = ifelse(
      is.na(grouping_area_km2) & any(!is.na(grouping_area_km2)),
      unique(stats::na.omit(grouping_area_km2))[1],
      grouping_area_km2
    )
  ) |>
  dplyr::ungroup()

# temp fix:
set_data <- set_data |>
  dplyr::left_join(hbll_ins_lu, by = "grouping_code") |>
  dplyr::mutate(
    grouping_code = dplyr::if_else(!is.na(fe_grouping_code), fe_grouping_code, grouping_code)
  ) |>
  dplyr::select(-fe_grouping_code) |>
  dplyr::group_by(survey_series_id, grouping_code) |>
  dplyr::mutate(
    grouping_area_km2 = ifelse(
      is.na(grouping_area_km2) & any(!is.na(grouping_area_km2)),
      unique(stats::na.omit(grouping_area_km2))[1],
      grouping_area_km2
    )
  ) |>
  dplyr::ungroup()

tictoc::tic()
indexes <- purrr::map(spp_name, \(sp) {
  cat(sp, "\n")
  this_dat <- dplyr::filter(set_data, species_common_name == sp)
  ssids <- sort(unique(stats::na.omit(this_dat$survey_series_id)))
  purrr::map_dfr(ssids, \(ssid) {
    .dat <- dplyr::filter(this_dat, survey_series_id == ssid)
    cat(" SSID: ", ssid, "\n")
    out <- gfdata::get_design_index(sp, ssid = ssid, reps = 200L, data = this_dat)
    out$species_common_name <- sp
    out$survey_series_id <- ssid
    out
  })
}) |>
  dplyr::bind_rows()
tictoc::toc()
