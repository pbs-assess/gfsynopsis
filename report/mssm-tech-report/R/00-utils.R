# Include this function here instead of relying on the gfsynopsis in case of refactoring
prep_stitch_dat <- function(survey_dat, bait_counts) {
  if ("survey_series_id.x" %in% names(survey_dat)) {
    colnames(survey_dat)[colnames(survey_dat) == "survey_series_id.x"] <- "survey_series_id"
  }

  ll <- grepl("HBLL", unique(survey_dat$survey_abbrev))
  if (sum(ll) > 0) {
    survey_dat <- dplyr::left_join(survey_dat, bait_counts,
      by = c("year", "fishing_event_id", "survey_series_id" = "ssid")
    ) |>
      dplyr::mutate(count_bait_only = replace(count_bait_only, which(count_bait_only == 0), 1)) |>
      dplyr::mutate(prop_bait_hooks = count_bait_only / hook_count) |>
      dplyr::mutate(
        hook_adjust_factor = -log(prop_bait_hooks) / (1 - prop_bait_hooks),
        prop_removed = 1 - prop_bait_hooks
      )
  }
  out <-
    survey_dat |>
    sdmTMB::add_utm_columns(c("longitude", "latitude"), utm_crs = 32609) |>
    # @FIXME: area swept has been or will be added to gfdata function
    dplyr::mutate(
      area_swept1 = doorspread_m * (speed_mpm * duration_min),
      area_swept2 = tow_length_m * doorspread_m,
      area_swept = dplyr::case_when(
        grepl("SYN", survey_abbrev) & !is.na(area_swept2) ~ area_swept2,
        grepl("SYN", survey_abbrev) & is.na(area_swept2) ~ area_swept1,
        grepl("HBLL", survey_abbrev) ~ hook_count * 0.0024384 * 0.009144 * 1000
      )
    ) |>
    dplyr::mutate(hook_adjust_factor = ifelse(
      grepl("SYN", survey_abbrev), NA, hook_adjust_factor)) |>
    dplyr::mutate(offset = dplyr::case_when(
      grepl("SYN", survey_abbrev) ~ log(area_swept / 1e5),
      grepl("HBLL", survey_abbrev) ~ log(hook_count / hook_adjust_factor)
    )) |>
    dplyr::mutate(catch = ifelse(grepl("SYN", survey_abbrev), catch_weight, catch_count)) |>
    dplyr::mutate(present = ifelse(catch > 0, 1, 0)) |>
    dplyr::mutate(survey_type = dplyr::case_when(
      grepl("SYN", survey_abbrev) ~ "synoptic",
      grepl("HBLL OUT", survey_abbrev) ~ "hbll_outside",
      grepl("HBLL INS", survey_abbrev) ~ "hbll_inside"
    )) |>
    dplyr::mutate(log_hook_count = log(hook_count)) |>
    dplyr::mutate(fyear = as.factor(year)) |>
    dplyr::mutate(prop_removed = ifelse(grepl("HBLL", survey_abbrev), 1 - prop_bait_hooks, NA)) |>
    dplyr::filter(!is.na(offset), is.finite(offset)) |>
    # 150 remove duplicated fishing event
    # @FIXME shouldn't need this after switch to trials version of gfdata)
    dplyr::distinct(species_common_name, fishing_event_id, .keep_all = TRUE)
}

prep_mssm_dat <- function(survey_dat) {
  # Some doorspreads were missing in 2022, use the average of other 2022 doorspreads
  mean_2022_door <- filter(survey_dat, year == 2022, doorspread_m != 0) |>
    filter(species_code == unique(survey_dat$species_code)) |>
    summarise(mean_door = mean(doorspread_m))

  survey_dat |>
    # This might be needed as gfdata gets updated, but currently grouping_desc
    # is not retrieved in data call used for 2023 report.
    #filter(grepl('WCVI Shrimp Survey Area 124|125', grouping_desc)) |>
    sdmTMB::add_utm_columns(c("longitude", "latitude"), utm_crs = 32609) |>
    mutate(doorspread_m = ifelse((year == 2022 & doorspread_m == 0), mean_2022_door[[1]], doorspread_m)) |>
    # @FIXME: area swept has been or will be added to gfdata function
    dplyr::mutate(
      area_swept1 = doorspread_m * (speed_mpm * duration_min),
      area_swept2 = tow_length_m * doorspread_m,
      area_swept = dplyr::case_when(
        !is.na(area_swept2) ~ area_swept2,
         is.na(area_swept2) ~ area_swept1)
    ) |>
    dplyr::mutate(offset = log(area_swept / 1e5),
                  catch = catch_weight,
                  present = ifelse(catch > 0, 1, 0),
                  survey_type = 'mssm',
                  year_bin = ifelse(year >= 2003, ">=2003", "<2003"), # All species ID'd to lowest taxonomic level in 2003 onward
                  fyear = as.factor(year)) |>
    dplyr::mutate(year_bin = factor(year_bin, levels = c("<2003", ">=2003"))) |>
    dplyr::filter(!is.na(offset), is.finite(offset)) |>
    dplyr::filter(!is.na(catch)) |> # small catches of small fish might have a count but no weight
    dplyr::filter(!(year %in% 1977:1978 & month == 9)) |> # Filter out the extra sampling in September
    dplyr::mutate(gear = dplyr::case_when( # add gear type change
      year < 1977 ~ 'Shrimp Balloon',
      year < 2006 & year > 1977 ~ 'NMFS',
      year > 2006 ~ 'American',
      year == 2006 & fishing_event_id %in% c(1158541, 1158542) ~ 'American',
      year == 2006 & fishing_event_id %in% c(1158559, 1158560) ~ 'NMFS',
      year == 2006 & !(fishing_event_id %in% c(1158541, 1158542, 1158559, 1158560)) ~ 'American'
    )
  )
}
