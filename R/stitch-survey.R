#' Prepare survey set data for index stitching
#'
#' @param spp_dat A dataframe from [gfplot::get_survey_sets()]
#'
#' @returns A dataframe the same length as `spp_dat`

#' @export
prep_stitch_dat <- function(spp_dat) {
  # Add baited hook counts to spp_dat for LL surveys
  # @FIXME this chunk is probably unecessary if all surveys are in spp_dat
  ll <- grepl("HBLL", unique(spp_dat$survey_abbrev))
  if (length(ll > 0)) {
    bait_count <- readRDS(here::here("data-outputs", "bait_counts.rds"))
    spp_dat <- left_join(spp_dat, bait_count,
      by = c("year", "fishing_event_id", "survey_series_id" = "ssid")
    ) |>
      mutate(count_bait_only = replace(count_bait_only, which(count_bait_only == 0), 1)) %>%
      mutate(prop_bait_hooks = count_bait_only / hook_count) %>%
      mutate(hook_adjust_factor = -log(prop_bait_hooks) / (1 - prop_bait_hooks))
  }
  out <-
    spp_dat |>
    sdmTMB::add_utm_columns(c("longitude", "latitude"), utm_crs = 32609) |>
    dplyr::mutate(
      area_swept1 = doorspread_m * (speed_mpm * duration_min),
      area_swept2 = tow_length_m * doorspread_m,
      area_swept = dplyr::case_when(
        grepl("SYN", survey_abbrev) & !is.na(area_swept2) ~ area_swept2,
        grepl("SYN", survey_abbrev) & is.na(area_swept2) ~ area_swept1,
        grepl("HBLL", survey_abbrev) ~ hook_count * 0.0024384 * 0.009144 * 1000
      )
    ) |>
    dplyr::mutate(offset = dplyr::case_when(
      grepl("SYN", survey_abbrev) ~ log(area_swept / 1e5),
      grepl("HBLL", survey_abbrev) ~ log(area_swept / hook_adjust_factor)
    )) |>
    dplyr::mutate(catch = ifelse(grepl("SYN", survey_abbrev), catch_weight, catch_count)) |>
    dplyr::mutate(present = ifelse(catch > 0, 1, 0)) |>
    dplyr::mutate(survey_type = dplyr::case_when(
      grepl("SYN", survey_abbrev) ~ "synoptic",
      grepl("HBLL OUT", survey_abbrev) ~ "hbll_outside",
      grepl("HBLL INS", survey_abbrev) ~ "hbll_inside"
    )) |>
    dplyr::mutate(species_common_name = gsub(
      "rougheye/blackspotted",
      "rougheye-blackspotted", species_common_name
    )) |>
    dplyr::filter(!is.na(offset))
  out
}


#' Get table of positive sets for each region and survey type
#'
#' @param spp_dat A dataframe from [gfsynopsis::prep_stitch_dat()]
#' @param species A string specifying the `species_common_name`
#' @param survey_type A string matching one of: "synoptic", "hbll_outside", "hbll_inside"
#'
#' @returns A dataframe
#' @export
get_stitch_lu <- function(spp_dat, species, survey_type) {
  stopifnot(survey_type %in% c("synoptic", "hbll_outside", "hbll_inside"))
  spp_dat |>
    dplyr::filter(species_common_name %in% {{ species }}, survey_type %in% {{ survey_type }}) |>
    dplyr::group_by(species_common_name, survey_type, survey_abbrev, year) |>
    dplyr::add_count(name = "n_sets") |>
    dplyr::add_tally(present, name = "n_pos") |>
    dplyr::distinct(species_common_name, year, survey_type, survey_abbrev, n_pos, n_sets) |>
    dplyr::group_by(species_common_name, survey_type, survey_abbrev) |>
    dplyr::summarise(
      mean_n_pos = mean(n_pos), mean_n_sets = mean(n_sets),
      prop_pos = mean_n_pos / mean_n_sets,
      .groups = "drop"
    ) |>
    dplyr::mutate_at(c("mean_n_pos", "mean_n_sets"), round, 0) |>
    dplyr::mutate_at("prop_pos", round, 2) |>
    dplyr::mutate(include_in_stitch = ifelse(prop_pos <= 0.05, 0, 1)) |>
    dplyr::arrange(survey_type, species_common_name)
}

# Prepare grids ----------------------------------------------------------------
#' Write grids used for stitching index
#'
#' @description
#' Write grid objects used for stitching index. These are written to a
#' <data-outputs/grids> directory.
#' Note: Might not be needed if clean grids are added to `gfdata`
#'
#' @return
#' @export
#'
prep_stitch_grids <- function() {
  grid_dir <- here::here("data-outputs", "grids")
  if (!file.exists(grid_dir)) dir.create(grid_dir)

  synoptic_grid_file <- here::here(grid_dir, "synoptic_grid.rds")
  hbll_out_grid_file <- here::here(grid_dir, "hbll_out_grid.rds")
  hbll_ins_grid_file <- here::here(grid_dir, "hbll_ins_grid.rds")
  # @TODO will update when grids are added to gfdata
  if (!file.exists(synoptic_grid_file)) {
    syn_grid <-
      gfplot::synoptic_grid |>
      dplyr::rename(area = "cell_area") |>
      dplyr::select(-survey_series_name, -utm_zone, -survey_domain_year)
    saveRDS(syn_grid, synoptic_grid_file)
  }

  if (!file.exists(hbll_out_grid_file)) {
    hbll_n_grid <- gfplot::hbll_n_grid$grid |>
      dplyr::mutate(survey = "HBLL OUT N")
    hbll_s_grid <- gfplot::hbll_s_grid$grid |>
      dplyr::mutate(survey = "HBLL OUT S")

    hbll_out_grid <- dplyr::bind_rows(hbll_n_grid, hbll_s_grid) |>
      dplyr::rename(longitude = "X", latitude = "Y") |>
      sdmTMB::add_utm_columns(c("longitude", "latitude"), utm_crs = 32609) |>
      dplyr::mutate(area = 4) |>
      dplyr::select(survey, X, Y, depth, area)
    saveRDS(hbll_out_grid, hbll_out_grid_file)
  }

  if (!file.exists(hbll_ins_grid_file)) {
    hbll_ins_grid <-
      readRDS(here::here("data", "hbll-inside-grid.rds")) |>
      sdmTMB::add_utm_columns(c("longitude", "latitude"), utm_crs = 32609) |>
      dplyr::select(survey, X, Y, depth, area)
    saveRDS(hbll_ins_grid, hbll_ins_grid_file)
  }
}

# Utility functions ------------------------------------------------------------
#' Choose the survey grid matching the survey type
#'
#' @param survey_type A string matching one of: "synoptic", "hbll_outside", "hbll_inside"
#'
#' @return A dataframe containing a survey grid from [gfsynopsis::prep_stitch_grids()]
#' @export
#'
choose_survey_grid <- function(survey_type) {
  switch(survey_type,
    synoptic = readRDS(here::here("data-outputs", "grids", "synoptic_grid.rds")),
    hbll_outside = readRDS(here::here("data-outputs", "grids", "hbll_out_grid.rds")),
    hbll_inside = readRDS(here::here("data-outputs", "grids", "hbll_ins_grid.rds")),
    stop("Invalid `survey_type` value")
  )
}

#' Make prediction grid over years and survey grid
#'
#' @param survey_grid A dataframe from [gfsynopsis::prep_stitch_grids()]
#' @param years A numeric vector of years
#'
#' @return A dataframe with as many rows as `nrow(survey_grid) * length(years)`
#'
make_grid <- function(survey_grid, years) {
  years <- sort(unique(years))
  .nd <- do.call(
    "rbind",
    replicate(length(years), survey_grid, simplify = FALSE)
  )
  .nd$year <- rep(years, each = nrow(survey_grid))
  .nd
}

#' Format stitched index for plotting
#'
#' @description
#' Formats output of [gfsynopsis::get_stitched_index()] to match output of
#' [gfplot::tidy_survey_index()] so that synopsis plots can be made using
#' [gfplot::plot_survey_index()]
#'
#' @param stitched_index A dataframe from [gfsynopsis::get_stiched_index()]
#'
#' @return
#' @export
#'
tidy_stitched_index <- function(stitched_index) {
  stitched_index |>
    dplyr::rename(
      survey_abbrev = "stitch_regions", biomass = "est",
      lowerci = "lwr", upperci = "upr"
    )
}

# ------------------------------------------------------------------------------

#' Get stitched index across survey regions in synoptic trawl and HBLL surveys
#'
#' @param dat A dataframe from [gfsynopsis::prep_stitch_dat()].
#' @param species A string specifying the `species_common_name`.
#' @param survey_type A string matching one of: "synoptic" (the default), "hbll_outside", "hbll_inside".
#' @param model_type A string matching one of: "st-rw" (the default), "st-rw_tv-rw".
#' @param mesh Optional mesh object created using [sdmTMB::make_mesh()].
#' @param cutoff If `mesh = NULL`, mesh cutoff for [sdmTMB::make_mesh()].
#' @param family The family and link for [sdmTMB::sdmTMB()].
#' @param offset A string naming the offset column in `dat` used in [sdmTMB::sdmTMB()]
#' @param silent A boolean. Silent or include optimization details.
#' @param ctrl Optimization control options via [sdmTMB::sdmTMBcontrol()].
#' @param cache A string specifying file path to cache location.
#'
#' @returns Either a string or dataframe:
#' * `insufficient data to stitch regions` if the number of positive sets is too low to stitch
#' * `Failed sanity check` if the model failed to converge
#' * A dataframe containing the stitched index
#' @export
#'
get_stitched_index <- function(
    dat, species = "arrowtooth flounder",
    survey_type = "synoptic",
    model_type = "st-rw",
    mesh = NULL, cutoff = 20, family = sdmTMB::tweedie(), offset = "offset", silent = TRUE,
    ctrl = sdmTMB::sdmTMBcontrol(nlminb_loops = 1L, newton_loops = 1L),
    cache = here::here("report", "stitch-cache")) {
  cache <- file.path(cache, survey_type)
  if (!file.exists(cache)) dir.create(cache)

  # Skip model fitting if fewer than 2 regions have >= 0.05 positive sets
  stitch_lu <- get_stitch_lu(dat, species, survey_type)

  if (survey_type != "synoptic") {
    stitch_regions_df <- stitch_lu |>
      dplyr::filter(species_common_name %in% {{ species }} & survey_type %in% {{ survey_type }} &
        include_in_stitch == 1)
  } else {
    wchg_pos_0.05 <- stitch_lu[[which(stitch_lu$survey_abbrev == "SYN WCHG"), "prop_pos"]] > 0.05
    other_regions_0.05 <- sum(stitch_lu[which(stitch_lu$survey_abbrev != "SYN WCHG"), "prop_pos"]) > 0.05

    # If WCHG and other regions are all > 0.05 stitch use all regions
    if (wchg_pos_0.05 & other_regions_0.05) {
      stitch_regions_df <- stitch_lu
    }

    # If WCHG < 0.05 but other regions > 0.05, stitch other regions
    if (!wchg_pos_0.05 & other_regions_0.05) {
      stitch_regions_df <- stitch_lu[which(stitch_lu$survey_abbrev != "SYN WCHG"), ]
    }
    # If no region > 0.05 do not stitch
    if (!wchg_pos_0.05 & !other_regions_0.05) {
      stitch_regions_df <- NULL
    }
  }

  stitch_regions <- stitch_regions_df[["survey_abbrev"]]

  if (length(stitch_regions) < 2) {
    cat("\n\tInsufficient data to stitch regions for: ", survey_type, species, "\n")
    out <- "insufficient data to stitch regions"
    saveRDS(out, here::here(cache, paste0(species, "_no-stitch.rds")))
    return(out)
  }

  # Only calculate positive sets if stitching
  mean_num_sets <- sum(stitch_regions_df$mean_n_sets)
  mean_num_pos_sets <- sum(stitch_regions_df$mean_n_pos)

  dat <- dat |>
    dplyr::filter(species_common_name == species & survey_type == survey_type &
      survey_abbrev %in% stitch_regions)

  dat <- droplevels(dat) # drop extra factor levels before running models

  cat("\n\tStitching index for:", species)
  cat("\n\t\t- For regions: ", paste(stitch_regions, collapse = ", "))

  if (is.null(mesh)) {
    cat("\n\t\t- No mesh provided, making mesh with cutoff:", cutoff)
    mesh <- sdmTMB::make_mesh(dat, c("X", "Y"), cutoff = 20)
  }

  missing_years <- sdmTMB:::find_missing_time(dat$year)

  if (length(missing_years) < 1L) {
    cat("\n\t\t- No missing time to be filled in.")
    missing_years <- NULL
  } else {
    cat("\n\t\t- Filling in extra_time with:", missing_years)
  }

  if (!is.null(offset)) offset <- dat[[offset]]

  cat("\n\tFitting:", model_type, " ", species, "\n")

  fit <- switch(model_type,
    `st-rw` = try(
      sdmTMB::sdmTMB(
        formula = catch ~ 1, family = family,
        time = "year", spatiotemporal = "rw", spatial = "on",
        data = dat, mesh = mesh, offset = offset, extra_time = missing_years,
        silent = silent, control = ctrl
      )
    ),
    `st-rw_tv-rw` = try(
      sdmTMB::sdmTMB(
        formula = catch ~ 0, family = family,
        time_varying = ~1, time_varying_type = "rw",
        time = "year", spatiotemporal = "rw", spatial = "on",
        data = dat, mesh = mesh, offset = offset, extra_time = missing_years,
        silent = silent, control = ctrl
      )
    ),
    stop("Invalid `model_type` value")
  )

  if (!all(unlist(sdmTMB::sanity(fit, gradient_thresh = 0.01)))) {
    cat("\n\tFailed sanity check for:", model_type, " ", species, "\n")
    out <- "Failed sanity check"
    sanity_filename <- here::here(cache, paste0(species, "_", model_type, "_failed-sanity.rds"))
    saveRDS(out, sanity_filename)
    return(out)
  }

  if (inherits(fit, "sdmTMB")) {
    cat("\n\tGetting predictions\n")
    # Prepare newdata for getting predictions
    year_range_seq <- min(dat$year):max(dat$year)
    grid <- choose_survey_grid(survey_type)
    newdata <- gfsynopsis:::make_grid(survey_grid = grid, years = year_range_seq) |>
      dplyr::filter(
        survey %in% fit$data$survey_abbrev,
        year %in% fit$data$year
      ) |>
      droplevels()

    pred <- predict(fit, newdata, return_tmb_object = TRUE)
    pred$newdata_input <- newdata # Remove if this is unnecessary

    pred_filename <- here::here(cache, paste0(species, "_", model_type, "_pred.rds"))
    cat("\n\tSaving:", pred_filename, "\n")
    saveRDS(pred, pred_filename)
  }

  if (length(pred) > 1) {
    cat("\n\tCalculating index\n")
    index <- sdmTMB::get_index(pred, bias_correct = TRUE, area = pred$newdata$area)
    index$mean_cv <- mean(sqrt(exp(index$se^2) - 1))
    index$num_sets <- mean_num_sets
    index$num_pos_sets <- mean_num_pos_sets
    index$survey_type <- survey_type
    index$stitch_regions <- paste(stitch_regions, collapse = ", ")
    out <- index
  }
  index_filename <- here::here(cache, paste0(species, "_", model_type, "_index.rds"))
  cat("\n\tSaving:", index_filename, "\n")
  saveRDS(out, index_filename)
  out
}

## Stitch indices ---------------------------------------------------------------
# spp_dat <- readRDS(file = here::here("data-outputs", "survey-sets.rds")) |>
#   dplyr::tibble()

# cache <- here::here("report", "stitch-cache", 'synoptic')
# #cache <- here::here("report", "stitch-cache", 'hbll_outside')
# cached_spp <- NULL
# #cached_spp <- gsub('(_.*)', '', list.files(cache))
# missing_spp <- cached_spp[!(unique(spp_dat$species_common_name) %in% cached_spp)]
# spp_dat |>
#   prep_stitch_dat() |> dplyr::distinct(survey_type)
# get_stitch_lu(spp_dat = _, species = unique(spp_dat$species_common_name), survey_type == 'hbll_inside')

# spp_df_list <- spp_dat |>
#   dplyr::filter(!(species_common_name %in% cached_spp)) |>
#   dplyr::group_split(species_common_name) |>
#   purrr::map(prep_stitch_dat)

## Stitch SYN -------
# spp_df_list |>
#   purrr::map(\(df) get_stitched_index(dat = df, species = unique(df$species_common_name),
#     survey_type = 'synoptic', model_type = 'st-rw'))

## Stitch HBLL OUT -------
# spp_df_list |>
#   purrr::map(\(df) get_stitched_index(dat = df, species = unique(df$species_common_name),
#     survey_type = 'hbll_outside', model_type = 'st-rw'))

## Look at what is being excluded/included --------------------------------------
## Useful for looking at what gets stitched
# spp_dat <- readRDS(file = here::here("data-outputs", "survey-sets.rds")) |>
#   dplyr::tibble() |>
#   prep_stitch_dat()

# positive_sets <- get_stitch_lu(spp_dat,
#   species = unique(spp_dat$species_common_name),
#   survey_type = "hbll_inside"
# )

# stitch_ft <- positive_sets |>
#   dplyr::group_by(species_common_name, survey_type) |>
#   dplyr::summarise(stitch_tally = sum(include_in_stitch)) |>
#   dplyr::mutate(to_stitch = ifelse(stitch_tally < 2, 0, 1)) |>
#   dplyr::right_join(positive_sets) |>
#   dplyr::arrange(survey_type, species_common_name)

# cl <- officer::fp_border(color = "black", width = 3)

# break_position <- function(x) {
#   z <- data.table::rleidv(x)
#   c(z[-length(z)] != z[-1], FALSE)
# }

# inclusion_table <- stitch_ft |>
#   flextable::flextable() |>
#   flextable::merge_v(x = _, j = "species_common_name") |>
#   flextable::hline(i = ~ break_position(species_common_name)) |>
#   flextable::fix_border_issues() |>
#   flextable::bg(x = _, i = ~ prop_pos <= 0.01, j = 5, bg = "red") |>
#   flextable::bg(x = _, i = ~ (prop_pos > 0.01 & prop_pos <= 0.03), j = 5, bg = "orange") |>
#   flextable::bg(x = _, i = ~ (prop_pos > 0.03 & prop_pos <= 0.05), j = 5, bg = "yellow")
# inclusion_table
# ------------------------------------------------------------------------------
