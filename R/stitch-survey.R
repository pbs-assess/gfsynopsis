# Data cleaning/prep -----------------------------------------------------------
prep_stitch_dat <- function(species_dat) {
  species_dat |>
    sdmTMB::add_utm_columns(c("longitude", "latitude"), utm_crs = 32609) |>
    dplyr::mutate(
      area_swept1 = doorspread_m * (speed_mpm * duration_min),
      area_swept2 = tow_length_m * doorspread_m,
      area_swept = ifelse(!is.na(area_swept2), area_swept2, area_swept1)
    ) |>
    dplyr::mutate(trawl_offset = log(area_swept / 1e5)) |> # Value used for offset
    dplyr::mutate(hook_offset = log(hook_count)) |> # Value used for offset
    dplyr::mutate(
      catch = ifelse(grepl("SYN", survey_abbrev), catch_weight, catch_count),
      offset = ifelse(grepl("SYN", survey_abbrev), trawl_offset, hook_offset)
    ) |>
    dplyr::mutate(present = ifelse(catch > 0, 1, 0)) |>
    dplyr::mutate(survey_type = dplyr::case_when(
      grepl("SYN", survey_abbrev) ~ "synoptic",
      grepl("HBLL OUT", survey_abbrev) ~ "hbll_outside",
      grepl("HBLL INS", survey_abbrev) ~ "hbll_inside"
    )) |>
    dplyr::mutate(species_common_name = gsub("rougheye/blackspotted",
      "rougheye-blackspotted", species_common_name)) |>
    dplyr::filter(!is.na(offset))
}

get_stitch_lu <- function(species_dat, species, survey_type) {
  stopifnot(survey_type %in% c("synoptic", "hbll_outside", "hbll_inside"))
  species_dat |>
    dplyr::filter(species_common_name %in% {{ species }}, survey_type %in% {{ survey_type }}) |>
    dplyr::group_by(species_common_name, survey_type, survey_abbrev, year) |>
    dplyr::add_count(name = "n_sets") |>
    dplyr::add_tally(present, name = "n_pos") |>
    dplyr::distinct(species_common_name, year, survey_type, survey_abbrev, n_pos, n_sets) |>
    dplyr::group_by(species_common_name, survey_type, survey_abbrev) |>
    dplyr::summarise(
      mean_n_pos = mean(n_pos), mean_n_sets = mean(n_sets),
      prop_pos = mean_n_pos / mean_n_sets
    ) |>
    dplyr::mutate_at(c("mean_n_pos", "mean_n_sets"), round, 0) |>
    dplyr::mutate_at("prop_pos", round, 2) |>
    dplyr::mutate(include_in_stitch = ifelse(prop_pos <= 0.05, 0, 1)) |>
    dplyr::ungroup() |>
    dplyr::arrange(survey_type, species_common_name)
}

# Prepare grids ----------------------------------------------------------------
prep_stitch_grids <- function() {
  grid_dir <- here::here("data-outputs", "grids")
  if (!file.exists(grid_dir)) dir.create(grid_dir)

  synoptic_grid_file <- here::here(grid_dir, "synoptic_grid.rds")
  hbll_out_grid_file <- here::here(grid_dir, "hbll_out_grid.rds")
  hbll_ins_grid_file <- here::here(grid_dir, "hbll_ins_grid.rds")

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

  # This inside grid is corrected for area overlapping with land
  # From https://github.com/Blue-Matter/quillback-rockfish/blob/master/data-generated/hbll-inside-grid.rds
  # QUESTION: Is there source code available for generating this grid?
  # Should it be included in gfplot?
  if (!file.exists(hbll_ins_grid_file)) {
    hbll_ins_grid <-
      readRDS(here::here("data", "hbll-inside-grid.rds")) |>
      sdmTMB::add_utm_columns(c("longitude", "latitude"), utm_crs = 32609) |>
      dplyr::select(survey, X, Y, depth, area)
    saveRDS(hbll_ins_grid, hbll_ins_grid_file)
  }
}

# Utility functions ------------------------------------------------------------
choose_survey_grid <- function(survey) {
  switch(survey,
    synoptic = readRDS(here::here("data-outputs", "grids", "synoptic_grid.rds")),
    hbll_out_grid = readRDS(here::here("data-outputs", "grids", "hbll_out_grid.rds")),
    hbll_ins_grid = readRDS(here::here("data-outputs", "grids", "hbll_ins_grid.rds")),
    stop("Invalid `survey` value")
  )
}

make_grid <- function(.x, years) {
  years <- sort(unique(years))
  .nd <- do.call(
    "rbind",
    replicate(length(years), .x, simplify = FALSE)
  )
  .nd$year <- rep(years, each = nrow(.x))
  .nd
}

check_cache <- function(cache, filename) {
  filecheck <- grep(filename, list.files(cache))
  if (length(filecheck) >= 1) {
    return(filename)
  }
}

# ------------------------------------------------------------------------------

get_stitched_index <- function(
    dat, species = "arrowtooth flounder",
    survey_type = c("synoptic", "hbll_outside", "hbll_inside"),
    mesh = NULL, cutoff = 20, family = sdmTMB::tweedie(), offset = "offset", silent = TRUE,
    ctrl = sdmTMB::sdmTMBcontrol(nlminb_loops = 1L, newton_loops = 1L),
    cache = here::here("report", "stitch-cache"), parallel = FALSE,
    overwrite_cache = FALSE) {
  cache <- file.path(cache, survey_type)
  if (!file.exists(cache)) dir.create(cache)

  out <- list()

  # Skip model fitting if fewer than 2 regions have >= 0.05 positive sets
  stitch_lu <- get_stitch_lu(dat, species, survey_type)
  stitch_regions <- stitch_lu |>
    dplyr::filter(species_common_name %in% {{ species }} & survey_type %in% {{ survey_type }} &
      include_in_stitch == 1)

  mean_num_sets <- sum(stitch_regions$mean_n_sets)
  mean_num_pos_sets <- sum(stitch_regions$mean_n_pos)

  stitch_regions <- stitch_regions[["survey_abbrev"]]

  if (length(stitch_regions) < 2) {
    cat("\n\tInsufficient data to stitch regions for: ", survey_type, species, "\n")
    out[[1]] <- "insufficient data to stitch regions"
    saveRDS(out, here::here(cache, paste0(species, "_no-stitch.rds")))
    return(out)
  }

  dat <- dat |>
    dplyr::filter(species_common_name == species & survey_type == survey_type &
      survey_abbrev %in% stitch_regions)

  dat <- droplevels(dat) # drop extra factor levels before running models

  cat("\n\tStitching index for:", species)
  cat("\n\t\t- For regions: ", paste(stitch_regions, collapse = ", "))

  if (is.null(mesh)) {
    message(cat("\t\t- No mesh provided, making mesh with cutoff:", cutoff))
    mesh <- sdmTMB::make_mesh(dat, c("X", "Y"), cutoff = 20)
  }

  missing_years <- sdmTMB:::find_missing_time(dat$year)

  if (length(missing_years) < 1L) {
    message(cat("\t\t- No missing time to be filled in."))
    missing_years <- NULL
  } else {
    message(cat("\t\t- Filling in extra_time with:", missing_years))
  }

  if (!is.null(offset)) offset <- dat[[offset]]

  message(cat("\n\tFitting st RW, time_varying RW for:", species))
  fit <- try(
    sdmTMB::sdmTMB(
      catch ~ 0,
      family = family,
      time_varying = ~1, time_varying_type = "rw",
      time = "year", spatiotemporal = "rw", spatial = "on",
      data = dat,
      mesh = mesh,
      offset = offset,
      silent = silent,
      extra_time = missing_years,
      control = ctrl
    )
  )

  if (!all(unlist(sdmTMB::sanity(fit, gradient_thresh = 0.01)))) {
    message(cat("\n\tFailed sanity check, skipping predictions and index"))
    out[[1]] <- "Failed sanity check"
    saveRDS(out, here::here(cache, paste0(species, "_failed-sanity.rds")))
    return(out)
  }

  if (inherits(fit, "sdmTMB")) {
    message("\n\t Getting predictions")
    # Prepare newdata for getting predictions
    year_range_seq <- min(dat$year):max(dat$year)
    grid <- choose_survey_grid(survey_type)
    newdata <- make_grid(.x = grid, years = year_range_seq) |>
      dplyr::filter(
        survey %in% fit$data$survey_abbrev,
        year %in% fit$data$year
      ) |>
      droplevels()

    pred <- predict(fit, newdata, return_tmb_object = TRUE)
    pred$newdata_input <- newdata # Remove if this is unnecessary

    saveRDS(pred, here::here(cache, paste0(species, "_pred.rds")))
  }

  if (length(pred) > 1) {
    message("\n\t Calculating index")
    out[[1]] <- sdmTMB::get_index(pred, bias_correct = TRUE, area = pred$newdata$area)
  }

  saveRDS(out, here::here(cache, paste0(species, "_index.rds")))
  out
}

# Look at what is being excluded/included --------------------------------------
# Useful for looking at what gets stitched
# spp_dat <- readRDS(file = here::here("data-outputs", "survey-sets.rds")) |>
#   dplyr::tibble() |>
#   prep_stitch_dat()
# positive_sets <- get_stitch_lu(spp_dat,
#   species = unique(spp_dat$species_common_name),
#   survey_type = "synoptic"
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
