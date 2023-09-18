#' Get all data (includes some IPHC data that will get over-written
#'  in Survey biomass indices section of make-pages.R)
#' @param type A or B
#' @param path Path
#' @param compress TRUE or FALSE to compress .rds
#' @param force Should data be downloaded even if already cached data exists?
#' @param sleep System sleep in seconds between each species
#'   to be nice to the server.
#' @export
get_data <- function(type = c("A", "B"), path = ".",
  compress = FALSE, force = FALSE, sleep = 20) {
  dir.create(path, showWarnings = FALSE)
  .d <- get_spp_names()
  .d <- .d[.d$type %in% type, , drop = FALSE]
  already_exists <- gsub("\\.rds", "", list.files(path))
  if (!force)
    .d <- filter(.d, !spp_w_hyphens %in% already_exists)
  if (nrow(.d) > 0L) {
    for (i in seq_along(.d$species_code)) {

    tryCatch({gfdata::cache_pbs_data(species = .d$species_code[i],
        file_name = .d$spp_w_hyphens[i],
        path = path, unsorted_only = FALSE, historical_cpue = FALSE,
        survey_sets = TRUE, verbose = FALSE, compress = compress)}, error = function(e) e)

      Sys.sleep(sleep)
    }
  }
  if (force || !file.exists(file.path(path, "cpue-index-dat.rds"))) {
    .dat <- gfdata::get_cpue_index(gear = "bottom trawl", min_cpue_year = 1996)
    saveRDS(.dat, file = file.path(path, "cpue-index-dat.rds"), compress = compress)
  }
  get_data_iphc(type = type, path = file.path(path, "iphc"),
    compress = compress, force = force)
  get_data_iphc_hook_with_bait(path = file.path(path, "iphc"),
    compress = compress, force = force)
  get_iphc_hook_data(path = file.path(path, "iphc"))
}

#' Get the IPHC data for all years, should get merged into get_data at some point
#'  but before gfsynopsis meeting we don't want to re-get all the non-IPHC data
#' @param type A or B
#' @param path Path
#' @param compress TRUE or FALSE to compress .rds
#' @param force Should data be downloaded even if already cached data exists?
#' @export
#'
get_data_iphc <- function(type = c("A", "B"), path = ".",
  compress = FALSE, force = FALSE) {
  dir.create(path, showWarnings = FALSE)
  .d <- get_spp_names()
  .d <- .d[.d$type %in% type, , drop = FALSE]
  already_exists <- gsub("\\.rds", "", list.files(path))
  if (!force)
    .d <- filter(.d, !spp_w_hyphens %in% already_exists)
  if (nrow(.d) > 0L)
    gfiphc::cache_pbs_data_iphc(species = .d$species_common_name,
      file_name = .d$spp_w_hyphens,
      path = path, compress = compress)
}

#' Get the IPHC data for all years for hooks with bait (for hook competition
#'  calculations.
#' @param path Path
#' @param compress TRUE or FALSE to compress .rds
#' @param force Should data be downloaded even if already cached data exists?
#' @export
#'
get_data_iphc_hook_with_bait <- function(path = ".",
  compress = FALSE, force = FALSE) {
  dir.create(path, showWarnings = FALSE)
  if (!force & file.exists(file.path(path, "hook-with-bait.rds"))) {
     return() } else {
  gfiphc::cache_pbs_data_iphc(species = "hook with bait",
    file_name = "hook-with-bait",
    path = path, compress = compress)
  }
}

#' Download longline baited hook counts
#'
#' @description
#' Get the hook data for the HBLL outside and inside surveys from GFBIO.
#'
#' @param path Path where the 'bait-counts.rds' file is saved to
#' @param species Species code or common name to query
#' @param ssid Survey series id defaulting to the longline surveys
#'
#' @returns RDS object with a stored dataframe ssid, year, fishing_event_id, and baited hook count
#' @export
#'
get_ll_bait_counts <- function(path = ".", species = 442, ssid = c(22, 36, 39, 40)) {
  ll_hook_data <- gfdata::get_ll_hook_data(species = species, ssid = ssid)
  # Use bait counts only because other columns have questionable data quality
  bait_counts <- ll_hook_data |>
    dplyr::select(ssid, year, fishing_event_id, count_bait_only)
  saveRDS(bait_counts, file.path(path, 'bait-counts.rds'))
}


#' Get IPHC hook counts for all survey years
#'
#' @param path Path where the 'iphc-hook-counts.rds' file is saved to
#' @param species Species common name to query, defaults to 'pacific halibut'
#'   because to date this species does not affect the output.
#'
#' @returns An RDS object containing a dataframe with the following columns:
#'   - year: The year of the data.
#'   - station: The station identifier.
#'   - lat: The latitude coordinates.
#'   - lon: The longitude coordinates.
#'   - baited_hooks: The number of baited hooks.
#'   - setID: The set identifier (used by the IPHC, may not match GFBio output)
#'   - obsHooksPerSet: The observed hooks per set.
#'   - effSkateIPHC: The effective skate IPHC (International Pacific Halibut Commission).
#'   - iphcUsabilityCode: The IPHC usability code.
#'   - iphcUsabilityDesc: The description of the IPHC usability code.
#'
#' @export
#'
get_iphc_hook_data <- function(path = ".", species = 'pacific halibut') {
  sp_file <- paste0(gfsynopsis:::clean_name(species), '.rds')
  sp_dat <- readRDS(file.path(path, sp_file))$set_counts |>
    mutate(species = species)
  # Get hook_bait counts matching GFBio species counts: 1995:2022

  hook_bait <- readRDS(file.path(path, 'hook-with-bait.rds'))$set_counts |>
    mutate(baited_hooks = ifelse(!is.na(N_it), N_it, N_it20)) |>
    select(year, station, lat, lon, baited_hooks)
  sp_dat <- left_join(sp_dat, hook_bait)

  # Need total observed hook counts to calculate prop_removed
# ---------------------------------------------------------
# Get set information for years 2003:2012; 2014:2019; 2022
  if (!file.exists(file.path(path, 'iphc-set-info.rds'))) {
    message("File: <iphc_sets_info.rds> not found, querying GFBio using
        gfiphc::get_iphc_sets_info() - requires VPN connection\n")
    iphc_set_info <- get_iphc_sets_info() # requires VPN connection
    saveRDS(iphc_set_info, 'iphc-set-info.rds')
  }

  iphc_set_info <- readRDS(file.path(path, 'iphc-set-info.rds')) |>
    rename(lon = 'long') |>
    filter(year != 2022) # these hook counts are wrong from GFBio

  # Hook counts for 1996 - 2002
  set_1996_2002 <-
    gfiphc::data1996to2002 |>
      mutate(species = tolower(spNameIPHC), station = as.character(station)) |>
      rename(N_it = 'catchCount', obsHooksPerSet = hooksObserved) |>
      select(year, station, lat, lon, obsHooksPerSet, usable)

  # Hook counts for 1995 and 2013
  # Need to sum observations of all 'species' observed to get hook counts
  set_1995 <- left_join(
    gfiphc::setData1995,
    gfiphc::countData1995 |> group_by(station) |> summarise(obsHooksPerSet = sum(specCount))
  ) |>
    mutate(year = 1995)

  set_2013 <- left_join(
    gfiphc::setData2013,
    gfiphc::countData2013 |> group_by(station) |> summarise(obsHooksPerSet = sum(specCount))
  )

  # Hook counts for 2020, 2021, 2022
  set_2020_2021_2022 <- bind_rows(gfiphc::setData2020, gfiphc::setData2021, gfiphc::setData2022) |>
    rename(obsHooksPerSet = "hooksObs") |>
    select(year, station, lat, lon, obsHooksPerSet, usable, standard)

  set_info <-
    bind_rows(iphc_set_info, set_1995, set_1996_2002, set_2013, set_2020_2021_2022) %>%
    select(year, setID, station, obsHooksPerSet, effSkateIPHC, iphcUsabilityCode, iphcUsabilityDesc) %>%
    distinct(year, setID, station, .keep_all = TRUE)

  #unique(sp_dat$year)[!unique(sp_dat$year) %in% unique(c(set_info$year))]

  # Combine set information with species count data
  sp_with_hooks <- left_join(sp_dat, set_info) |>
   select(-species, -(E_it:C_it20))

  # Resolve the many-to-many by using lat/lon as additional key values
  year2019_stations <-
    left_join(
      filter(sp_dat, (year == 2019 & station %in% c("2099", "2107"))),
      filter(set_info, (year == 2019 & station %in% c("2099", "2107")))
    ) |>
    select(all_of(colnames(sp_with_hooks)))

  iphc_hook_out <-
    sp_with_hooks |>
    # simplest to remove unresolved many-to-many and add proper values in
    filter(!(year == 2019 & station %in% c("2099", "2107"))) |>
    bind_rows(year2019_stations) |>
    arrange(year, station) |>
    select(-usable, -standard)

  saveRDS(iphc_hook_out, file.path(path, 'iphc-hook-counts.rds'))
}
