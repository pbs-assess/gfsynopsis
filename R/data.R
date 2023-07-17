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
  get_data_iphc(type = type, path = paste0(path, "/iphc"),
    compress = compress, force = force)
  get_data_iphc_hook_with_bait(path = paste0(path, "/iphc"),
    compress = compress, force = force)
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
  if (!force & file.exists(paste0(path, "/hook-with-bait.rds"))) {
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
