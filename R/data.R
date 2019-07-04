#' Get all data (includes some IPHC data that will get over-written
#'  in Survey biomass indices section of make-pages.R)
#' @param type A or B
#' @param path Path
#' @param compress TRUE or FALSE to compress .rds
#' @param force Should data be downloaded even if already cached data exists?
#' @export
get_data <- function(type = c("A", "B"), path = ".",
  compress = FALSE, force = FALSE) {
  dir.create(path, showWarnings = FALSE)
  .d <- get_spp_names()
  .d <- .d[.d$type %in% type, , drop = FALSE]
  already_exists <- gsub("\\.rds", "", list.files(path))
  if (!force)
    .d <- filter(.d, !spp_w_hyphens %in% already_exists)
  if (nrow(.d) > 0L)
    gfplot::cache_pbs_data(species = .d$species_code,
      file_name = .d$spp_w_hyphens,
      path = path, unsorted_only = FALSE, historical_cpue = FALSE,
      survey_sets = TRUE, verbose = FALSE, compress = compress)
  if (force || !file.exists(file.path(path, "cpue-index-dat.rds"))) {
    .dat <- gfplot::get_cpue_index(gear = "bottom trawl", min_cpue_year = 1996)
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

