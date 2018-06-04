#' Get all data
#'
#' @param type A or B
#' @param path Path
#' @param compress TRUE or FALSE to compress .rds
#' @export
get_data <- function(type = "A", path = ".", compress = FALSE) {
  dir.create(path, showWarnings = FALSE)
  .d <- get_spp_names()
  .d <- dplyr::filter(.d, .data$type %in% type)
  gfplot::cache_pbs_data(species = .d$species_common_name,
    path = path, unsorted_only = FALSE, historic_cpue = FALSE,
    survey_sets = TRUE, verbose = FALSE, compress = compress)
  .dat <- gfplot::get_cpue_index(gear = "bottom trawl", min_cpue_year = 1996)
  saveRDS(.dat, file = file.path(path, "cpue-index-dat.rds"), compress = compress)
}
