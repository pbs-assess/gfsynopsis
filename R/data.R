#' Get all data
#'
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
      path = path, unsorted_only = FALSE, historic_cpue = FALSE,
      survey_sets = TRUE, verbose = FALSE, compress = compress)

  if (force || !file.exists(file.path(path, "cpue-index-dat.rds"))) {
    .dat <- gfplot::get_cpue_index(gear = "bottom trawl", min_cpue_year = 1996)
    saveRDS(.dat, file = file.path(path, "cpue-index-dat.rds"), compress = compress)
  }
}
