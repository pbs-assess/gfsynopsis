# A one-off script from 2019-09-17 to just extract the necessary data
# and substituted into the previous extraction for speed.

library(here)
library(dplyr)
# library(gfplot)
library(gfiphc)
library(gfsynopsis)
# library(rosettafish)
# library(foreach)
# library(future)
library(gfdata)

get_data_20190917 <- function(type = c("A", "B"), path = ".",
  compress = FALSE, force = FALSE, sleep = 0.5) {
  dir.create(path, showWarnings = FALSE)
  .d <- get_spp_names()
  .d <- .d[.d$type %in% type, , drop = FALSE]

  for (i in seq_along(.d$species_code)) {
    cache_pbs_data_20190917(species = .d$species_code[i],
      file_name = .d$spp_w_hyphens[i],
      path = path, unsorted_only = FALSE, historical_cpue = FALSE,
      survey_sets = TRUE, verbose = FALSE, compress = compress)
    Sys.sleep(sleep)
  }

  # if (force || !file.exists(file.path(path, "cpue-index73-dat.rds"))) {
  #   .dat <- gfdata::get_cpue_index(gear = "bottom trawl", min_cpue_year = 1996)
  #   saveRDS(.dat, file = file.path(path, "cpue-index-dat.rds"), compress = compress)
  # }

  # message("Extracting IPHC data... ")
  #
  # get_data_iphc(type = type, path = paste0(path, "/iphc"),
  #   compress = compress, force = force)
  # get_data_iphc_hook_with_bait(path = paste0(path, "/iphc"),
  #   compress = compress, force = force)
}

cache_pbs_data_20190917 <- function(species, file_name = NULL, path = ".",
  compress = FALSE, unsorted_only = TRUE, historical_cpue = FALSE,
  survey_sets = FALSE, verbose = TRUE) {
  dir.create(path, showWarnings = FALSE)

  for (sp_i in seq_along(species)) {
    this_sp <- species[[sp_i]]

    if (is.null(file_name)) {
      this_sp_clean <- gsub("/", "-", gsub(" ", "-", this_sp))
    } else {
      this_sp_clean <- gsub("/", "-", gsub(" ", "-", file_name[[sp_i]]))
    }
    message("Extracting data for ", gfdata:::codes2common(this_sp))

    survey_sets <- gfdata::get_survey_sets(this_sp,
      join_sample_ids = TRUE,
      verbose = TRUE, sleep = 0.01
    )
    # survey_samples <- gfdata::get_survey_samples(this_sp) # need all!
    # survey_index <- gfdata::get_survey_index(this_sp) # might as well get all just in case!

    message("Reading old data for ", gfdata:::codes2common(this_sp))
    d_old <- readRDS(paste0("/Volumes/Extreme-SSD/gfs/report/data-cache/", this_sp_clean, ".rds"))

    # d_old$survey_sets <- dplyr::filter(d_old$survey_samples, survey_series_id != 3)
    # d_old$survey_sets <- dplyr::bind_rows(d_old$survey_sets, survey_sets)

    # d_old$survey_samples <- survey_samples
    d_old$survey_sets <- survey_sets
    # d_old$survey_index <- survey_index

    saveRDS(d_old,
      file = paste0(file.path(path, this_sp_clean), ".rds"),
      compress = compress
    )
  }
  # message("All data extracted and saved in the folder `", path, "`.")
}

dc <- here("report", "data-cache")
get_data_20190917(type = c("A", "B"), path = dc, force = FALSE)
