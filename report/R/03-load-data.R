# Read in fresh data or load cached data if available -----------------

message("Loading data")
gfsynopsis::get_data(type = c("A", "B"), path = dc, force = FALSE)
spp <- gfsynopsis::get_spp_names() %>%
  select(
    species_common_name, species_code,
    species_science_name, spp_w_hyphens, type, itis_tsn, worms_id
  ) |>
  arrange(species_common_name)
spp <- join_worms_spp(spp)
# FIXME:
# Error in `dplyr::bind_rows()`:
# ! Argument 47 must be a data frame or a named atomic vector.

# Gather and arrange some metadata ------------------------------------

# UPDATE THE .CSV IN THIS EACH YEAR! see R/get_cosewic_data.R
cos <- get_cosewic_data()
spp <- left_join(spp, cos, by = "species_science_name")

# UPDATE THE .CSV IN THIS EACH YEAR! see R/join_refs_spp.R
spp <- join_refs_spp(spp, french = french)

# filter to the species we use for speed
if (!file.exists(file.path(dc, "survey-sets-synopsis.rds"))) {
  spp_name <- gfsynopsis::get_spp_names()$species_common_name
  dsets <- readRDS(file.path(dc, "survey-sets.rds"))
  dsets_spp <- dplyr::filter(dsets, species_common_name %in% spp_name)
  included <- unique(dsets_spp$species_common_name)
  spp_name[!spp_name %in% included]
  # check last years synopsis report to make sure these are fine
  # all good in 2026
  saveRDS(dsets_spp, file = file.path(dc, "survey-sets-synopsis.rds"), compress = FALSE)
}

# as of 2026 (i.e.g, Norm is retired :( ), we need to calculate the
# design-based indices here on the fly based on the survey set data

# set_data <- readRDS(file.path(dc, "survey-sets-synopsis.rds"))
# spp_name <- gfsynopsis::get_spp_names()$species_common_name

set_data <- readRDS("~/src/gfdata/data-raw/pcod-set-data.rds")
spp_name <- set_data$species_common_name[1]
tictoc::tic()
indexes <- purrr::map(spp_name[1], \(sp) {
  cat(sp, "...\n")
  this_dat <- dplyr::filter(set_data, species_common_name == sp)
  ssids <- sort(unique(stats::na.omit(this_dat$survey_series_id)))
  purrr::map_dfr(ssids, \(ssid) {
    .dat <- dplyr::filter(this_dat, ssid == ssid)
    cat(" ", ssid, "...\n")
    out <- gfdata::get_design_index(sp, ssid = ssid, reps = 1000L, data = this_dat)
    out$species_common_name <- sp
    out$survey_series_id <- ssid
    out
  })
}) |>
  dplyr::bind_rows()
tictoc::toc()
