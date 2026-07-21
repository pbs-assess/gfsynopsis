# Read in fresh data or load cached data if available -----------------

if (!is_hake_server()) {
  message("Loading data")
  gfsynopsis::get_data(type = c("A", "B"), path = dc, force = FALSE)
}

# Combine 'herrings' commercial data with Pacific Herring so that it is included in
# the Pacific Herring pages. The cache is updated here because the species
# data lists are read again in 06-build-figure-pages.R.
pacific_herring_file <- file.path(dc, "pacific-herring.rds")
herrings_file <- file.path(dc, "herrings.rds")
if (file.exists(pacific_herring_file) && file.exists(herrings_file)) {
  pacific_herring <- readRDS(pacific_herring_file)
  herrings <- readRDS(herrings_file)

  for (element in c("catch", "cpue_spatial", "cpue_spatial_ll")) {
    herrings_element <- herrings[[element]]
    if (is.null(herrings_element) || !nrow(herrings_element)) next

    herrings_element$species_common_name <- "pacific herring"
    herrings_element$species_scientific_name <- "clupea pallasii"
    if ("species_code" %in% names(herrings_element)) {
      herrings_element$species_code <- "096"
    }

    # distinct() makes this safe to run again if the cache has already been
    # combined by an earlier run.
    pacific_herring[[element]] <- dplyr::bind_rows(
      pacific_herring[[element]], herrings_element
    ) |>
      dplyr::distinct()
  }

  saveRDS(pacific_herring, pacific_herring_file)
}

excluded_report_spp <- "herrings"
spp <- gfsynopsis::get_spp_names() %>%
  select(
    species_common_name, species_code,
    species_science_name, spp_w_hyphens, type, itis_tsn, worms_id
  ) |>
  filter(!species_common_name %in% excluded_report_spp) |>
  arrange(species_common_name)
spp <- join_worms_spp(spp)

# Gather and arrange some metadata ------------------------------------

# UPDATE THE .CSV IN THIS EACH YEAR! see R/get_cosewic_data.R
cos <- get_cosewic_data()
spp <- left_join(spp, cos, by = "species_science_name")

# UPDATE THE .CSV IN THIS EACH YEAR! see R/join_refs_spp.R
spp <- join_refs_spp(spp, french = french)

# split out the survey set data into individual species .rds
# files to match historical formats and for loading speed
# because the full df is large

set_data <- readRDS(file.path(dc, "survey-sets.rds"))

if (!is_hake_server()) { # @Question - why check for hake server here? Is this just temporary to prevent it from resplitting since this has already been done?
  survey_set_dir <- file.path(dc, "survey-sets")
  dir.create(survey_set_dir, showWarnings = FALSE, recursive = TRUE)

  survey_set_spp <- split(set_data, set_data$species_common_name)
  survey_set_spp <- survey_set_spp[!names(survey_set_spp) %in% excluded_report_spp]
  survey_set_names <- setNames(spp$spp_w_hyphens, spp$species_common_name)
  missing_spp <- setdiff(names(survey_set_spp), names(survey_set_names))
  if (length(missing_spp)) {
    stop(
      "Species in survey set data missing from get_spp_names(): ",
      paste(missing_spp, collapse = ", ")
    )
  }

  for (sp in names(survey_set_spp)) {
    out_file <- file.path(survey_set_dir, paste0(survey_set_names[sp], ".rds"))
    if (file.exists(out_file)) next
    saveRDS(
      gfdata:::convert_to_old_sets(survey_set_spp[[sp]]),
      file = out_file,
      compress = "zstd"
    )
  }
}

# Check that design indices generated in gfdata are up to date
design_max_year <- max(gfdata::design_indexes$year, na.rm = TRUE)
survey_max_year <- max(set_data$year, na.rm = TRUE)
if (design_max_year < survey_max_year) {
  warning("gfdata::design_indexes only goes to ", design_max_year,
    " but survey-sets goes to ", survey_max_year,
    ". Re-run data-raw/design-indexes.R in gfdata and reinstall.", call. = FALSE)
}
