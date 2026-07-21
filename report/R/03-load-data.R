# Read in fresh data or load cached data if available -----------------

if (!is_hake_server()) {
  message("Loading data")
  gfsynopsis::get_data(type = c("A", "B"), path = dc, force = FALSE)
}
spp <- gfsynopsis::get_spp_names() %>%
  select(
    species_common_name, species_code,
    species_science_name, spp_w_hyphens, type, itis_tsn, worms_id
  ) |>
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
