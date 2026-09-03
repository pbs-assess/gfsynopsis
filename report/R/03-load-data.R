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
  pacific_herring_updated <- FALSE

  for (element in c("catch", "cpue_spatial", "cpue_spatial_ll")) {
    herrings_element <- herrings[[element]]
    if (is.null(herrings_element) || !nrow(herrings_element)) next

    herrings_element$species_common_name <- "pacific herring"
    herrings_element$species_scientific_name <- "clupea pallasii"
    if ("species_code" %in% names(herrings_element)) {
      herrings_element$species_code <- "096"
    }

    # Avoid rewriting the shared cache when it already contains all of the
    # herring rows; this file is read by every figure-building worker.
    missing_herring <- dplyr::anti_join(
      herrings_element, pacific_herring[[element]], by = names(herrings_element)
    )

    if (nrow(missing_herring)) {
      # distinct() makes this safe to run again if the cache has already been
      # partially combined by an earlier run.
      pacific_herring[[element]] <- dplyr::bind_rows(
        pacific_herring[[element]], herrings_element
      ) |>
        dplyr::distinct()
      pacific_herring_updated <- TRUE
    }
  }

  if (pacific_herring_updated) {
    saveRDS(pacific_herring, pacific_herring_file)
  }
}

excluded_report_spp <- "herrings"
spp <- gfsynopsis::get_spp_names() %>%
  select(
    species_common_name, species_code,
    species_science_name, spp_w_hyphens, type, itis_tsn, worms_id
  ) |>
  filter(!species_common_name %in% excluded_report_spp) |>
  arrange(species_common_name)
spp <- join_worms_spp(spp, check_cache = TRUE) # set to FALSE if new species added!

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

na_grouping_code <- set_data |>
  dplyr::filter(species_common_name == "petrale sole") |>  # pick any one!
  dplyr::filter(is.na(grouping_code))
nrow(na_grouping_code)

table(na_grouping_code$survey_abbrev, na_grouping_code$year)

na_grouping_code <- set_data |>
  dplyr::filter(species_common_name == "petrale sole") |>  # pick any one!
  dplyr::filter(!survey_abbrev %in% c("MSSM WCVI", "HS MSA")) |>
  dplyr::filter(is.na(grouping_code)| !grepl("^SYN", survey_abbrev) | survey_series_id == 16L | !is.na(grouping_code_updated))

table(na_grouping_code$survey_abbrev, na_grouping_code$year)


# Match the legacy survey-set selection: retain events with an original
# grouping code, and for SYN events other than WCHG require a survey-specific
# updated grouping match as well. WCHG is exempt because its 2006 legacy
# FISHING_EVENT_GROUPING codes differ from the original grouping codes. The
# updated-grouping requirement still removes events such as SYN WCVI 1720127,
# which has no matching FISHING_EVENT_GROUPING / SURVEY_GROUPING pair.
# set_data <- set_data |>
#   dplyr::filter(!is.na(grouping_code)) |>
#   dplyr::filter(
#     !grepl("^SYN", survey_abbrev) |
#       survey_series_id == 16L |
#       !is.na(grouping_code_updated)
#   )

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
