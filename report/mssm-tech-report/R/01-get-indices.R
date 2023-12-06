
# Calculate indices ------------------------------------------------------------
# catch ~ 1 model is in mssm_sc and is what is in the report

## Get model where pre and post 2003 is added as factor
# Use 3km grid
# All species ID'd to lowest taxonomic level in 2003 onward
furrr::future_walk(spp_vector, function(.sp) {
#purrr::walk(spp_vector, function(.sp) {
  spp_filename <- paste0(gfsynopsis:::clean_name(.sp), "_st-rw.rds")
    survey_dat <- readRDS(file.path(data_cache, paste0(gfsynopsis:::clean_name(.sp), ".rds")))$survey_sets |>
      filter(survey_abbrev == "MSSM WCVI")
    # Some species not included in survey_set data frame at all, so we need to skip these
    if (nrow(survey_dat) == 0) {
      out <- "No MSSM survey data"
      message(out)
      saveRDS(out, file.path(file.path(mssm_sc, '3km-grid'), spp_filename))
    } else {
      survey_dat <- prep_mssm_dat(survey_dat)

      get_stitched_index(
        form = 'catch ~ 1',
        survey_dat = survey_dat, species = .sp,
        family = sdmTMB::tweedie(),
        survey_type = "mssm", model_type = 'st-rw', cache = file.path(mssm_sc, '3km-grid'),
        cutoff = 5, silent = FALSE,
        grid_dir = NULL, check_cache = TRUE,
        survey_grid = NULL
      )
    }
})
beepr::beep()

# Use 2km grid
furrr::future_walk(spp_vector, function(.sp) {
#purrr::walk(spp_vector, function(.sp) {
  spp_filename <- paste0(gfsynopsis:::clean_name(.sp), "_st-rw.rds")
    survey_dat <- readRDS(file.path(data_cache, paste0(gfsynopsis:::clean_name(.sp), ".rds")))$survey_sets |>
      filter(survey_abbrev == "MSSM WCVI")
    # Some species not included in survey_set data frame at all, so we need to skip these
    if (nrow(survey_dat) == 0) {
      out <- "No MSSM survey data"
      message(out)
      #saveRDS(out, file.path(file.path(mssm_year_sc, 'year-bin'), spp_filename))
      saveRDS(out, file.path(file.path(mssm_sc, '2km-grid'), spp_filename))
    } else {
      survey_dat <- prep_mssm_dat(survey_dat)

      get_stitched_index(
        form = 'catch ~ 1',
        survey_dat = survey_dat, species = .sp,
        family = sdmTMB::tweedie(),
        survey_type = "mssm", model_type = 'st-rw', cache = file.path(mssm_sc, '2km-grid'),
        cutoff = 5, silent = FALSE,
        grid_dir = NULL, check_cache = TRUE,
        survey_grid = mssm_grid_2km$mssm_grid |>
          filter(year >= 2009 & year < 2022) |>
          distinct(X, Y, survey, area)
      )
    }
})
beepr::beep()


# # Fit SYN WCVI ------------------
future::plan(future::multicore, workers = 8)
furrr::future_walk(spp_vector, function(.sp) {
#purrr::walk(spp_vector, function(.sp) {
  spp_filename <- paste0(gfsynopsis:::clean_name(.sp), "_st-rw.rds")
    survey_dat <- readRDS(file.path(data_cache, paste0(gfsynopsis:::clean_name(.sp), ".rds")))$survey_sets |>
      filter(survey_abbrev == "SYN WCVI")
      survey_dat <- prep_stitch_dat(survey_dat)

      get_stitched_index(
        form = 'catch ~ 1',
        survey_dat = survey_dat, species = .sp,
        family = sdmTMB::tweedie(),
        survey_type = "SYN WCVI", model_type = 'st-rw',
        cache = file.path(syn_sc, 'syn-wcvi-grid'),
        #cache = file.path(syn_sc, 'mssm-grid-3km'),
        cutoff = 20, silent = FALSE,
        survey_grid = NULL,
        # survey_grid = gfdata::mssm_grid |>
        #   mutate(survey = 'SYN WCVI') |>
        #   filter(year >= 2009 & year < 2022) |>
        #   distinct(X, Y, survey, area),
        #grid_dir = NULL,
        grid_dir = grid_dir
        #check_cache = TRUE
      )
})
future::plan(future::sequential)
beepr::beep()