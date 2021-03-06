setwd(here::here())
data_cache <- here::here("report", "data-cache")
build_dir <- file.path("report", "report-rmd")
dir.create(here::here("report", "data-cache"), showWarnings = FALSE)
dir.create(here::here(build_dir), showWarnings = FALSE)
survey_cols <- c(
  RColorBrewer::brewer.pal(5L, "Set1"),
  RColorBrewer::brewer.pal(8L, "Set1")[7:8],
  "#303030", "#a8a8a8", "#a8a8a8", "#a8a8a8"
)

# Set your species here:
this_spp <- "eulachon"
this_spp_hyphens <- gsub(" ", "-", this_spp)

# Must be on PBS network:
gfdata::cache_pbs_data(
  species = this_spp,
  file_name = this_spp_hyphens,
  path = data_cache,
  unsorted_only = FALSE,
  historical_cpue = FALSE,
  survey_sets = TRUE,
  verbose = TRUE,
  compress = FALSE
)

# The last function call creates this data file:
dat <- readRDS(paste0(file.path(data_cache, this_spp), ".rds"))

# If you want to fit and plot the commercial CPUE indexes then run the following:
# (must be on PBS network; a lot of data + a bit slow)
# dat$cpue_index <- gfdata::get_cpue_index(gear = "bottom trawl", min_cpue_year = 1996)

get_max_yrs <- function(x, .grep) {
  x %>%
    dplyr::filter(grepl(.grep, survey_abbrev)) %>%
    dplyr::group_by(survey_abbrev) %>%
    dplyr::summarize(max_year = max(year)) %>%
    dplyr::pull(max_year) %>% unique() %>% sort()
}
synoptic_max_survey_years <- get_max_yrs(dat$survey_sets, "^SYN")
hbll_out_max_survey_years <- get_max_yrs(dat$survey_sets, "^HBLL OUT")

gfsynopsis::make_pages(
  dat = dat,
  dat_iphc = NULL, # NULL to skip; note these figures do not include Andy's IPHC adjustments
  spp = this_spp,
  d_geostat_index = NULL, # geostatistical index standardization; NULL to skip
  french = FALSE,
  report_lang_folder = build_dir,
  resolution = 170, # balance size with resolution
  png_format = TRUE, # vs. PDF
  save_gg_objects = TRUE, # save the ggplots to an .rds file?
  synoptic_max_survey_years = synoptic_max_survey_years,
  hbll_out_max_survey_years = hbll_out_max_survey_years,
  survey_cols = survey_cols
)

# Now go look in `report/report-rmd/figure-pages`
# and `report/report-rmd/ggplot-objects` if `save_gg_objects = TRUE`

gg_rds <- file.path(build_dir, "ggplot-objects", paste0(this_spp_hyphens, ".rds"))
if (file.exists(gg_rds)) {
  g <- readRDS(gg_rds)
  g$survey_index # for example
  names(g) # available plots
}
