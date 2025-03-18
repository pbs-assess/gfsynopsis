# This file generates all the main synopsis figures in `report/figure-pages`.
# It must be run before the report can be rendered.

# Things to update each year:
# UPDATE THE .CSV IN R/join_refs_spp.R
# UPDATE THE .CSV IN R/get_cosewic_data.R

# Settings ------------------------------------------------------------

french <- FALSE
# shapefile <- NULL

ext <- "png" # pdf vs. png figs; png for CSAS and smaller file sizes
example_spp <- c("petrale sole", "pacific cod") # a species used as an example in the Res Doc
optimize_png <- TRUE # optimize the figures at the end? Need optipng installed.
parallel_processing <- TRUE
cores <- floor(future::availableCores() / 2)

dc <- here::here("report", "data-cache-2025-03")
shapefile <- sf::st_read(here::here("report/spatial-filtering/shape-files/haida/"))

# Setup ---------------------------------------------------------------

setwd(here::here())

if (french) {
  options(french = TRUE)
  options(OutDec = ",")
}
is_rstudio <- !is.na(Sys.getenv("RSTUDIO", unset = NA))
is_unix <- .Platform$OS.type == "unix"

if (french) {
  build_dir <- "report/tech-report-fr"
} else {
  build_dir <- "report/tech-report"
}

library(here)
library(dplyr)
library(gfplot)
devtools::load_all(".") # gfsynopsis; weird hexbin errors if not load_all()ed
library(rosettafish)
library(future)
wd <- getwd()
if (!grepl("gfsynopsis", wd)) stop("Working directory wrong? Should be this repo main folder.")

is_hake_server <- function() {
  future::availableCores() > 50L
}
if (is_hake_server()) {
  cores <- 30L
  RhpcBLASctl::blas_set_num_threads(1) # default currently is all/80!
  RhpcBLASctl::omp_set_num_threads(1)
}

# Set up parallel processing or sequential ----------------------------

options(future.globals.maxSize = 800 * 1024^2) # 800 mb
if (parallel_processing) {
  if (!is_rstudio && is_unix) {
    future::plan(multicore, workers = cores)
  } else {
    future::plan(multisession, workers = cores)
  }
} else {
  future::plan(sequential)
}

# Read in fresh data or load cached data if available -----------------

message("Loading data")
gfsynopsis::get_data(type = c("A", "B"), path = dc, force = FALSE)
d_cpue <- readRDS(file.path(dc, "cpue-index-dat.rds"))
spp <- gfsynopsis::get_spp_names() %>%
  select(
    species_common_name, species_code,
    species_science_name, spp_w_hyphens, type, itis_tsn, worms_id
  ) |>
  arrange(species_common_name)
spp <- join_itis_spp(spp)

# Geostatistical model fits (a bit slow) ------------------------------
# fi <- here("report", "geostat-cache", "geostat-index-estimates.rds")
# if (!file.exists(fi)) source(here("report/make-geostat.R"))
# dat_geostat_index <- readRDS(fi)

# Gather and arrange some metadata ------------------------------------

# UPDATE THE .CSV IN THIS EACH YEAR! see R/get_cosewic_data.R
cos <- get_cosewic_data()
spp <- left_join(spp, cos, by = "species_science_name")

# UPDATE THE .CSV IN THIS EACH YEAR! see R/join_refs_spp.R
spp <- join_refs_spp(spp, french = french)

# Cache stitched indices ----------------------------------------------

message("Cache stitched indexes")
dc_stitch <- file.path(dc, "stitch-data") # Data used
stitch_cache <- file.path("report", "stitch-cache") # Stitched outputs
dir.create(stitch_cache, showWarnings = FALSE, recursive = TRUE)

# Stitch inputs
model_type <- "st-rw"
spp_vector <- spp$species_common_name[order(spp$species_common_name)]
# randomize to avoid clumps of non-fitting in parallel:
set.seed(92729)
spp_vector <- sample(spp_vector, length(spp_vector))
# Synoptic/HBLL
bait_counts <- readRDS(file.path(dc, "bait-counts.rds"))
grid_dir <- file.path(dc, "grids")
# IPHC
# Use 2017 grid for predictions (can be changed)
iphc_grid <- gfdata::iphc_sets |>
  filter(year == 2017) |>
  rename(lon = "longitude", lat = "latitude") |>
  select(year, station, lon, lat) |>
  sdmTMB::add_utm_columns(ll_names = c("lon", "lat"))

# 2023 specific code ------
# For 2023, let's use the below chunk because we have not updated the grids.
prep_stitch_grids(
  grid_dir = grid_dir,
  hbll_ins_grid_input = file.path(dc_stitch, "hbll-inside-grid_water-area.rds")
)
# -----

# Stitch surveys if not cached
if (FALSE) { # slow to check!
  source(here::here("report", "run-stitching.R"))
}

# get all survey years to convert NAs to 0s:
dog <- readRDS(paste0(dc, "/north-pacific-spiny-dogfish.rds"))$survey_index
all_survey_years <- dplyr::select(dog, survey_abbrev, year) %>%
  dplyr::distinct()

if (!is_hake_server()) {
  # these are complex, do outside first:
  source(here::here("report", "plot-indices.R"))
  # make_index_panel("north-pacific-spiny-dogfish")
  # make_index_panel("basking-shark")
  # make_index_panel("pacific-cod", all_survey_years = all_survey_years)
  # make_index_panel("arrowtooth-flounder", all_survey_years = all_survey_years)
  # # make_index_panel("big-skate")
  # make_index_panel("longnose-skate", all_survey_years = all_survey_years)
  # make_index_panel("whitebarred-prickleback", all_survey_years = all_survey_years)
  # make_index_panel("rougheye-blackspotted-rockfish-complex", all_survey_years = all_survey_years)
  index_ggplots <- furrr::future_map(spp$spp_w_hyphens, make_index_panel, all_survey_years = all_survey_years)
}

# CPUE model fits -----------------------------------------------------

if (parallel_processing && is_hake_server()) future::plan(future::multisession, workers = 10L)
if (parallel_processing && !is_hake_server()) future::plan(future::multisession, workers = 1L)
source(here::here("report/cpue-sdmTMB.R"))
future::plan(sequential)

if (is_hake_server()) stop()

# Make figure pages ---------------------------------------------------

message("Make figure pages")
fig_check <- file.path(
  build_dir, "figure-pages",
  gfsynopsis:::clean_name(spp$species_common_name)
)
fig_check1 <- paste0(fig_check, "-1.", ext)
fig_check2 <- paste0(fig_check, "-2.", ext)
missing <- !file.exists(fig_check1) | !file.exists(fig_check2)
# missing <- rep(TRUE, length(missing))
for (i in which(!missing)) {
  cat(
    crayon::green(clisymbols::symbol$tick),
    "Figure pages for", spp$species_common_name[i], "already exist\n"
  )
}
missing_spp <- spp$species_common_name[missing]
to_build <- which(missing)

rlang::inform("Building")
rlang::inform(paste(spp$species_common_name)[to_build])

# Trash compiled objects for safety:
unlink("vb_gfplot.*")
unlink("lw_gfplot.*")

if (exists("ii")) {
  to_build <- to_build[to_build %in% ii]
}

purrr::walk(to_build, function(i) {
  tryCatch(
    {
      cli::cli_inform("------------------------------------")
      cli::cli_progress_step(paste0("Building figure pages for ", spp$species_common_name[i]), spinner = TRUE)

      dat <- readRDS(file.path(dc, paste0(spp$spp_w_hyphens[i], ".rds")))
      dat_iphc <- gfdata::load_iphc_dat(species = spp$species_common_name[i]) |>
        rename(lat = "latitude", lon = "longitude")
      hbll_bait_counts <- readRDS(file.path(dc, "bait-counts.rds"))
      dat$cpue_index <- d_cpue

      length_ticks <- readr::read_csv(here::here("report/length-axis-ticks.csv"),
        show_col_types = FALSE
      ) |> as.data.frame()

      # FIXME!
      if (spp$species_common_name[i] == "kelp greenling") {
        dat$survey_samples <-
          dplyr::filter(dat$survey_samples, !(weight > 0.1 & length < 10))
      }

      gfsynopsis::make_pages(
        dat = dat,
        dat_iphc = dat_iphc,
        spp = spp$species_common_name[i],
        all_survey_years = all_survey_years,
        d_geostat_index = NULL, # dat_geostat_index, # spatiotemporal model fits
        include_map_square = FALSE, # to check the map aspect ratio
        french = french,
        report_lang_folder = build_dir,
        resolution = 150, # balance size with resolution
        png_format = if (ext == "png") TRUE else FALSE,
        parallel = FALSE, # for CPUE fits; need a lot of memory if true!
        save_gg_objects = spp$species_common_name[i] %in% example_spp,
        synoptic_max_survey_years = list("SYN WCHG" = 2022, "SYN HS" = 2023, "SYN WCVI" = 2022, "SYN QCS" = 2023),
        hbll_out_max_survey_years = list("HBLL OUT N" = 2023, "HBLL OUT S" = 2022),
        final_year_comm = 2023,
        final_year_surv = 2023,
        length_ticks = length_ticks[length_ticks$species_code == spp$species_code[i], ],
        stitch_model_type = "st-rw",
        grid_dir = file.path(data_cache, "grids"),
        hbll_bait_counts = hbll_bait_counts,
        # index_ggplot = ggplot() + geom_point(),
        index_ggplot = index_ggplots[[i]],
        spatiotemporal_cpue = TRUE,
        raw_cpue = NULL,
        shapefile = shapefile
      )
    },
    error = function(e) stop("Error")
  )
})

# Extract saved ggplot objects for example plots ----------------------
# These objects are too big to cache in an .Rmd file otherwise.
if (!exists("ii")) {
  if (!french) {
    g_alt <- readRDS(paste0(build_dir, "/ggplot-objects/pacific-cod.rds"))
    saveRDS(g_alt$cpue_spatial, file = paste0(build_dir, "/ggplot-objects/pacific-cod-cpue-spatial.rds"))
    saveRDS(g_alt$cpue_spatial_ll, file = paste0(build_dir, "/ggplot-objects/pacific-cod-cpue-spatial-ll.rds"))
  }
  if (french) {
    g_alt <- readRDS(paste0(build_dir, "/ggplot-objects/pacific-cod.rds"))
    saveRDS(g_alt$cpue_spatial, file = paste0(build_dir, "/ggplot-objects/pacific-cod-cpue-spatial.rds"))
    saveRDS(g_alt$cpue_spatial_ll, file = paste0(build_dir, "ggplot-objects/pacific-cod-cpue-spatial-ll.rds"))
  }
}

# Generate plot-pages.Rmd ---------------------------------------------
# This is the guts of where the .tex / .Rmd figure page code gets made
temp <- lapply(spp$species_common_name, \(x) generate_plotpages_Rmd(x = x, spp = spp))
temp <- lapply(temp, function(x) paste(x, collapse = "\n"))
temp <- paste(temp, collapse = "\n")
temp <- c("<!-- This page has been automatically generated: do not edit by hand -->\n", temp)
con <- file(file.path(build_dir, "plot-pages.Rmd"), encoding = "UTF-8")
writeLines(temp, con = con)

# Optimize png files for TeX ------------------------------------------
# pdflatex is much faster if png files are optimized

if (optimize_png) {
  cores <- parallel::detectCores()
  files_per_core <- ceiling(length(spp$species_common_name) * 2 / cores)
  setwd(file.path(build_dir, "figure-pages"))
  # if (!gfplot:::is_windows() && parallel_processing) {
  if (!gfplot:::is_windows()) {
    system(paste0(
      "find -X . -name '*.png' -print0 | xargs -0 -n ",
      files_per_core, " -P ", cores, " /opt/homebrew/bin/optipng -strip all"
    ))
    setwd(here())
  }
}
