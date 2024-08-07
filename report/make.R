setwd(here::here())

french <- FALSE

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
# This file generates all the main synopsis figures in `report/figure-pages`.
# It must be run before the report can be rendered.
library(here)
library(dplyr)
library(gfplot)
#devtools::load_all("../gfplot")
# library(gfsynopsis)
devtools::load_all(".")
library(rosettafish)
# library(foreach)
library(future)
# setwd(here())
wd <- getwd()
if (!grepl("gfsynopsis", wd)) stop("Working directory wrong? Should be this repo main folder.")

# ------------------------------------------------------------------------------
# Settings:
ext <- "png" # pdf vs. png figs; png for CSAS and smaller file sizes
example_spp <- c("petrale sole", "pacific cod") # a species used as an example in the Res Doc
optimize_png <- TRUE # optimize the figures at the end? Need optipng installed.
parallel_processing <- TRUE
cores <- floor(future::availableCores() / 2)

is_hake_server <- function() {
  future::availableCores() > 50L
}
if (is_hake_server()) {
  cores <- 30L
  RhpcBLASctl::blas_set_num_threads(1) # default currently is all/80!
  RhpcBLASctl::omp_set_num_threads(1)
}

# ------------------------------------------------------------------------------
# Set up parallel processing or sequential
options(future.globals.maxSize = 800 * 1024 ^ 2) # 800 mb
if (parallel_processing) {
  if (!is_rstudio && is_unix) {
    future::plan(multicore, workers = cores)
  } else {
    future::plan(multisession, workers = cores)
  }
} else {
  future::plan(sequential)
}

# Read in fresh data or load cached data if available: ------------------------
message('Loading data')
dc <- here("report", "data-cache-2024-05")
gfsynopsis::get_data(type = c("A", "B"), path = dc, force = FALSE)
d_cpue <- readRDS(file.path(dc, "cpue-index-dat.rds"))
spp <- gfsynopsis::get_spp_names() %>%
  select(
    species_common_name, species_code,
    species_science_name, spp_w_hyphens, type, itis_tsn, worms_id
  ) |> arrange(species_common_name)

# Geostatistical model fits: (a bit slow) --------------------------------------
# fi <- here("report", "geostat-cache", "geostat-index-estimates.rds")
# if (!file.exists(fi)) source(here("report/make-geostat.R"))
# dat_geostat_index <- readRDS(fi)

# ------------------------------------------------------------------------------
# Gather and arrange some metadata
if (!file.exists(here("report", "itis.rds"))) {
  cls <- taxize::classification(spp$itis_tsn[!is.na(spp$itis_tsn)], db = 'itis')
  saveRDS(cls, file = here("report", "itis.rds"))
} else {
  cls <- readRDS(here("report", "itis.rds"))
}
cls <- plyr::ldply(cls) %>%
  rename(itis_tsn = .id) %>%
  filter(rank %in% c('order', 'family')) %>%
  reshape2::dcast(itis_tsn ~ rank, value.var = 'name')
spp <- left_join(spp, mutate(cls, itis_tsn = as.integer(itis_tsn)),
  by = "itis_tsn")

spp[grep("tope", spp$species_common_name),"worms_id"] <- "105820"
spp[spp$worms_id == "unknown", ]

# Missing from ITIS:
spp$order[spp$species_common_name == "deacon rockfish"] <-
  spp$order[spp$species_common_name == "vermilion rockfish"]
spp$family[spp$species_common_name == "deacon rockfish"] <-
  spp$family[spp$species_common_name == "vermilion rockfish"]

# downloaded from:
# https://species-registry.canada.ca/index-en.html#/species?ranges=Pacific%20Ocean&taxonomyId=Fishes%20%28marine%29&sortBy=commonNameSort&sortDirection=asc&pageSize=10
# they seem to keep changing URL query syntax, so: Range - Pacific Ocean; Taxonomic group - Fishes (marine)
# on 2024-06-18
cos <- readr::read_csv(here::here("report/COSEWIC-species.csv"), show_col_types = FALSE)
#cos <- readr::read_csv(here::here("report/COSEWIC-species-old.csv"), show_col_types = FALSE)
cos <- dplyr::filter(cos, !grepl("Salmon", `COSEWIC common name`))
cos <- dplyr::filter(cos, !grepl("Trout", `COSEWIC common name`))
# cos <- dplyr::filter(cos, !grepl("Pixie Poacher", `COSEWIC common name`)) # pixie poacher seems to have been removed since 2024
cos <- rename(cos, species_science_name = `Scientific name`,
  cosewic_status = `COSEWIC status`, sara_status = `Schedule status`,
  cosewic_pop = `COSEWIC population`)
# For species with different cosewic statuses for different populations, combine the text:
cos_multi_pop <- cos |>
  group_by(species_science_name) |>
  filter(n_distinct(cosewic_status) > 1) |>
  group_by(cosewic_status, .add = TRUE) |>
  summarise(cosewic_pop_status =
      paste0(unique(cosewic_status), " (", paste(unique(cosewic_pop), collapse = ", "), ")"),
    .groups = 'drop_last') |>
  summarise(cosewic_pop_status = paste0(unique(cosewic_pop_status), collapse = ", "), .groups = 'drop')
# duplicate of inside YE:
cos <- select(cos, species_science_name, cosewic_status, sara_status)
cos <- mutate(cos, species_science_name = ifelse(grepl("type I", species_science_name), "Sebastes aleutianus/melanostictus complex", species_science_name))
cos <- left_join(cos, cos_multi_pop, by = "species_science_name") %>%
  mutate(cosewic_status = ifelse(!is.na(cosewic_pop_status), cosewic_pop_status, cosewic_status)) %>%
  select(-cosewic_pop_status)
cos$species_science_name <- tolower(cos$species_science_name)
cos <- distinct(cos) # to remove duplicates (e.g., Eulachon, YE, RE/BS rockfish)
spp <- left_join(spp, cos, by = "species_science_name")

# Also check for new Species at Risk/COSEWIC documents ---
# See: https://species-registry.canada.ca/index-en.html#/documents?documentTypeId=18,14,11,9&ranges=18&sortBy=documentTypeSort&sortDirection=asc&pageSize=10
# Can download and sort by publication date

# ------------------------------------------------------------------------------
# Parse metadata that will be used at the top of each species page:
refs <- readr::read_csv(here("report/spp-refs.csv"), show_col_types = FALSE)
spp <- left_join(spp, refs, by = "species_common_name")
spp$type_other_ref[!is.na(spp$type_other_ref)] <-
  rosettafish::en2fr(spp$type_other_ref[!is.na(spp$type_other_ref)], french, allow_missing = TRUE)

spp$species_science_name <- gfplot:::firstup(spp$species_science_name)
spp$species_science_name <- gsub(" complex", "", spp$species_science_name)
spp$resdoc <- gsub(", ", ", @", spp$resdoc)
spp$resdoc <- ifelse(is.na(spp$resdoc), "", paste0("@", spp$resdoc, ""))
spp$sar <- gsub(", ", ", @", spp$sar)
spp$sar <- ifelse(is.na(spp$sar), "", paste0("@", spp$sar, ""))
spp$other_ref <- gsub(", ", ", @", spp$other_ref)
spp$other_ref_cite <- ifelse(is.na(spp$other_ref), "",
  paste0(spp$type_other_ref, ": @", spp$other_ref, "")
)
spp <- arrange(spp, species_code)
spp <- spp %>%
  mutate(species_common_name = gsub(
    "rougheye/blackspotted rockfish complex",
    "Rougheye/Blackspotted Rockfish Complex", species_common_name
  )) %>%
  mutate(species_common_name = gsub(
    "c-o sole",
    "C-O Sole", species_common_name
  ))
if (isFALSE(french)) {
  spp$species_french_name <-
    tolower(rosettafish::en2fr(gfsynopsis:::first_cap(spp$species_common_name)))
  spp$species_common_name <- tolower(spp$species_common_name)
} else { # French
  spp$species_french_name <- rosettafish::en2fr(spp$species_common_name)
  spp$species_french_name <- purrr::map_chr(spp$species_french_name, gfsynopsis:::cap)
  spp$species_common_name <- tolower(spp$species_common_name)
}

# ------------------------------------------------------------------------------
# Cache stitched index
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
grid_dir <- file.path(dc, 'grids')
# IPHC
# Use 2017 grid for predictions (can be changed)
iphc_grid <- gfdata::iphc_sets |>
  filter(year == 2017) |>
  rename(lon = "longitude", lat = "latitude") |>
  select(year, station, lon, lat) |>
  sdmTMB::add_utm_columns(ll_names = c('lon', 'lat'))

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

# ------------------------------------------------------------------------------
# CPUE model fits

if (parallel_processing) future::plan(future::multisession, workers = 4L)
message("Fit CPUE models")
cpue_cache <- file.path("report", "cpue-cache")
dir.create(cpue_cache, showWarnings = FALSE)
xx <- spp$species_common_name
xx <- sample(xx, length(xx), replace = FALSE)
furrr::future_walk(xx, function(.sp) {
# purrr::walk(xx, function(.sp) {
  spp_file <- gfsynopsis:::clean_name(.sp)
  cpue_cache_spp <- paste0(file.path(cpue_cache, spp_file), ".rds")
  if (!file.exists(cpue_cache_spp)) {
    cat(.sp, "\n")
    cpue_index <- gfsynopsis::fit_cpue_indices(
      dat = d_cpue,
      species = .sp,
      save_model = .sp %in% example_spp,
      parallel = FALSE
    )
    saveRDS(cpue_index, file = cpue_cache_spp, compress = FALSE)
  }
})
future::plan(sequential)

# if (parallel_processing && is_hake_server()) future::plan(future::multisession, workers = 10L)
# if (parallel_processing && !is_hake_server()) future::plan(future::multisession, workers = 2L)
# source(here::here("report/cpue-sdmTMB.R"))
# message("Fit sdmTMB CPUE models")
cpue_cache <- file.path("report", "cpue-sdmTMB-cache")
raw_cpue_cache <- file.path("report", "raw-cpue-cache")
dir.create(cpue_cache, showWarnings = FALSE)
dir.create(raw_cpue_cache, showWarnings = FALSE)
# xx <- spp$species_common_name
# set.seed(123)
# xx <- sample(xx, length(xx), replace = FALSE)
# # xx[!xx %in% tolower(unique(d_cpue$species_common_name))]
# furrr::future_walk(xx, function(.sp) {
# # purrr::walk(xx, \(.sp) {
#   spp_file <- gfsynopsis:::clean_name(.sp)
#   cpue_cache_spp <- paste0(file.path(cpue_cache, spp_file), ".rds")
#   raw_cpue_cache_spp <-
#   regions <- list(
#     c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG"),
#     c("SYN HS", "SYN WCHG"),
#     c("SYN QCS"),
#     c("SYN WCVI")
#   )
#   if (!file.exists(cpue_cache_spp)) {
#     cat(.sp, "\n")
#     cpue_index_l <- lapply(regions, \(r) {
#       .r <- gsub(" ", "-", paste(r, collapse = "-"))
#       .f <- paste0(file.path(raw_cpue_cache, paste0(spp_file, "-", .r)), ".rds")
#       ret <- fit_sdmTMB_cpue(
#         cpue_data_file = here::here("report/data-cache-2024-05/cpue-index-dat.rds"),
#         raw_cpue_caching_file = here::here(.f),
#         survey_grids = r,
#         final_year = 2023,
#         species = .sp
#       )
#       gc()
#       ret
#     })
#     cpue_index <- do.call(rbind, cpue_index_l)
#     saveRDS(cpue_index, file = cpue_cache_spp, compress = FALSE)
#   }
# })
future::plan(sequential)
f <- list.files(raw_cpue_cache, full.names = TRUE)
raw_cpue <- purrr::map_dfr(f, \(x) {
  a <- readRDS(x)
  if (!all(is.na(a[[1L]]))) a
})
raw_cpue <- filter(raw_cpue, is.finite(est_unstandardized))
raw_cpue <- filter(raw_cpue, !is.na(est_unstandardized))
# raw_cpue <- NULL

if (!is_hake_server()) {

  # ------------------------------------------------------------------------------
  # This is the guts of where the figure pages get made:
message("Make figure pages")
fig_check <- file.path(build_dir, "figure-pages",
  gfsynopsis:::clean_name(spp$species_common_name))
fig_check1 <- paste0(fig_check, "-1.", ext)
fig_check2 <- paste0(fig_check, "-2.", ext)
missing <- !file.exists(fig_check1) | !file.exists(fig_check2)
# missing <- rep(TRUE, length(missing))
for (i in which(!missing)) {
  cat(crayon::green(clisymbols::symbol$tick),
    "Figure pages for", spp$species_common_name[i], "already exist\n")
}
missing_spp <- spp$species_common_name[missing]
to_build <- which(missing)

# to_build <- to_build[seq(1, floor(length(to_build) / 2))]
# to_build <- to_build[seq(ceiling(length(to_build) / 2), length(to_build))]

rlang::inform("Building")
rlang::inform(paste(spp$species_common_name)[to_build])

# Trash compiled objects for safety:
# unlink("vb_gfplot.*")
# unlink("lw_gfplot.*")

if (exists("ii")) {
  to_build <- to_build[to_build %in% ii]
}

purrr::walk(to_build, function(i) {
  tryCatch({
    cli::cli_inform("------------------------------------")
    cli::cli_progress_step(paste0("Building figure pages for ", spp$species_common_name[i]), spinner = TRUE)

    dat <- readRDS(file.path(dc, paste0(spp$spp_w_hyphens[i], ".rds")))
    dat_iphc <- gfdata::load_iphc_dat(species = spp$species_common_name[i]) |>
      rename(lat = 'latitude', lon = 'longitude')
    hbll_bait_counts <- readRDS(file.path(dc, 'bait-counts.rds'))
    dat$cpue_index <- d_cpue

    length_ticks <- readr::read_csv(here::here("report/length-axis-ticks.csv"),
      show_col_types = FALSE) |> as.data.frame()

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
      length_ticks = length_ticks[length_ticks$species_code == spp$species_code[i],],
      stitch_model_type = 'st-rw',
      grid_dir = file.path(data_cache, 'grids'),
      hbll_bait_counts = hbll_bait_counts,
      index_ggplot = index_ggplots[[i]],
      spatiotemporal_cpue = TRUE,
      raw_cpue = raw_cpue
    )
    # }, error = function(e) warning("Error"))
  }, error = function(e) stop("Error"))
})
# beepr::beep()

# Extracts just the CPUE map plots for Pacific Cod for the examples.
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

# ------------------------------------------------------------------------------
# This is the guts of where the .tex / .Rmd figure page code gets made

# Generate `plot-pages.Rmd`:
temp <- lapply(spp$species_common_name, function(x) {
  message(x)
  spp_file <- gfsynopsis:::clean_name(x)
  if (french) {
    spp_title <- spp$species_french_name[spp$species_common_name == x]
  } else {
    spp_title <- stringr::str_to_title(x)
  }
  if (spp_title == "North Pacific Spiny Dogfish") {
    spp_title <- "Pacific Spiny Dogfish"
  }
  spp_hyphen <- spp$spp_w_hyphens[spp$species_common_name == x]
  out <- list()
  latin_name <- spp$species_science_name[spp$species_common_name == x]
  sar <- spp$sar[spp$species_common_name == x]
  resdoc <- spp$resdoc[spp$species_common_name == x]
  species_code <- spp$species_code[spp$species_common_name == x]
  other_ref <- spp$other_ref_cite[spp$species_common_name == x]
  sara_status <- spp$sara_status[spp$species_common_name == x]
  cosewic_status <- spp$cosewic_status[spp$species_common_name == x]
  cosewic_report <- spp$cosewic_status_reports[spp$species_common_name == x]
  worms_id <- spp$worms_id[spp$species_common_name == x]

  resdoc_text <- if (grepl(",", resdoc)) {
    paste0(en2fr("Last Research Document", french), "s: ")
  } else {
    paste0(en2fr("Last Research Document", french), ": ")
  }
  sar_text <- if (grepl(",", sar)) {
    paste0(en2fr("Last Science Advisory Report", french), "s: ")
  } else {
    paste0(en2fr("Last Science Advisory Report", french), ": ")
  }

  i <- 1
  out[[i]] <- "\\clearpage\n"
  i <- i + 1
  out[[i]] <- paste0("## ", spp_title, " {#sec:", spp_hyphen, "}\n")
  i <- i + 1
  out[[i]] <- paste0(
    gfsynopsis:::emph(latin_name), " (", species_code, ")", "\\\n",
    en2fr("Order", french), ": ", spp$order[spp$species_common_name == x], ", ",
    en2fr("Family", french), ": ", spp$family[spp$species_common_name == x],
    ","
  )
  i <- i + 1
  out[[i]] <- paste0(
    "[FishBase]",
    "(http://www.fishbase.org/summary/",
    gsub(" ", "-", gfplot:::firstup(latin_name)), ")"
  )
  if (species_code == "394") { # Sebastes aleutianus/melanostictus
    .names <- rougheye_split(gfplot:::firstup(latin_name))
    out[[i]] <- paste0(
      "[FishBase 1]",
      "(http://www.fishbase.org/summary/", .names[1], "),"
    )
    i <- i + 1
    out[[i]] <- paste0(
      "[FishBase 2]",
      "(http://www.fishbase.org/summary/", .names[2], ")"
    )
  }
  if (species_code == "039") { # Requiem Sharks
    out[[i]] <- paste0(
      "[FishBase]",
      "(http://www.fishbase.org/Summary/FamilySummary.php?ID=11)"
    )
  }
  if (worms_id != "unknown") {
    out[[i]] <- paste0(out[[i]], ", ")
    i <- i + 1
    out[[i]] <- paste0(
      "[WoRMS]",
      "(http://www.marinespecies.org/aphia.php?p=taxdetails&id=",
      worms_id, ")"
    )
  }
  out[[i]] <- paste0(out[[i]], "\\")
  if (resdoc != "") {
    i <- i + 1
    out[[i]] <- paste0(resdoc_text, resdoc, "\\")
  }
  if (sar != "") {
    i <- i + 1
    out[[i]] <- paste0(sar_text, sar, "\\")
  }
  i <- i + 1

  if (!is.na(other_ref)) {
    if (other_ref != "") {
      out[[i]] <- paste0(other_ref, "\\")
      if (!is.na(cosewic_status) && cosewic_status != "") {
        # out[[i]] <- paste0(out[[i]], "\\")
      }
      if (french) {
        out[[i]] <- gsub("Last joint Canada-US stock assessment:", "Dernière évaluation conjointe des stocks Canada-États-Unis :", out[[i]])
        out[[i]] <- gsub("Last Science Response:", "Réponse des sciences :", out[[i]])
        out[[i]] <- gsub("IPHC Report of Assessment and Research Activities:", "Rapport des activités d'évaluation et de recherche de l'CIFP :", out[[i]])
        out[[i]] <- gsub("Technical Report:", "Rapport technique :", out[[i]])
        out[[i]] <- gsub("Species at Risk Act Management Plan Series:", "Série de plans de gestion de la Loi sur les espèces en péril :", out[[i]])
      }
      if (species_code == "034" && french) {
        out[[i]] <- "Stratégie et plan d'action pour le rétablissement de la Loi sur les espèces en péril : @dfo2011baskingshark, @cosewic2020baskingshark"
      }
      i <- i + 1
    }
  }
  if (!is.na(cosewic_report)) {
    if (cosewic_report != "") {
      out[[i]] <- paste0(en2fr("COSEWIC Status Report", french), ": @", cosewic_report, "\\")
      i <- i + 1
    }
  }
  if (!is.na(cosewic_status)) {
    if (cosewic_status != "") {
      out[[i]] <- paste0(en2fr("COSEWIC Status", french), ": ", en2fr(cosewic_status, french))
      if (!is.na(sara_status)) {
        if (sara_status != "") {
          out[[i]] <- paste0(out[[i]], ", ", en2fr("SARA Status", french), ": ", en2fr(sara_status, french))
        }
      }
      out[[i]] <- paste0(out[[i]], "\n")
      i <- i + 1
    }
  }
  # if (species_code == "610" && french) {
  #   out[[i - 1]] <- "Document de recherche présentant une étude de cas de 3CD Rex Sole : @anderson2021mp"
  # }
  # if (species_code == "394") {
  #   if (!french) {
  #     out[[i]] <- paste0(en2fr("COSEWIC Status", french), ": ", en2fr("Special Concern", french), ", ", en2fr("SARA Status",french), ": ",  en2fr("Special Concern", french), "\n")
  #   } else {
  #     out[[i]] <- paste0(en2fr("COSEWIC Status", french), ":", en2fr("Special Concern", french), ", ", en2fr("SARA Status", french), ":", en2fr("Special Concern"), "\n")
  #   }
  #   i <- i + 1
  # }
  if (species_code == "225") {
    if (!french) {
      out[[i]] <- "Note that Pacific Hake undergoes a directed joint
      Canada-US coastwide\n survey and annual assessment, which are not
      included in this report. The most recent\n stock assessment
      should be consulted for details on stock status."
    } else {
      out[[i]] <- "Il est à noter que le merlu du Chili fait l’objet d’un relevé et d’une évaluation annuels ciblés menés conjointement par le Canada et les É.-U. à l'échelle de la côte, qui ne sont pas compris dans le présent rapport. L’évaluation la plus récente des stocks doit être consultée pour obtenir des détails sur l’état des stocks."
    }
    i <- i + 1
  }
  if (species_code == "614") {
    if (!french) {
      out[[i]] <- "Note that Pacific Halibut undergoes thorough assessment by the
      International Pacific\n Halibut Commission based on the annual
      standardized setline survey. The most\n recent stock assessment
      should be consulted for details on stock status."
    } else {
      out[[i]] <- "Il est à noter que le flétan du Pacifique fait l’objet d’une évaluation approfondie par la Commission internationale du flétan du Pacifique qui se fonde sur un relevé annuel normalisé en fonction de la ligne de référence. L’évaluation la plus récente des stocks doit être consultée pour obtenir des détails sur l’état des stocks."
    }
    i <- i + 1
  }
  if (species_code == "455") {
    if (!french) {
      out[[i]] <- "The annual sablefish trap survey is not included in this report. Commercial biological samples from a head-only sampling program that began in 2018 [@lacko2023] are not shown."
    } else {
      out[[i]] <- "Il est à noter que la morue charbonnière fait l’objet de relevés annuels au casier ciblés qui servent à l’évaluation des stocks et qui ne sont pas compris dans le présent rapport. L’évaluation la plus récente des stocks doit être consultée pour obtenir des détails sur l’état des stocks."
    }
    i <- i + 1
  }
  out[[i]] <- "\\begin{figure}[b!]"
  i <- i + 1
  out[[i]] <- paste0(
    "\\includegraphics[width=6.4in]{figure-pages/",
    spp_file, "-1.", ext, "}"
  )
  i <- i + 1
  out[[i]] <- "\\end{figure}"
  i <- i + 1
  out[[i]] <- "\\clearpage"
  i <- i + 1
  out[[i]] <- "\\begin{figure}[b!]"
  i <- i + 1
  out[[i]] <- paste0(
    "\\includegraphics[width=6.4in]{figure-pages/",
    spp_file, "-2.", ext, "}"
  )
  i <- i + 1
  out[[i]] <- "\\end{figure}\n"
  i <- i + 1
  out[[i]] <- "\n"
  out
})

temp <- lapply(temp, function(x) paste(x, collapse = "\n"))
temp <- paste(temp, collapse = "\n")
temp <- c("<!-- This page has been automatically generated: do not edit by hand -->\n", temp)
con <- file(file.path(build_dir, "plot-pages.Rmd"), encoding = "UTF-8")
writeLines(temp, con = con)

# ------------------------------------------------------------------------------
# Optimize png files for TeX

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
  }
  # } else if (gfplot:::is_windows() && parallel_processing) {
  #   library(doParallel)
  #   doParallel::registerDoParallel(cores = cores)
  #   fi <- list.files(".", "*.png")
  #   plyr::l_ply(fi, function(i) system(paste0("optipng -strip all ", i)),
  #     .parallel = TRUE
  #   )
  #   doParallel::stopImplicitCluster()
  # } else {
  #   temp <- lapply(fi, function(i) system(paste0("optipng -strip all ", i)))
  # }
  setwd(here())
}


}
