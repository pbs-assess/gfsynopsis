french <- FALSE
is_rstudio <- !is.na(Sys.getenv("RSTUDIO", unset = NA))
is_unix <- .Platform$OS.type == "unix"

if (french) {
  build_dir <- "report/report-rmd-fr"
} else {
  build_dir <- "report/report-rmd"
}
# This file generates all the main synopsis figures in `report/figure-pages`.
# It must be run before the report can be rendered.
library(here)
library(dplyr)
# library(gfplot)
devtools::load_all("../gfplot")
library(gfiphc)
library(gfsynopsis)
# devtools::load_all(".")
library(rosettafish)
# library(foreach)
library(future)
# setwd(here())
wd <- getwd()
options("sdmTMB.cores" = 4L)
if (!grepl("gfsynopsis", wd)) stop("Working directory wrong? Should be this repo main folder.")

# ------------------------------------------------------------------------------
# Settings:
ext <- "png" # pdf vs. png figs; png for CSAS and smaller file sizes
example_spp <- c("petrale sole", "pacific cod") # a species used as an example in the Res Doc
optimize_png <- TRUE # optimize the figures at the end? Need optipng installed.
parallel_processing <- FALSE
cores <- floor(future::availableCores() / 2.5)

# ------------------------------------------------------------------------------
# Set up parallel processing or sequential
options(future.globals.maxSize = 800 * 1024 ^ 2) # 800 mb
if (parallel_processing) {
  if (!is_rstudio && is_unix) {
    future::plan(multisession, workers = cores)
  } else {
    future::plan(sequential) # much frustration
  }
} else {
  future::plan(sequential)
}

# Read in fresh data or load cached data if available: ------------------------
dc <- here("report", "data-cache-april-2022")
gfsynopsis::get_data(type = c("A", "B"), path = dc, force = FALSE)
d_cpue <- readRDS(file.path(dc, "cpue-index-dat.rds"))
spp <- gfsynopsis::get_spp_names() %>%
  select(
    species_common_name, species_code,
    species_science_name, spp_w_hyphens, type, itis_tsn, worms_id
  )

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

# Missing from ITIS:
spp$order[spp$species_common_name == "deacon rockfish"] <-
  spp$order[spp$species_common_name == "vermilion rockfish"]
spp$family[spp$species_common_name == "deacon rockfish"] <-
  spp$family[spp$species_common_name == "vermilion rockfish"]

# downloaded from:
# https://species-registry.canada.ca/index-en.html#/species?ranges=1,18&taxonomyId=4&sortBy=commonNameSort&sortDirection=asc&pageSize=10
# on 2021-08-13
cos <- readr::read_csv(here::here("report/COSEWIC-species.csv"), show_col_types = FALSE)
cos <- dplyr::filter(cos, !grepl("Salmon", `COSEWIC common name`))
cos <- dplyr::filter(cos, !grepl("Trout", `COSEWIC common name`))
cos <- dplyr::filter(cos, !grepl("Eulachon", `COSEWIC common name`))
cos <- dplyr::filter(cos, !grepl("Pixie Poacher", `COSEWIC common name`))
cos <- rename(cos, species_science_name = `Scientific name`, cosewic_status = `COSEWIC status`, sara_status = `Schedule status`)
# duplicate of inside YE:
cos <- dplyr::filter(cos, !grepl("Pacific Ocean outside waters population", `COSEWIC population`))
cos <- select(cos, species_science_name, cosewic_status, sara_status)
cos <- mutate(cos, species_science_name = ifelse(grepl("type I", species_science_name), "Sebastes aleutianus/melanostictus", species_science_name))
cos$species_science_name <- tolower(cos$species_science_name)
spp <- left_join(spp, cos, by = "species_science_name")

# ------------------------------------------------------------------------------
# Parse metadata that will be used at the top of each species page:
refs <- readr::read_csv(here("report/spp-refs.csv"), show_col_types = FALSE)
spp <- left_join(spp, refs, by = "species_common_name")
spp$type_other_ref[!is.na(spp$type_other_ref)] <-
  rosettafish::en2fr(spp$type_other_ref[!is.na(spp$type_other_ref)], french)

spp$species_science_name <- gfplot:::firstup(spp$species_science_name)
spp$species_science_name <- gsub(" complex", "", spp$species_science_name)
spp$resdoc <- gsub(", ", ", @", spp$resdoc)
spp$resdoc <- ifelse(is.na(spp$resdoc), "", paste0("@", spp$resdoc, ""))
spp$sar <- gsub(", ", ", @", spp$sar)
spp$sar <- ifelse(is.na(spp$sar), "", paste0("@", spp$sar, ""))
spp$other_ref_cite <- ifelse(is.na(spp$other_ref), "",
  paste0(spp$type_other_ref, ": @", spp$other_ref, ""))
spp$other_ref_cite <- gsub(", ", ", @", spp$other_ref_cite)
spp <- arrange(spp, species_code)
spp <- spp %>% mutate(species_common_name = gsub("rougheye/blackspotted rockfish complex",
  "Rougheye/Blackspotted Rockfish Complex", species_common_name)) %>%
  mutate(species_common_name = gsub("c-o sole",
    "C-O Sole", species_common_name))
spp$species_french_name <-
  tolower(rosettafish::en2fr(gfsynopsis:::first_cap(spp$species_common_name)))
spp$species_common_name <- tolower(spp$species_common_name)

# ------------------------------------------------------------------------------
# CPUE model fits

# d_cpue <- readRDS(file.path(dc, "cpue-index-dat.rds"))
cpue_cache <- file.path("report", "cpue-cache")
dir.create(cpue_cache, showWarnings = FALSE)
# purrr::walk(spp$species_common_name[1:55], function(.sp) {
purrr::walk(spp$species_common_name[56:113], function(.sp) {
# purrr::walk(spp$species_common_name, function(.sp) {
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

# ------------------------------------------------------------------------------
# This is the guts of where the figure pages get made:

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
rlang::inform(paste(spp$species_common_name[to_build]))

length_ticks <- readr::read_csv(here::here("report/length-axis-ticks.csv"),
  show_col_types = FALSE)

# Trash compiled objects for safety:
unlink("vb_gfplot.*")
unlink("lw_gfplot.*")

# missing <- rep(TRUE, length(missing))
# for (i in to_build[seq(1, floor(length(to_build) / 2))]) {
# for (i in to_build[seq(floor(length(to_build) / 2), length(to_build))]) {
for (i in to_build) {
# out <- lapply(which(missing), function(i) {
# out <- future.apply::future_lapply(which(missing), function(i) {
  tryCatch({
    cat(crayon::red(clisymbols::symbol$cross),
      "Building figure pages for", spp$species_common_name[i], "\n")
    dat <- readRDS(file.path(dc, paste0(spp$spp_w_hyphens[i], ".rds")))

    dat_iphc <- readRDS(file.path(dc, paste0("iphc/", spp$spp_w_hyphens[i], ".rds")))
    dat$cpue_index <- d_cpue
    gfsynopsis::make_pages(
      dat = dat,
      dat_iphc = dat_iphc,
      spp = spp$species_common_name[i],
      d_geostat_index = NULL, # dat_geostat_index, # spatiotemporal model fits
      include_map_square = FALSE, # to check the map aspect ratio
      french = french,
      report_lang_folder = build_dir,
      resolution = 150, # balance size with resolution
      png_format = if (ext == "png") TRUE else FALSE,
      parallel = FALSE, # for CPUE fits; need a lot of memory if true!
      save_gg_objects = spp$species_common_name[i] %in% example_spp,
      synoptic_max_survey_years = list("SYN WCHG" = 2020, "SYN HS" = 2021, "SYN WCVI" = 2021, "SYN QCS" = 2021),
      hbll_out_max_survey_years = list("HBLL OUT N" = 2021, "HBLL OUT S" = 2020),
      iphc_max_survey_year = 2021,
      final_year_comm = 2021,
      final_year_surv = 2021,
      length_ticks = length_ticks[length_ticks$species_code == spp$species_code[i],],
      survey_cols = c(RColorBrewer::brewer.pal(5L, "Set1"),
        RColorBrewer::brewer.pal(8L, "Set1")[7:8],
        "#303030", "#a8a8a8", "#a8a8a8", "#a8a8a8")
    )
  # }, error = function(e) warning("Error"))
  }, error = function(e) stop("Error"))
# }, future.packages = c("gfplot", "gfsynopsis", "rosettafish", "gfiphc",
  # "magrittr", "dplyr", "boot", "rlang", "RColorBrewer", "ggplot2"))
# })
}

# Extracts just the CPUE map plots for Pacific Cod for the examples.
# These objects are too big to cache in an .Rmd file otherwise.
g_alt <- readRDS("report/report-rmd/ggplot-objects/pacific-cod.rds")
saveRDS(g_alt$cpue_spatial, file = "report/report-rmd/ggplot-objects/pacific-cod-cpue-spatial.rds")
saveRDS(g_alt$cpue_spatial_ll, file = "report/report-rmd/ggplot-objects/pacific-cod-cpue-spatial-ll.rds")
if (file.exists("report/report-rmd-fr/ggplot-objects")) {
  g_alt <- readRDS("report/report-rmd-fr/ggplot-objects/pacific-cod.rds")
  saveRDS(g_alt$cpue_spatial, file = "report/report-rmd-fr/ggplot-objects/pacific-cod-cpue-spatial.rds")
  saveRDS(g_alt$cpue_spatial_ll, file = "report/report-rmd-fr/ggplot-objects/pacific-cod-cpue-spatial-ll.rds")
}

# ------------------------------------------------------------------------------
# This is the guts of where the .tex / .Rmd figure page code gets made

# Generate `plot-pages.Rmd`:
temp <- lapply(spp$species_common_name, function(x) {
  spp_file <- gfsynopsis:::clean_name(x)
  if (french) {
    spp_title <- stringr::str_to_title(spp$species_french_name[spp$species_common_name == x])
  } else {
    spp_title <- stringr::str_to_title(x)
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
  if (species_code == "394") {
    if (!french) {
      out[[i]] <- paste0(en2fr("COSEWIC Status", french), ": ", en2fr("Special Concern", french), ", ", en2fr("SARA Status",french), ": ",  en2fr("Special Concern", french), "\n")
    } else {
      out[[i]] <- paste0(en2fr("COSEWIC Status", french), ":", en2fr("Special Concern", french), ", ", en2fr("SARA Status", french), ":", en2fr("Special Concern"), "\n")
    }
    i <- i + 1
  }
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
      out[[i]] <- "Note that Sablefish undergoes directed annual trap surveys,
    which are used for\n stock assessment and are not included in
    this report. The most recent\n stock assessment should be
    consulted for details on stock status."
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
      files_per_core, " -P ", cores, " optipng -strip all"
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
