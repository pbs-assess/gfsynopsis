# This file generates all the main synopsis figures in `report/figure-pages`.
# It must be run before the report can be rendered.
library(here)
library(dplyr)
library(gfplot)
library(gfsynopsis)
library(foreach)

# ------------------------------------------------------------------------------
# Settings:
ext <- "png" # pdf vs. png figs; png for CSAS and smaller file sizes
example_spp <- c("petrale sole", "pacific cod") # a species used as an example in the Res Doc
optimize_png <- TRUE # optimize the figures at the end? Need optipng installed.
parallel_processing <- TRUE
cores <- round(parallel::detectCores()/2)

# ------------------------------------------------------------------------------
# Read in fresh data or load cached data if available:
dc <- here("report", "data-cache")
gfsynopsis::get_data(type = c("A", "B"), path = dc, force = FALSE)
d_cpue <- readRDS(file.path(dc, "cpue-index-dat.rds"))
spp <- gfsynopsis::get_spp_names() %>%
  select(species_common_name, species_code,
    species_science_name, spp_w_hyphens, type, itis_tsn, worms_id)

# Geostatistical model fits: (a bit slow)
fi <- here("report", "geostat-cache", "geostat-index-estimates.rds")
if (!file.exists(fi)) source(here("report/make-geostat.R"))
dat_geostat_index <- readRDS(fi)

# ------------------------------------------------------------------------------

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

if (!file.exists(here("report", "cosewic.rds"))) {
  cosewic <- gfplot::get_sara_dat()
  if (any(grepl("on_schedule", names(cosewic))))
    names(cosewic)[7] <- "schedule"
  saveRDS(cosewic, file = here("report", "cosewic.rds"))
} else {
  cosewic <- readRDS(here("report", "cosewic.rds"))
}
cosewic <- rename(cosewic, species_science_name = scientific_name) %>%
  select(species_science_name, population, range, cosewic_status,
    schedule, sara_status) %>%
  mutate(species_science_name = tolower(species_science_name))

cosewic <- filter(cosewic, !grepl("Atlantic", population))
cosewic <- filter(cosewic, !grepl("Pacific Ocean outside waters population", population))
spp <- left_join(spp, cosewic, by = "species_science_name")

# ------------------------------------------------------------------------------
# This section is used for hacked parallel processing from the command line.
# Unecessary, but speeds up rebuilding and no proper form of parallel processing
# seems to work in the figure building.
# e.g. from root project folder in macOS or Linux or maybe Cygwin on Windows:
# open new Terminal
# make one
# open new Terminal
# make two
# etc.
if (exists("N")) warning("A global variable `N` exists; filtering by N.")
if (exists("N")) spp <- spp[N, , drop = FALSE]

# ------------------------------------------------------------------------------
# Parse metadata that will be used at the top of each species page:
refs <- readr::read_csv(here("report/spp-refs.csv"))
spp <- left_join(spp, refs, by = "species_common_name")
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

# ------------------------------------------------------------------------------
# This is the guts of where the figure pages get made:
# i <- which(spp$species_common_name  ==  'copper rockfish') # for debugging
# for (i in seq_along(spp$species_common_name)) {

if (parallel_processing) {
  cl <- parallel::makeCluster(cores, outfile = "")
  doParallel::registerDoParallel(cl)
}
cpue_models <- foreach::foreach(i = seq_along(spp$species_common_name),
.packages = c("gfplot", "gfsynopsis"),
.export = c("ext", "d_cpue", "dat_geostat_index", "example_spp")) %dopar% {

  fig_check <- paste0(here::here("report", "figure-pages"), "/",
    gfsynopsis:::clean_name(spp$species_common_name[i]))
  fig_check1 <- paste0(fig_check, "-1.", ext)
  fig_check2 <- paste0(fig_check, "-2.", ext)
  if (!file.exists(fig_check1) || !file.exists(fig_check2)) {
    cat(crayon::red(clisymbols::symbol$cross),
      "Building figure pages for", spp$species_common_name[i], "\n")
    dat <- readRDS(file.path(dc, paste0(spp$spp_w_hyphens[i], ".rds")))
    dat_iphc <- readRDS(file.path(dc, paste0("iphc/", spp$spp_w_hyphens[i], ".rds")))
    dat$cpue_index <- d_cpue
    gfsynopsis::make_pages(
      dat = dat,
      dat_iphc = dat_iphc,
      spp = spp$species_common_name[i],
      d_geostat_index = dat_geostat_index,
      include_map_square = FALSE,
      report_folder = here::here("report"),
      resolution = 150, # balance size with resolution
      png_format = if (ext == "png") TRUE else FALSE,
      parallel = FALSE, # for CPUE fits; need a lot of memory if true!
      save_gg_objects = spp$species_common_name[i] %in% example_spp,
      survey_cols = c(RColorBrewer::brewer.pal(5L, "Set1"),
        RColorBrewer::brewer.pal(8L, "Set1")[7:8],
        "#303030", "#a8a8a8", "#a8a8a8", "#a8a8a8")
    )
  } else {
    cat(crayon::green(clisymbols::symbol$tick),
      "Figure pages for", spp$species_common_name[i], "already exist\n")
  }
}
doParallel::stopImplicitCluster()

# ------------------------------------------------------------------------------
# This is the guts of where the .tex / .Rmd figure page code gets made

# Generate `plot-pages.Rmd`:
temp <- lapply(spp$species_common_name, function(x) {
  spp_file <- gfsynopsis:::clean_name(x)
  spp_title <- gfsynopsis:::all_cap(x)
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

  resdoc_text <- if (grepl(",", resdoc)) "Last Research Documents: " else "Last Research Document: "
  sar_text <- if (grepl(",", sar)) "Last Science Advisory Reports: " else "Last Science Advisory Report: "

  i <- 1
  out[[i]] <- "\\clearpage\n"
  i <- i + 1
  out[[i]] <- paste0("## ", spp_title, " {#sec:", spp_hyphen, "}\n")
  i <- i + 1
  out[[i]] <- paste0(
    gfsynopsis:::emph(latin_name), " (", species_code, ")", "\\\n",
    "Order: ", spp$order[spp$species_common_name == x], ", ",
    "Family: ", spp$family[spp$species_common_name == x],
    ",")
  i <- i + 1
  out[[i]] <- paste0("[FishBase link]",
    "(http://www.fishbase.org/summary/",
    gsub(" ", "-", gfplot:::firstup(latin_name)), ")")
  if (species_code == '394') { # Sebastes aleutianus/melanostictus
    .names <- rougheye_split(gfplot:::firstup(latin_name))
    out[[i]] <- paste0("[FishBase link 1]",
      "(http://www.fishbase.org/summary/", .names[1], "),")
    i <- i + 1
    out[[i]] <- paste0("[FishBase link 2]",
      "(http://www.fishbase.org/summary/", .names[2], ")")
  }
  if (species_code == '039') { # Requiem Sharks
    out[[i]] <- paste0("[FishBase link]",
    "(http://www.fishbase.org/Summary/FamilySummary.php?ID=11)")
  }
  if (worms_id != 'unknown') {
    out[[i]] <- paste0(out[[i]], ", ")
    i <- i + 1
    out[[i]] <- paste0(
      '[WoRMS link]',
      '(http://www.marinespecies.org/aphia.php?p=taxdetails&id=',
      worms_id, ')')
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
      out[[i]] <- paste0(other_ref)
      if (!is.na(cosewic_status) && cosewic_status != "")
        out[[i]] <- paste0(out[[i]], "\\")
      i <- i + 1
    }
  }
  if (!is.na(cosewic_report)) {
    if (cosewic_report != "") {
      out[[i]] <- paste0("COSEWIC status report: @", cosewic_report, "\\")
      i <- i + 1
    }
  }
  if (!is.na(cosewic_status)) {
    if (cosewic_status != "") {
      out[[i]] <- paste0("COSEWIC status: ", cosewic_status)
      if (!is.na(sara_status))
        if (sara_status != "")
          out[[i]] <- paste0(out[[i]], ", SARA status: ", sara_status)
        out[[i]] <- paste0(out[[i]], "\n")
        i <- i + 1
    }
  }
  if (species_code == "394") { # FIXME Rougheye Rockfish
    out[[i]] <- "COSEWIC status: Special Concern, SARA status: Special Concern\n"
    i <- i + 1
  }
  if (species_code == "225") {
    out[[i]] <- "Note that Pacific Hake undergoes a directed joint
                Canada-US coastwide\n survey and annual assessment, which are not
                included in this report. The most recent\n stock assessment
                should be consulted for details on stock status."
    i <- i + 1
  }
  if (species_code == "614") {
    out[[i]] <- "Note that Pacific Halibut undergoes thorough assessment by the
                International Pacific\n Halibut Commission based on the annual
                standardized setline survey. The most\n recent stock assessment
                should be consulted for details on stock status."
    i <- i + 1
  }
  if (species_code == "455") {
    out[[i]] <- "Note that Sablefish undergoes directed annual trap surveys,
                which are used for\n stock assessment and are not included in
                this report. The most recent\n stock assessment should be
                consulted for details on stock status."
    i <- i + 1
  }
  out[[i]] <- "\\begin{figure}[b!]"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=6.4in]{../figure-pages/",
    spp_file, "-1.", ext, "}")
  i <- i + 1
  out[[i]] <- "\\end{figure}"
  i <- i + 1
  out[[i]] <- "\\clearpage"
  i <- i + 1
  out[[i]] <- "\\begin{figure}[b!]"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=6.4in]{../figure-pages/",
    spp_file, "-2.", ext, "}")
  i <- i + 1
  out[[i]] <- "\\end{figure}\n"
  i <- i + 1
  out[[i]] <- "\n"
  out
})

temp <- lapply(temp, function(x) paste(x, collapse = "\n"))
temp <- paste(temp, collapse = "\n")
temp <- c("<!-- This page has been automatically generated: do not edit by hand -->\n", temp)
if (!exists("N"))
  writeLines(temp, con = here("report", "report-rmd", "plot-pages.Rmd"))

# ------------------------------------------------------------------------------
# Optimize png files for TeX

if (!exists("N") && optimize_png) {
  cores <- parallel::detectCores()
  files_per_core <- ceiling(length(spp$species_common_name)*2/cores)
  setwd(here("report/figure-pages"))
  if (!gfplot:::is_windows()) {
    system(paste0("find -X . -name '*.png' -print0 | xargs -0 -n ",
      files_per_core, " -P ", cores, " optipng -strip all"))
  } else {
    library(doParallel)
    doParallel::registerDoParallel(cores = cores)
    fi <- list.files(".", "*.png")
    plyr::l_ply(fi, function(i) system(paste0("optipng -strip all ", i)),
      .parallel = TRUE)
  }
  setwd(here())
}
