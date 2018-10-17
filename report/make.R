# library(INLA) # FIXME: could not find function "inla.models" on Windows? #31
devtools::load_all("../gfplot") # for development
devtools::load_all(".")  # for development
# library(gfplot)
# library(gfsynopsis)
library(dplyr)

# ------------------------------------------------------------------------------
# Settings:
ext <- "png" # PDF vs. PNG figs; PNG for CSAS, PDF for fast LaTeX
example_spp <- "petrale sole" # a species used as an example in the Res Doc
parallel <- FALSE
if (parallel) library(doParallel)

# ------------------------------------------------------------------------------
# Read in fresh data or load cached data if available:
dc <- file.path("report", "data-cache")
gfsynopsis::get_data(type = c("A"), path = dc, force = FALSE)
d_cpue <- readRDS(file.path(dc, "cpue-index-dat.rds"))
spp <- gfsynopsis::get_spp_names() %>%
  select(species_common_name, species_code, species_science_name, spp_w_hyphens, type)

# ------------------------------------------------------------------------------
# This section is used for hacked parallel processing from the command line.
# Unecessary, but speeds up rebuilding.
# e.g. from root project folder in macOS or Linux:
# open new Terminal
# make one
# open new Terminal
# make two
# open new Terminal
# make three
if (exists("N")) spp <- spp[N, , drop = FALSE]

# ------------------------------------------------------------------------------
# Parse metadata that will be used at the top of each species page:
# spp <- filter(spp, species_common_name != "pacific hake")
refs <- readr::read_csv("report/spp-refs.csv")
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
spp <- arrange(spp, type, species_code)

# ------------------------------------------------------------------------------
# This is the guts of where the figure pages get made:

# i <- which(spp$species_common_name  ==  'kelp greenling')

if (parallel) registerDoParallel(cores = floor(parallel::detectCores()/2))
plyr::l_ply(seq_along(spp$species_common_name), function(i) {
# for (i in seq_along(spp$species_common_name)) {
  fig_check <- paste0(file.path("report", "figure-pages"), "/",
    gfsynopsis:::clean_name(spp$species_common_name[i]))
  fig_check1 <- paste0(fig_check, "-1.", ext)
  fig_check2 <- paste0(fig_check, "-2.", ext)
  if (!file.exists(fig_check1) || !file.exists(fig_check2)) {
    cat(crayon::red(clisymbols::symbol$cross),
      "Building figure pages for", spp$species_common_name[i], "\n")
    dat <- readRDS(paste0(file.path(dc, spp$spp_w_hyphens[i]), ".rds"))
    dat$cpue_index <- d_cpue
    gfsynopsis::make_pages(dat, spp$species_common_name[i],
      include_map_square = FALSE,
      resolution = 160, # balance size with resolution
      png_format = if (ext == "png") TRUE else FALSE,
      parallel = FALSE,

      save_gg_objects = spp$species_common_name[i] %in% example_spp,
      survey_cols = c(RColorBrewer::brewer.pal(5L, "Set1"),
        RColorBrewer::brewer.pal(8L, "Set1")[7:8],
        "#303030", "#a8a8a8", "#a8a8a8", "#a8a8a8")
    )
  } else {
    cat(crayon::green(clisymbols::symbol$tick),
      "Figure pages for", spp$species_common_name[i], "already exist\n")
  }
}, .parallel = parallel)

# ------------------------------------------------------------------------------
# This is the guts of where the .tex / .Rmd figure page code gets made
spp$b_section <- c(FALSE, diff(as.numeric(as.factor(spp$type))) == 1)
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
  .b_section <- spp$b_section[spp$species_common_name == x]

  resdoc_text <- if (grepl(",", resdoc)) "Last Research Documents: " else "Last Research Document: "
  sar_text <- if (grepl(",", sar)) "Last Science Advisory Reports: " else "Last Science Advisory Report: "

  i <- 1
  out[[i]] <- "\\clearpage\n"
  if (.b_section) {
    i <- i + 1
    out[[i]] <- "# SYNOPSIS PLOTS: TYPE B SPECIES {#sec:synopsis-plots-B}\n"
  }
  i <- i + 1
  out[[i]] <- paste0("## ", spp_title, " {#sec:", spp_hyphen, "}\n")
  i <- i + 1
  out[[i]] <- paste0(gfsynopsis:::emph(latin_name),
    " | DFO species code: ", species_code, "\n")
  i <- i + 1
  out[[i]] <- paste0(resdoc_text, resdoc, "\n")
  i <- i + 1
  out[[i]] <- paste0(sar_text, sar)
  i <- i + 1
  if (!is.na(other_ref)) {
    if (other_ref != "") {
      out[[i]] <- paste0(other_ref)
      i <- i + 1
    }
  }
  out[[i]] <- "\\begin{figure}[b!]"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=6.4in]{../figure-pages/", spp_file, "-1.", ext, "}")
  i <- i + 1
  out[[i]] <- "\\end{figure}"
  i <- i + 1
  out[[i]] <- "\\clearpage"
  i <- i + 1
  out[[i]] <- "\\begin{figure}[b!]"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=6.4in]{../figure-pages/", spp_file, "-2.", ext, "}")
  i <- i + 1
  out[[i]] <- "\\end{figure}\n"
  i <- i + 1
  out[[i]] <- "\n"
  out
})

temp <- lapply(temp, function(x) paste(x, collapse = "\n"))
temp <- paste(temp, collapse = "\n")
if (!exists("N")) writeLines(temp, con = "report/report-rmd/plot-pages.Rmd")

# ------------------------------------------------------------------------------
# Make alphabetical index
# Now in .Rmd files

# spp_sorted <- arrange(spp, species_common_name)
# temp_ind <- lapply(spp_sorted$species_common_name, function(x) {
#   spp_title <- gfsynopsis:::first_cap(x)
#   spp_latin <- spp$species_science_name[spp$species_common_name == x]
#   spp_hyphen <- spp$spp_w_hyphens[spp$species_common_name == x]
#   paste0(spp_title, " (*", spp_latin, "*):", " \\@ref(", spp_hyphen, ")\n")
# })
# temp_ind <- c("# INDEX (SORTED BY COMMON NAME)\n", temp_ind)
# temp_ind <- paste(temp_ind, collapse = "")
# if (!exists("N")) writeLines(temp_ind, con = "report/report-rmd/alpha-index.Rmd")

# ------------------------------------------------------------------------------
# Optimize png files for TeX

if (!exists("N")) {
  cores <- parallel::detectCores()
  files_per_core <- ceiling(length(spp$species_common_name)*2/cores)
  setwd("report/figure-pages")
  if (!gfplot:::is_windows()) {
    system(paste0("find -X . -name '*.png' -print0 | xargs -0 -n ",
      files_per_core, " -P ", cores, " optipng -strip all"))
  } else {
    registerDoParallel(cores = cores)
    fi <- list.files(".", "*.png")
    plyr::l_ply(fi, function(i) system(paste0("optipng -strip all ", i)),
      .parallel = TRUE)
  }
  setwd("../..")
}
