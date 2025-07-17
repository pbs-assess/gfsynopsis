# Settings ------------------------------------------------------------

dc <- here::here("report", "data-cache-2025-03") # cache folder location
tag <- "haida" # e.g., main or haida (affects cache folder name)

ext <- "png" # pdf vs. png figs; png for CSAS and smaller file sizes
example_spp <- c("petrale sole", "pacific cod") # species used as example in the report
optimize_png <- TRUE # optimize the figures at the end? Need optipng installed.
parallel_processing <- TRUE
cores <- floor(future::availableCores() / 2)
french <- FALSE

# Likely only needed for Haida report
design_survey_to_include <- "SYN WCHG" # set to NULL if no design index should be included on survey index panel

# path based on tag for plot descriptions
ggplot_objects <- here::here("report", paste0("tech-report-", tag), "ggplot-objects")

# optional shape file to filter any spatial by:
if (tag == "haida") {
  shapefile <- sf::st_read(here::here("report/spatial-filtering/shape-files/haida/"))
} else {
  shapefile <- NULL
}


