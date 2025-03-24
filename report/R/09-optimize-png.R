# Optimize png files for TeX ------------------------------------------
# pdflatex is much faster if png files are optimized

if (optimize_png) {
  cores <- parallel::detectCores()
  files_per_core <- ceiling(length(spp$species_common_name) * 2 / cores)
  setwd(file.path(build_dir, "figure-pages"))
  if (!gfplot:::is_windows()) {
    system(paste0(
      "find -X . -name '*.png' -print0 | xargs -0 -n ",
      files_per_core, " -P ", cores, " optipng -strip all"
    ))
    setwd(here())
  }
}
