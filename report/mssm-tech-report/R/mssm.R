if (!('mssm_loaded' %in% ls())) {
  source(here::here('report', 'mssm-tech-report', 'R', '00-load.R'))
}

optimize_png <- TRUE
if (optimize_png) {
  cores <- parallel::detectCores()
  files_per_core <- 4L
  wd <- getwd()
  setwd(mssm_figs)
  if (!gfplot:::is_windows()) {
    system(paste0(
      "find -X . -name '*.png' -print0 | xargs -0 -n ",
      files_per_core, " -P ", cores, " optipng -strip all"
    ))
  }
  setwd(wd)
}

