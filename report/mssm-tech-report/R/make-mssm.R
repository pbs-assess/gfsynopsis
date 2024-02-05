source(here::here('report', 'mssm-tech-report', 'R', '00-load.R'))
source(here::here('report', 'mssm-tech-report', 'R', '02-load-indices.R'))
source(here::here('report', 'mssm-tech-report', 'R', '03-age-size-frequencies.R'))
source(here::here('report', 'mssm-tech-report', 'R', '03-compare-indices.R'))
source(here::here('report', 'mssm-tech-report', 'R', '03-grids.R'))
source(here::here('report', 'mssm-tech-report', 'R', '03-mssm-wcvi-encounters.R'))
source(here::here('report', 'mssm-tech-report', 'R', '03-sampling-changes.R'))
source(here::here('report', 'mssm-tech-report', 'R', '04-rolling-window-correlation.R'))


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

