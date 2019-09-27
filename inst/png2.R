cores <- parallel::detectCores() / 2
files_per_core <- 2
setwd(file.path("report", "report-rmd", "knitr-figs"))

system(paste0(
  "find -X . -name '*.png' -print0 | xargs -0 -n ",
  files_per_core, " -P ", cores, " optipng -strip all"
))

setwd("../../report-rmd-fr/knitr-figs/")

system(paste0(
  "find -X . -name '*.png' -print0 | xargs -0 -n ",
  files_per_core, " -P ", cores, " optipng -strip all"
))
