library(parallel)
library(foreach)
cores <- parallel::detectCores()[1L]
cl <- parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)

setwd("report/figure-pages")
fi <- list.files(".", "*.png")
library(doParallel)
registerDoParallel(cores = cores)

# out <- foreach::foreach(i = fi) %do% {
plyr::l_ply(fi, function(i) {
  system(paste0("optipng -strip all ", i))
}, .parallel = TRUE)

setwd("../..")

# fast: optipng -o2 -strip all <image.png>
