library(parallel)
library(foreach)
cores <- parallel::detectCores()[1L]
cl <- parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)

setwd("report/figure-pages")
fi <- list.files(".", "*.png")
out <- foreach::foreach(i = fi) %dopar% {
  system(paste0("optipng -strip all ", i))
}

# fast: optipng -o2 -strip all <image.png>
