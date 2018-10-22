load_all("../gfplot/")
load_all(".")
dc <- "report/data-cache"
dir.create(dc, showWarnings = FALSE)
dir.create("report/figs", showWarnings = FALSE)

# cache_pbs_data(..., path = dc)

dat <- list()
dat$survey_tows     <- readRDS(file.path(dc, "pbs-survey-sets.rds"))
dat$survey_samples  <- readRDS(file.path(dc, "pbs-survey-samples.rds"))
dat$comm_samples    <- readRDS(file.path(dc, "pbs-comm-samples.rds"))
dat$catch           <- readRDS(file.path(dc, "pbs-catch.rds"))
dat$cpue_spatial    <- readRDS(file.path(dc, "pbs-cpue-spatial.rds"))
dat$cpue_spatial_ll <- readRDS(file.path(dc, "pbs-cpue-spatial-ll.rds"))
dat$survey_index    <- readRDS(file.path(dc, "pbs-survey-index.rds"))
dat$age_precision   <- readRDS(file.path(dc, "pbs-age-precision.rds"))

lu <- unique(dat$survey_tows[,c("species_common_name", "species_code")])

# > unique(lu$species_common_name)
# [1] "big skate"                              "longnose skate"
# [3] "pacific cod"                            "pacific hake"
# [5] "walleye pollock"                        "rougheye/blackspotted rockfish complex"
# [7] "pacific ocean perch"                    "redbanded rockfish"
# [9] "shortraker rockfish"                    "silvergray rockfish"
# [11] "copper rockfish"                        "darkblotched rockfish"
# [13] "widow rockfish"                         "yellowtail rockfish"
# [15] "quillback rockfish"                     "bocaccio"
# [17] "canary rockfish"                        "redstripe rockfish"
# [19] "yellowmouth rockfish"                   "yelloweye rockfish"
# [21] "shortspine thornyhead"                  "longspine thornyhead"
# [23] "sablefish"                              "lingcod"
# [25] "arrowtooth flounder"                    "petrale sole"
# [27] "rex sole"                               "southern rock sole"
# [29] "dover sole"                             "english sole"

sn <- "lingcod"
sc <- lu$species_code[lu$species_common_name == sn]
# ageing_method_codes <- 6
ageing_method_codes <- c(3, 17)

d <- lapply(dat, function(x) {
  if ("species_common_name" %in% names(x))
    dplyr::filter(x, species_common_name == sn)
  else
    dplyr::filter(x, species_code == sc)
})

tidy_age_precision(d$age_precision) %>%
  plot_age_precision()
ggsave("~/Desktop/precision.pdf", width = 6, height = 6)

tidy_ages_raw(d$survey_samples) %>%
  plot_ages()

make_pages(d, sn, output_path = "report/figs",
           ageing_method_codes = ageing_method_codes)  #AME: don't this file is used anymore but make_pages now
                                                       #  needs dat_iphc as second argument
