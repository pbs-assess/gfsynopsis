
cache             <- file.path("report", "data-cache2")
d_survey_sets     <- readRDS(file.path(cache, "pbs-survey-sets.rds"))
d_survey_samples  <- readRDS(file.path(cache, "pbs-survey-samples.rds"))
d_comm_samples    <- readRDS(file.path(cache, "pbs-comm-samples.rds"))
d_catch           <- readRDS(file.path(cache, "pbs-catch.rds"))
d_cpue_spatial    <- readRDS(file.path(cache, "pbs-cpue-spatial.rds"))
d_cpue_spatial_ll <- readRDS(file.path(cache, "pbs-cpue-spatial-ll.rds"))
d_survey_index    <- readRDS(file.path(cache, "pbs-survey-index.rds"))
d_age_precision   <- readRDS(file.path(cache, "pbs-age-precision.rds"))
d_cpue_index      <- readRDS(file.path(cache, "pbs-cpue-index.rds"))

d_age_precision$species_common_name <- tolower(d_age_precision$species_common_name)

library(dplyr)
d_survey_sets     <- filter(d_survey_sets, species_common_name == "pacific ocean perch")
d_survey_samples  <- filter(d_survey_samples, species_common_name == "pacific ocean perch")
d_comm_samples    <- filter(d_comm_samples, species_common_name == "pacific ocean perch")
d_catch           <- filter(d_catch, species_common_name == "pacific ocean perch")
d_cpue_spatial    <- filter(d_cpue_spatial, species_common_name == "pacific ocean perch")
d_cpue_spatial_ll <- filter(d_cpue_spatial_ll, species_common_name == "pacific ocean perch")
d_survey_index    <- filter(d_survey_index, species_common_name == "pacific ocean perch")
d_age_precision   <- filter(d_age_precision, species_common_name == "pacific ocean perch")

dat                 <- list()
dat$survey_sets     <- d_survey_sets
dat$survey_samples  <- d_survey_samples
dat$comm_samples    <- d_comm_samples
dat$catch           <- d_catch
dat$cpue_spatial    <- d_cpue_spatial
dat$cpue_spatial_ll <- d_cpue_spatial_ll
dat$survey_index    <- d_survey_index
dat$age_precision   <- d_age_precision
# dat$cpue_index      <- d_cpue_index
names(dat)

saveRDS(dat, "report/report/data/pop-example.rds", compress = FALSE)
