# d <- readRDS("report/data-cache/arrowtooth-flounder.rds")
# d <- d$survey_sets
#
#
#
# library(dplyr)
# library(ggplot2)
#
# dat <- select(d, survey_abbrev, species_science_name, species_common_name, year, latitude, longitude, density_kgpm2)
#
# head(as.data.frame(dat))
#


library(dplyr)

dc <- file.path("report", "data-cache")
if (!file.exists(file.path(dc, "pacific-ocean-perch.rds"))) { # a random check
  gfsynopsis::get_data(type = "A", path = dc)
}
spp <- gfsynopsis:::get_spp_names() %>% filter(type == "A")
spp <- filter(spp, species_common_name != "pacific hake")

out <- list()
for (i in seq_along(spp$species_common_name)) {
  message(spp$species_common_name[i])
  dat <- readRDS(paste0(file.path(dc, spp$spp_w_hyphens[i]), ".rds"))
  d <- dat$survey_sets
  d <- select(d, survey_abbrev, species_science_name, species_common_name, year, latitude, longitude, density_kgpm2)
  out[[i]] <- d
}

dat <- bind_rows(out)
dat <- filter(dat, survey_abbrev %in%
    c("SYN WCHG", "SYN WCVI", "SYN HS", "SYN QCS")) %>%
  arrange(survey_abbrev, species_common_name, year)
glimpse(dat)

dat <- as.data.frame(dat)
saveRDS(dat, file = "~/Desktop/pbs-synoptic-2019-10-17.rds")
