devtools::load_all("../gfplot")
devtools::load_all(".")
library(dplyr)

# ------------------------------------------------------------
dc <- file.path("report", "data-cache")
if (!file.exists(file.path(dc, "pacific-ocean-perch.rds"))) { # a random check
  gfsynopsis::get_data(type = "A", path = dc)
}
spp <- gfsynopsis:::get_spp_names() %>% filter(type == "A")
spp <- filter(spp, species_common_name != "pacific hake")

out <- list()
for (i in seq_along(spp$species_common_name)) {
  dat <- readRDS(paste0(file.path(dc, spp$spp_w_hyphens[i]), ".rds"))
  d <- dat$survey_index
  out[[i]] <- d
}

dat <- bind_rows(out)
dat <- filter(dat, survey_abbrev %in% c('HBLL OUT N', 'HBLL OUT S', 'SYN WCHG', 'SYN WCVI', 'SYN HS', 'HBLL INS N', 'HBLL INS S', 'SYN QCS'))

dat <- filter(dat, survey_abbrev %in% c('SYN WCHG', 'SYN WCVI', 'SYN HS', 'SYN QCS')) %>%
  arrange(survey_abbrev, species_common_name, year)


glimpse(dat)

readr::write_csv(dat, "pbs-gf-trawl.csv")
