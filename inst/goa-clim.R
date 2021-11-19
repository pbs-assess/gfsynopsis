library(dplyr)
sp <- gfsynopsis::get_spp_names()
d <- readRDS("report/data-cache/cpue-index-dat.rds")
d <- d %>% mutate(species_common_name = tolower(species_common_name))
nrow(d)
d <- filter(d, species_common_name %in% sp$species_common_name)
nrow(d)
area_grep_pattern <- "^5A|^5B|^5C|^5D|^5E|^3C|^3D"
pbs_areas <- gfplot::pbs_areas[grep(area_grep_pattern,
  gfplot::pbs_areas$major_stat_area_description), , drop = FALSE]
d <- left_join(pbs_areas, d)
d <- d %>% mutate(year = lubridate::year(best_date))
dat <- d %>%
  filter(year <= 2020) %>%
  group_by(year, species_common_name, major_stat_area_description) %>%
  summarise(catch_kg = sum(landed_kg, na.rm = TRUE) + sum(discarded_kg, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(species_common_name, major_stat_area_description, year)
dat <- left_join(dat, select(sp, species_common_name, species_science_name, itis_tsn, species_code, worms_id))
saveRDS(dat, file = "~/Desktop/goa-clim-bc-groundfish-2020.rds")
