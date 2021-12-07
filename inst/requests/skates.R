# get skates

library(gfdata)
library(dplyr)

spp <- c("big skate",
"longnose skate")


out <- list()
for (i in seq_along(spp)) {
  spp2 <- gsub(" ", "-", spp)
  d <- get_catch(spp)
  out[[i]] <- d
}

dat <- bind_rows(out)

glimpse(dat)

dat <- dat %>%
  arrange(survey_abbrev, species_common_name, year) %>%
  select(-survey_series_id)

readr::write_csv(dat, "inst/data/skates.csv")
