# get data for Ambre
library(gfdata)
library(dplyr)

spp <- c("pacific cod",
         "walleye pollock",
         "lingcod",
         "arrowtooth flounder",
         "pacific hake",
         "pacific halibut",
         "pacific ocean perch",
         "southern rock sole",
         "silvergray rockfish")


out <- list()
for (i in seq_along(spp)) {
  spp2 <- gsub(" ", "-", spp)
  d <- get_catch(spp)
  out[[i]] <- d
}

dat <- bind_rows(out)

glimpse(dat)

dat <- filter(dat, survey_abbrev %in%
                c("HBLL OUT N", "HBLL OUT S", "SYN WCHG", #"SYN WCVI",
                  "SYN HS", #"HBLL INS N", "HBLL INS S",
                  "MSA HS",
                  "SYN QCS"
                )) %>%
  arrange(survey_abbrev, species_common_name, year) %>%
  select(-survey_series_id)


readr::write_csv(dat, "inst/data/north-shelf-catch.csv")


# get_index()
