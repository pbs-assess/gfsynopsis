# get data for Ambre
library(gfdata)
library(dplyr)
library(gfplot)

spp <- c("pacific cod",
         "walleye pollock",
         "lingcod",
         "arrowtooth flounder",
         "pacific hake",
         "pacific halibut",
         "pacific ocean perch",
         "southern rock sole",
         "silvergray rockfish")


dat <- get_catch(spp)


# readr::write_csv(dat, "inst/requests/all-silvergray-catch.csv")
# dat <- readr::read_csv("inst/requests/all-silvergray-catch.csv")
# readr::write_csv(d, "inst/requests/all-north-shelf-catch.csv")
dat <- readr::read_csv("inst/requests/all-north-shelf-catch.csv")

glimpse(dat)
tdat <- tidy_catch(dat, area = c("5A","5B","5C","5D","5E"))
plot_catch(tdat)
tdat2 <- tdat %>% rename(kg = value) %>% filter(year < 2021)

readr::write_csv(tdat2, "inst/requests/north-shelf-catch-by-area.csv")


## survey indices

i <- get_survey_index(spp)

unique(i$survey_abbrev)

i2 <- filter(i, survey_abbrev %in%
                         c("HBLL OUT N", #"HBLL OUT S",
                           "OTHER HS MSA", #"IPHC FISS",
                           "MSSM QCS",
                           "SYN WCHG",
                           "SYN HS",
                           "SYN QCS"
                         ))


readr::write_csv(i2, "inst/requests/north-shelf-survey-indices.csv")



# set level data if wanting to calculate new indices for certain areas

dat2 <- get_survey_sets(spp)

readr::write_csv(dat2, "inst/requests/all-north-shelf-surveys.csv")

glimpse(dat2)

unique(dat2$survey_abbrev)
unique(dat2$survey_series_desc)

get_major_areas() %>% View()

dat2 <- dat2 %>%
  mutate(area_num = as.numeric(major_stat_area_code))


dat4 <- dat2 %>%
  filter(is.na(area_num))
unique(dat4$survey_abbrev)


# trim to rough southern boundary or northern shelf bioregion
dat3 <- dat2 %>% filter(latitude > 49.8)

unique(dat3$area_num)
unique(dat3$survey_abbrev)

library(ggplot2)

dat3 %>% ggplot() +
  geom_point(aes(longitude, latitude, colour = survey_abbrev),
             size = 0.3, alpha = 0.7) +
  guides(colour = guide_legend(override.aes = list(size=7))) +
  theme_bw()
ggsave("inst/requests/survey-data-map.pdf")


readr::write_csv(dat3, "inst/requests/north-shelf-surveys.csv")

