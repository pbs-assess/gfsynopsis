library(dplyr)

dat <- readxl::read_xlsx("~/Downloads/2006_2017 QMS TAC catch limit allocations.xlsx")
names(dat) <- tolower(names(dat))
dat$spp_agr_nme <- tolower(dat$spp_agr_nme)
dat$season_desc <- tolower(dat$season_desc)
dat$fishery_nme <- tolower(dat$fishery_nme)
dat$year <- lubridate::year(dat$effective_dtt)
dat <- rename(dat,
  area = faagr_nme, season = season_desc, fishery = fishery_nme,
  tac = tac_allocation, species = spp_agr_nme
)
dat <- mutate(dat, area = gsub(", ", "", area))
table(dat$area)

# Aggregate TAC by each fishery, year, species, and area:
dat <- group_by(dat, species, fishery, year, area) %>%
  summarize(tac = sum(tac))

# Remove areas that are to aggregated to match to any one survey:
dat <- filter(dat, !area %in%
  c(
    "Coastwide", "Gulf", "Offshore", "Offshore JV",
    "3D5A5B5C5D5E", "3C3D5A5B5C5D5E"
  ))
# "3C3D5A", "5C5D5E", "5D5E", "3D5A5B"))

dat <- group_by(dat, species) %>%
  mutate(n = n()) %>%
  filter(n > 1)

tribble(
  ~area, ~survey,
  "3C", "SYN WCVI",
  "3D", "SYN WCVI",
  "5C", "SYN QCS",
  "5B", "SYN QCS",
  "5E", "SYN WCHG",
  "5C5D", "SYN HS"
)
