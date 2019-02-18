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

dat <- group_by(dat, species) %>%
  mutate(n = n()) %>%
  filter(n > 1)

survey_look_up <- tribble(
  ~area, ~survey_syn, ~survey_ll,
  "3C", "SYN WCVI", "HBLL OUT S",
  "3C3D", "SYN WCVI", "HBLL OUT S",
  "3C3D5A", "SYN WCVI", NA,
  "3C3D5A5B", NA, NA,
  "3D", "SYN WCVI", "HBLL OUT S",
  "3D5A5B", NA, "HBLL OUT S",
  "4B", NA, NA,
  "5A5B", "SYN QCS", "HBLL OUT S",
  "5A5BArea 12", "SYN QCS", "HBLL OUT S",
  "5B", "SYN QCS", "HBLL OUT S",
  "5C", "SYN QCS", "HBLL OUT N",
  "5C5D", "SYN HS", "HBLL OUT N",
  "5C5D5E", NA, "HBLL OUT N",
  "5D5E", NA, "HBLL OUT N",
  "5E", "SYN WCHG", "HBLL OUT N",
  "3C3D5A5B5C5D5E", NA, "IPHC FISS",
  "3D5A5B5C5D5E", NA, "IPHC FISS",
  "Coastwide", NA, "IPHC FISS",
  "Gulf", NA, NA,
  "Offshore", NA, NA,
  "Offshore JV", NA, NA
)

# survey_type_look_up <- tribble(
#   ~species, ~survey_type,
#   "arrowtooth flounder", FALSE,
#   "big skate",
#   "bocaccio",
#   "canary rockfish",
#   "copper,
#    china & tiger rockfish",
#   "corals and sponges",
#   "dover sole",
#   "english sole",
#   "halibut",
#   "lingcod",
#   "long nose skate",
#   "longspine thornyheads",
#   "pacific cod",
#   "pacific hake",
#   "pacific ocean perch",
#   "petrale sole",
#   "quillback rockfish",
#   "quillback,
#    copper,
#    china & tiger rockfish",
#   "redbanded rockfish",
#   "redstripe rockfish",
#   "rock sole",
#   "rougheye rockfish",
#   "sablefish",
#   "shortraker rockfish",
#   "shortspine thornyheads",
#   "silvergray rockfish",
#   "spiny dogfish",
#   "trawl other rockfish",
#   "walleye pollock",
#   "widow rockfish",
#   "yelloweye rockfish",
#   "yellowmouth rockfish",
#   "yellowtail rockfish"
# )

dat <- inner_join(dat, survey_look_up, by = "area")

trawl <- group_by(dat, species, year, survey_syn) %>%
  summarize(tac_total = sum(tac, na.rm = TRUE)) %>%
  rename(survey = survey_syn)

ll <- group_by(dat, species, year, survey_ll) %>%
  summarize(tac_total = sum(tac, na.rm = TRUE)) %>%
  rename(survey = survey_ll)

dat_condensed <- bind_rows(trawl, ll) %>%
  filter(!is.na(survey)) %>%
  ungroup() %>%
  rename(survey_abbrev = survey)

# Make sure the species names match:
dat_condensed <- dat_condensed %>% mutate(species = gsub("thornyheads", "thornyhead", species))
dat_condensed <- dat_condensed %>% mutate(species = gsub("rock sole", "southern rock sole", species))
dat_condensed <- dat_condensed %>% mutate(species = gsub("spiny dogfish", "north pacific spiny dogfish", species))
dat_condensed <- dat_condensed %>% mutate(species = gsub("long nose skate", "longnose skate", species))
dat_condensed <- dat_condensed %>% mutate(species = gsub("rougheye rockfish", "rougheye blackspotted rockfish complex", species))

add_survey_data <- function(x) {
  species <- gsub(" ", "-", x$species[1])
  file <- file.path("report/data-cache/", paste0(species, ".rds"))
  if (!file.exists(file)) {
    warning("Missing: ", species)
    return(data.frame())
  }
  if (file.exists(file)) {
    message("Joining data for ", species)
    .d <- readRDS(file)
    .d <- .d$survey_index
    .d <- filter(.d, survey_abbrev %in% unique(x$survey_abbrev))
    .d <- select(
      .d, year, biomass, lowerci,
      upperci, re, survey_abbrev
    )
    out <- inner_join(.d, x, by = c("year", "survey_abbrev"))
    return(out)
  }
}

data_merged <- dat_condensed %>%
  group_by(species) %>%
  do({
    add_survey_data(.)
  })

library(ggplot2)
data_merged %>%
  group_by(species, survey_abbrev) %>%
  mutate(tac_total = scale(tac_total), biomass = scale(biomass)) %>%
  ggplot() +
  aes(biomass, tac_total) +
  geom_point() +
  facet_grid(survey_abbrev ~ species)

d <- data_merged %>%
  group_by(species, survey_abbrev) %>%
  mutate(biomass_scaled = scale(biomass)) %>%
  mutate(tac_total / 100) %>%
  mutate( # tac_total = tac_total / max(tac_total, na.rm = TRUE),
    species_survey = paste(species, survey_abbrev)
  ) %>%
  mutate(tac_total = tac_total + 1) %>%
  filter(!is.na(biomass_scaled)) %>%
  filter(!is.na(tac_total))

# library(glmmTMB)
# m <- glmmTMB(log(tac_total) ~ biomass_scaled + (1 + biomass_scaled | species_survey),
#   data = d)
# random_effects <- ranef(m)[[1]]$species_survey
# random_effects$species_survey <- row.names(random_effects)
# row.names(random_effects) <- NULL

m <- lme4::lmer(log(tac_total) ~ biomass_scaled + (1 + biomass_scaled | species_survey),
  data = d
)

random_effects <- as.data.frame(ranef(m)) %>%
  filter(term %in% "biomass_scaled") %>%
  arrange(condval) %>%
  mutate(grp = forcats::fct_reorder(grp, condval)) %>%
  mutate(condval = condval + summary(m)$coef[2, 1])

ggplot(random_effects, aes(grp, condval)) +
  geom_pointrange(aes(ymin = condval - 2 * condsd, ymax = condval + 2 * condsd)) +
  coord_flip() + xlab("") +
  geom_hline(yintercept = 0, lty = 2)
