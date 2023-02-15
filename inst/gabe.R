library(dplyr)
library(ggplot2)

f <- list.files("../gfsynopsis/report/cpue-cache/", full.names = TRUE)
f2 <- list.files("../gfsynopsis/report/cpue-cache/", full.names = FALSE)
f2 <- f2[!grepl("-model", f)]
f <- f[!grepl("-model", f)]
f
stopifnot(length(f2) == length(f))

# China rockfish
# Copper rockfish
# Tiger rockfish
# Darkblotched rockfish
# Greenstriped rockfish
# Shortraker rockfish
# Lingcod
# Longspine thornyhead
# Dover sole
# English sole
# Rex sole
# Flathead sole
# Sharpchin rockfish
# Giant and Pacific grenadier

get <- c()
get <- c(get, grep("china", f))
get <- c(get, grep("copper", f))
get <- c(get, grep("tiger", f))
get <- c(get, grep("darkblotched", f))
get <- c(get, grep("greenstriped", f))
get <- c(get, grep("shortraker", f))
get <- c(get, grep("lingcod", f))
get <- c(get, grep("longspine", f))
get <- c(get, grep("dover", f))
get <- c(get, grep("english", f))
get <- c(get, grep("rex", f))
get <- c(get, grep("flathead", f))
get <- c(get, grep("sharpchin", f))
get <- c(get, intersect(grep("gian", f), grep("grenadier", f)))

f[get]
f2[get]

cpue <- purrr::map_dfr(get, ~ {
  out <- readRDS(f[.x])
  sp <- stringr::str_to_title(gsub("\\.rds", "", gsub("-", " ", f2[.x])))
  if (!is.na(out)[[1]]) {
    out$species <- sp
    out
  } else {
    data.frame(year = NA, species = sp, area = "3CD", stringsAsFactors = FALSE)
  }
}) %>% dplyr::as_tibble()

ggplot(cpue, aes(year, est, ymin = lwr, ymax = upr)) +
  geom_ribbon(alpha = 0.2) + geom_line() +
  facet_grid(vars(species), vars(area), scales = "free_y") +
  theme_bw() +
  labs(x = "Year", y = "Standardized commercial\nbottom-trawl CPUE")

cpue$est_unstandardized <- NULL
cpue$est_link<- NULL
cpue$se_link<- NULL
cpue$model <- NULL
cpue <- as.data.frame(cpue)

ggsave("inst/gabe-cpue-request.pdf", width = 9.5, height = 18)
saveRDS(cpue, file = "inst/gabe-cpue-request.rds")

# surveys -------------------
f <- list.files("report/data-cache-3/", full.names = TRUE)

get <- c()
get <- c(get, grep("china", f))
get <- c(get, grep("copper", f))
get <- c(get, grep("tiger", f))
get <- c(get, grep("darkblotched", f))
get <- c(get, grep("greenstriped", f))
get <- c(get, grep("shortraker", f))
get <- c(get, grep("lingcod", f))
get <- c(get, grep("longspine", f))
get <- c(get, grep("dover", f))
get <- c(get, grep("english", f))
get <- c(get, grep("rex", f))
get <- c(get, grep("flathead", f))
get <- c(get, grep("sharpchin", f))
get <- c(get, intersect(grep("gian", f), grep("grenadier", f)))

stopifnot(length(get) == 14L)

f[get]

survey <- purrr::map_dfr(get, ~ {
  out <- readRDS(f[.x])$survey_index
  out
}) %>% dplyr::as_tibble()

survey <- dplyr::filter(survey, survey_abbrev %in%
    c("HBLL INS N", "HBLL INS S", "HBLL OUT N", "HBLL OUT S",
      "SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI"))

g <- survey %>%
  group_by(species_common_name, survey_abbrev) %>%
  mutate(lowerci = lowerci / exp(mean(log(biomass)))) %>%
  mutate(upperci = upperci / exp(mean(log(biomass)))) %>%
  mutate(biomass = biomass / exp(mean(log(biomass)))) %>%
  ggplot(aes(year, biomass, ymin = lowerci, ymax = upperci)) +
  geom_ribbon(alpha = 0.2) + geom_line(aes(colour = num_pos_sets / num_sets)) +
  facet_grid(vars(species_common_name), vars(survey_abbrev), scales = "free_y") +
  theme_bw() +
  coord_cartesian(ylim = c(0, 7)) +
  labs(x = "Year", y = "Index value\nscaled by geometric mean", colour = "Proportion\npositive\nsets") +
  scale_colour_viridis_c(trans = "log10")

ggsave("inst/gabe-survey-request.pdf", width = 17, height = 17)
saveRDS(survey, file = "inst/gabe-survey-request.rds")
