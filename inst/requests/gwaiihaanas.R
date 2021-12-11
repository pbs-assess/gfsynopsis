# gwaii haanas

library(tidyverse)
library(gfplot)
library(sf)

dc <- file.path("report", "data-cache")
if (!file.exists(file.path(dc, "yellowmouth-rockfish.rds"))) { # a random check
  gfsynopsis::get_data(type = "A", path = dc)
}


species <- "Yellowmouth Rockfish"
species <- "Darkblotched Rockfish"

spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

dat <- readRDS(paste0(file.path(dc, spp), ".rds"))


gwaiihaanas1 <- sf::st_read(here::here("inst/requests/GwaiiHaanas_Nov2018"),  quiet = TRUE)
gwaiihaanas2 <- gwaiihaanas1 %>% st_combine()
gwaiihaanas <- gwaiihaanas2 %>% st_transform(crs=3156)
gwaiihaanasLL <- gwaiihaanas2 %>% st_transform(crs=4326)

catch <- dat$catch %>% mutate(X = lon, Y = lat) %>% filter(!is.na(lon)) %>%
  filter(lon < -130.8 & lon > -133 & lat < 53 & lat >51.7)

catch_sf <-  st_as_sf(catch, coords = c("lon", "lat"), crs = 4326) %>% st_transform(crs=3156)

# ggplot(catch_sf) + geom_sf(alpha = 0.3)

keep <- st_intersects(gwaiihaanas, catch_sf)

catch_sf2 <- catch_sf[unlist(keep),]

catch_gh <- catch_sf2
catch_gh$geometry <- NULL

# saveRDS(catch_gh, "inst/requests/yellowmouth_gh_catch.rds")
# saveRDS(catch_gh, "inst/requests/darkblotched_gh_catch.rds")

ggplot(gwaiihaanasLL) + geom_sf(alpha = 0.3) +
  geom_point(data = filter(catch_gh, !(landed_kg + discarded_kg == 0)),
  # geom_jitter(data = filter(catch_gh, !(landed_kg + discarded_kg == 0)),
              width=0.02, height=0.03,
              aes(X, Y, colour = landed_kg + discarded_kg),
              alpha=0.9) +
  scale_colour_viridis_c("Total kg", trans = "log10") +
  ggtitle(paste(species)) +
  ggsidekick::theme_sleek() + theme(legend.position = c(0.2,0.2),
       axis.title = element_blank(), panel.grid.major = element_line(colour = "grey93"))

ggsave("Yellowmouth-Gwaii-Haanas-catch.pdf", width = 6, height = 6)
# ggsave("Darkblotched-Gwaii-Haanas-catch.pdf", width = 6, height = 6)



# # surveys don't go deep enough within the Gwaii Haanas footprint so almost no catch
# survey <- dat$survey_sets %>% mutate(X = longitude, Y = latitude,lon = longitude, lat = latitude) %>%
#   filter(!is.na(longitude)) %>%
#   filter(lon < -130.8 & lon > -133 & lat < 53 & lat >51.7)
#
# ggplot(gwaiihaanasLL) + geom_sf(alpha = 0.3) +
#   geom_point(data = filter(survey, !is.na(density_kgpm2)), aes(X, Y, colour = density_kgpm2)) +
#   scale_colour_viridis_c(trans = "log10")
#
# ggplot(gwaiihaanasLL) + geom_sf(alpha = 0.3) +
#   geom_point(data = filter(survey, !is.na(catch_count)), aes(X, Y, colour = catch_count)) +
#   scale_colour_viridis_c(trans = "log10")


# summary table

catch_gh1 <- readRDS("inst/requests/yellowmouth_gh_catch.rds") %>% mutate(species = "Yellowmouth")
catch_gh2 <- readRDS("inst/requests/darkblotched_gh_catch.rds") %>% mutate(species = "Darkblotched")

catch_gh <- bind_rows(catch_gh1 , catch_gh2)

total_gh <- catch_gh %>% mutate(zeros = ifelse(landed_kg + discarded_kg == 0, 1, 0)) %>% group_by(year, species) %>%
  summarise(total_kg = sum(landed_kg) + sum(discarded_kg),
            count_zeros = sum(zeros),
            count_events = n(),
            landed = sum(landed_kg), discarded = sum(discarded_kg)) %>% filter(year > 2006)

ggplot(total_gh) + geom_line(aes(year, total_kg, colour = species))


saveRDS(total_gh, "inst/requests/gwaiihaanas_catch_summary.rds")
readr::write_csv(total_gh, "inst/requests/gwaiihaanas_catch_summary.csv")
