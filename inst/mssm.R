d <- readRDS("~/Downloads/pcod-mssm.rds")

library(ggplot2)
library(dplyr)
library(sdmTMB)

glimpse(d)

ggplot(d, aes(longitude, latitude, size = density_kgpm2)) +
  geom_point(data = filter(d, density_kgpm2 == 0), pch = 4, size = 1, alpha = 0.2) +
  geom_point(data = filter(d, density_kgpm2 > 0), pch = 21, mapping = aes(colour = density_kgpm2)) +
  scale_size_area(max_size = 12) +
  facet_wrap(~year) +
  theme_light() +
  coord_fixed() +
  scale_colour_viridis_c(trans = "log10")

d <- filter(d, year >= 2012)
ggplot(d, aes(longitude, latitude)) +
  geom_point() +
  # facet_wrap(~year) +
  theme_light() +
  coord_fixed() +
  scale_colour_viridis_c(trans = "log10")

