# longspine thornyhead MCP and CPUE map for 1996-2020.
# Notes on what to include: ####
# Extent of Occurrence of the species in Canada.
# Area over which the species is distributed using a “minimum convex polygon” drawn around the known occurrences of the species.
# https://jamesepaterson.github.io/jamespatersonblog/03_trackingworkshop_homeranges
#
# The minimum convex polygon is the smallest polygon in which no internal angle exceeds 180 degrees.
# Estimate of the Area of Occupancy from area of each hexagon.
#
# Approximate boundaries of these 3 fishing areas, 3CD, 5AB, and 5E.  I believe they are in PBSdata::major.

# packages of homerange calculation
library(sp)
library(adehabitatHR) # calculate MC polygon
library(scales) # Helps make polygons partly transparent using the alpha argument below

# other packages
library(tidyverse)
library(gfplot)

# need ggforce installed but not loaded to plot rounded corners on MCP
# devtools::install_github("thomasp85/ggforce")


# find major region boundaries ####
# TODO: activate cropping within retrieval function
load_boundaries <- function(#xlim_ll, ylim_ll,
  # IDS,
  utm_zone) {
  data("major", package = "PBSdata", envir = environment())
  # major <- filter(major, .data$PID %in% IDS)
  # major <- PBSmapping::clipPolys(major,
  #   xlim = xlim_ll + c(-3, 3),
  #   ylim = ylim_ll + c(-3, 3)
  # )
  gfplot:::ll2utm(major, utm_zone = utm_zone)
}

majorbound  <- load_boundaries(9)

# to see what's what
library(PBSmapping) # needs this for some reason
attributes(majorbound)$PolyData
# 5E = 9
# 5AB = 5,6,
bound5ABnorth <- fortify(majorbound) %>%
  filter(PID %in% c(5,6)) %>%  filter( #POS != 46 & # strange jog over HG
    X > 200 & X < 560 & Y> 5700 )
# 3CD
bound3CDnorth <- fortify(majorbound) %>%
  filter(PID %in% c(3,4)) %>%  filter(X > 200 & X < 600 & Y > 5550)

# 5CD = 7, 8
bound5CD <- fortify(majorbound) %>%
  filter(PID %in% c(7,8))

bound5CDwest <- fortify(majorbound) %>%
  filter(PID %in% c(8)) %>% filter(POS == 15 | POS == 14)

# result of gfdata::get_cpue_spatial
d_cpue <- readRDS("inst/data/longspine-thornyhead-cpue.rds")

# result of gfdata::get_cpue_historical
d <- readRDS("inst/data/longspine-thornyhead-cpue-hist.rds")

# modify d to match d_cpue for use with plot_cpue_spatial function
d <- d %>%
  mutate(
    cpue = total / hours_fished # calculate CPUE for historical data
  ) %>%
  select( # select and rename variables to match newer CPUE data
    year, best_date, major_stat_area_code, trip_id,
    fishing_event_id,
    lat = latitude, lon = longitude,
    vessel_registration_number = vessel,
    species_common_name, cpue
  )

d1 <- na.omit(d) %>%
  filter(year > 1995) %>%
  filter(year < 2021) %>% # set date range
  filter(cpue != 0) %>% # remove 0 catches as I don't think all are included in commercial dataset
  mutate( # correct numeric vectors that had NAs
    trip_id = as.numeric(trip_id),
    vessel_registration_number = as.numeric(vessel_registration_number),
    fishing_event_id = as.numeric(fishing_event_id)
  )

# option to code 0s as historical
# d1[d1$cpue == 0, ]$year <- 1995



# calc estimated total area occupied by commercial CPUE hexagons ####
# TODO: create function for area occupied by hexagons
d_hex <- plot_cpue_spatial(d1, start_year = 1996, n_minimum_vessels = 1, return_data = TRUE, show_historical = F, ylim = c(5350, 6027)) %>% # ylim = c(5373, 6027) is default
  filter(x > 0 & x < 10000) # I'm assuming all data is relevant & within Canadian waters?
# filter(x > 122 & x < 890 & y > 5350 & y < 6027) # or trim to Sean's plot default

# map full dataset
# plot_cpue_spatial(d1, start_year = 1996, n_minimum_vessels = 1, return_data = F, show_historical = F, ylim = c(5350, 6027))

# Area of hexagons with bin_width = 7
r <- 7 / 2
A <- (3 * sqrt(3) / 2) * r * r
A

# total area of commercial CPUE hexagons in km2
round(nrow(d_hex) * A)

# calc minimum convex polygon (MCP)####
# TODO: create function for adding MCP
.d <- d1 %>%
  mutate(id = "longspine thornyhead") %>%
  rename(X = lon, Y = lat) %>%
  gfplot:::ll2utm(., utm_zone = 9) %>%
  filter(cpue != 0)

sp1 <- .d %>%
  dplyr::select(id, X, Y) %>%
  mutate(X = X * 1000, Y = Y * 1000)
coordinates(sp1) <- c("X", "Y")
proj4string(sp1) <- CRS("+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs")
mcp1 <- mcp(sp1, percent = 100)

# area defaults to hectares, so for area in km2
round(mcp1@data$area / 100) # 151080

mcp1geo <- spTransform(mcp1, CRS("+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs"))
# Polygon layer needs to be "fortified" to add geometry to the dataframe
mcp1geo <- fortify(mcp1geo) %>% mutate(X = long / 1000, Y = lat / 1000)

# plot MCP of commercial catch with privacy enacted ####
plot_cpue_spatial(d1,
  start_year = 1996, n_minimum_vessels = 3,
  percent_excluded_xy = c(0.01, 0.01),
  show_historical = F, ylim = c(5350, 6027)
) +
  geom_line(
    data = bound5ABnorth,
    aes(X, Y), colour = "black", lty = 2,
    inherit.aes = F
  ) +
  geom_line(
    data = bound3CDnorth,
    aes(X, Y), colour = "black", lty = 2,
    inherit.aes = F
  ) +
  geom_line(
    data = bound5CDwest,
    aes(X, Y), colour = "black", lty = 2,
    inherit.aes = F
  ) +
  annotate("text", x = 290, y = 5800, label = "5E") + #x = 230, y = 5800,
  annotate("text", x = 380, y = 5690, label = "5AB") + #x = 230, y = 5650,
  annotate("text", x = 500, y = 5550, label = "3CD") + #x = 230, y = 5550,
  annotate("text", x = 360, y = 5900, label = "5CD") + #x = 300, y = 6040
  geom_polygon(
    data = mcp1geo,
    aes(X, Y), colour = "red", alpha = 0.01, inherit.aes = F
  ) +
  theme(text = element_text(size = 14)) +
  ggtitle(paste("MCP for 1996 through 2020 CPUE =",
    round(mcp1@data$area / 100), "km2"),
    subtitle = paste("Estimated area of occupancy", round(nrow(d_hex) * A), "km2"))

ggsave("inst/mcp_CPUE_1996to2020.png", width = 7, height = 7)



# Does survey data capture some areas not in the commercial data? ####
# add in survey data by calculating an equivalent CPUE from catch_weight and duration_min cols
d_surv <- readRDS("inst/data/longspine-thornyhead-survey-sets.rds") %>% mutate(
  cpue = catch_weight / (duration_min / 60)
)

# tidy data
# use fishing_event_id as dummy vessel_registration and duplicate dataframe to prevent survey data from being excluded by privacy filter

d_surv1 <- tidy_survey_sets(d_surv, c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG"),
  c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
    2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018,
    2019, 2020),
  utm_zone = 9,
  density_column = "cpue"
) %>%
  mutate(
    cpue = density,
    vessel_registration_number = fishing_event_id
  ) %>%
  dplyr::select(-X, -Y)

# recode 0 survey catches as prior to 1996, so can be plotted using show_historical argument
d_surv1[d_surv1$cpue == 0, ]$year <- 1995

d_surv2 <- d_surv1 %>% mutate(vessel_registration_number = fishing_event_id + 1000)
d_surv3 <- d_surv1 %>% mutate(vessel_registration_number = fishing_event_id + 2000)

d_both <- bind_rows(d_surv1, d_surv2, d_surv3, d1)

# # plot all commercial and survey CPUE combined
# plot_cpue_spatial(d_both, start_year = 1996, n_minimum_vessels = 1,
#   show_historical = T, # in this case empty circles = 0 survey catches
#   ylim = c(5350, 6035)) # extend borders slightly

# calc estimated total area occupied by commercial CPUE and survey hexagons ####
d_hex_b <- plot_cpue_spatial(d_both,
  start_year = 1996,
  n_minimum_vessels = 1, return_data = TRUE, # use all data and retrun hex list
  show_historical = F
) %>%
  filter(x > 0 & x < 10000) # remove dummy rows

# total area of commercial CPUE hexagons in km2
round(nrow(d_hex) * A)
# total area of hexagons in km2 with survey added
round(nrow(d_hex_b) * A)


# calc MCP for both commercial and survey CPUE together ####
.d_both <- d_both %>%
  filter(cpue != 0) %>%
  mutate(id = "longspine thornyhead") %>%
  rename(X = lon, Y = lat) %>%
  gfplot:::ll2utm(., utm_zone = 9)

sp2 <- .d_both %>%
  dplyr::select(id, X, Y) %>%
  mutate(X = X * 1000, Y = Y * 1000)

coordinates(sp2) <- c("X", "Y")
proj4string(sp2) <- CRS("+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs")
mcp2 <- mcp(sp2, percent = 100)
mcp2geo <- spTransform(mcp2, CRS("+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs"))
mcp2geo <- fortify(mcp2geo) %>% mutate(X = long / 1000, Y = lat / 1000)

# plot MCP for combined commercial and survey ####
plot_cpue_spatial(d_both,
  start_year = 1996,
  n_minimum_vessels = 3,
  percent_excluded_xy = c(0.01, 0.01),
  show_historical = T, ylim = c(5350, 6027)
) +
  geom_line(
    data = bound5ABnorth,
    aes(X, Y), colour = "black", lty = 2,
    inherit.aes = F
  ) +
  geom_line(
    data = bound3CDnorth,
    aes(X, Y), colour = "black", lty = 2,
    inherit.aes = F
  ) +
  geom_line(
    data = bound5CDwest,
    aes(X, Y), colour = "black", lty = 2,
    inherit.aes = F
  ) +
  annotate("text", x = 290, y = 5800, label = "5E") + # x = 230, y = 5800,
  annotate("text", x = 380, y = 5690, label = "5AB") + #x = 230, y = 5650,
  annotate("text", x = 500, y = 5550, label = "3CD") + #x = 230, y = 5550,
  annotate("text", x = 400, y = 5900, label = "5CD") + #x = 300, y = 6040
  ggforce::geom_shape(
    data = mcp2geo,
    expand = unit(0.15, 'cm'),
    radius = unit(1, 'cm'),
    aes(X, Y), fill = NA, colour = "red", #size = 0.1,
    inherit.aes = F
    ) +
  # and experiment with echoing a polygon to hide precise vertices
  # remotes::install_github("coolbutuseless/ggecho")
  # library(ggecho)
  # # jittering polygons by both similar and different amounts should make it impossible to identify the true one
  # geom_polygon(
  #   data = mcp2geo,
  #   stat = "echo", y_offset = 0, x_offset = 0, n = 1, size_increment = 0.1,
  #   aes(X, Y), fill = NA, colour = "red", size = 0.01,
  #   inherit.aes = F
  # ) +
  # geom_polygon(
  #   data = mcp2geo,
  #   stat = "echo", y_offset = 1, x_offset = 1, n = 1, size_increment = 0.1,
  #   aes(X, Y), fill = NA, colour = "red", size = 0.01,
  #   inherit.aes = F
  # ) +
  # geom_polygon(
  #   data = mcp2geo,
  #   stat = "echo", y_offset = 1, x_offset = -6, n = 1, size_increment = 0.1,
  #   aes(X, Y), fill = NA, colour = "red", size = 0.01,
  #   inherit.aes = F
  # ) +
  # geom_polygon(
  #   data = mcp2geo,
  #   stat = "echo", y_offset = -2, x_offset = -3, n = 1, size_increment = 0.1,
  #   aes(X, Y), fill = NA, colour = "red", size = 0.01,
  #   inherit.aes = F
  # ) +
  # geom_polygon(
  #   data = mcp2geo,
  #   stat = "echo", y_offset = 1, x_offset = -4, n = 1, size_increment = 0.1,
  #   aes(X, Y), fill = NA, colour = "red", size = 0.01,
  #   inherit.aes = F
  # ) +
  theme(text = element_text(size = 14)) +
  ggtitle(paste(
    "MCP for 1996 through 2020 CPUE + surveys =",
    round(mcp2@data$area / 100), "km2"
  ),
    subtitle = paste("Estimated area of occupancy", round(nrow(d_hex_b) * A), "km2"))

ggsave("inst/mcp_CPUE_1996to2020_wSurvey_rounded.png", width = 7, height = 7)
# ggsave("analysis/misc/mcp_CPUE_1996to2020_wSurvey_private.png", width = 7, height = 7)


