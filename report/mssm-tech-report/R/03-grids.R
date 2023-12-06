if (!('mssm_loaded' %in% ls())) {
  source(here::here('report', 'mssm-tech-report', 'R', '00-load.R'))
}

sf_use_s2(FALSE)

# Setup coastline
utm_zone = 9
rotation_angle = 40
rotation_center = c(500, 5700)
bath = c(100, 200, 400, 600, 800, 1000, 1200)
buffer = c(-0.5, 1)

xlim_ll <- st_bbox(mssm_grid_sf)[c(1, 3)] + buffer
ylim_ll <- st_bbox(mssm_grid_sf)[c(2, 4)] + buffer

mssm_utm <- mssm_grid_sf |> st_transform(crs = 32609)

coast_utm <- gfplot:::load_coastline(xlim_ll, ylim_ll, utm_zone = 9) |>
  st_as_sf(coords = c('X', 'Y'), crs = 32609) |>
  group_by(PID) |>
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") |>
  mutate(geometry = geometry * 1000) |>
  st_set_crs(32609)

coast_ll <- coast_utm |> st_transform(crs = 4326)

data(bcBathymetry, package = "PBSmapping")

iso <- bcBathymetry
m <- matrix(iso$z, nrow = 540, ncol = 724)
m[m < 0] <- 0

test <- st_as_stars(m, st_dimensions(x = iso$x, y = iso$y))

test2 <- st_contour(test, na.rm = TRUE, breaks = 50)
plot(test2)


iso_utm <- gfplot:::load_isobath(xlim_ll, ylim_ll, bath = bath,
  utm_zone = utm_zone) |>
  st_as_sf(coords = c('X', 'Y'), crs = 32609) |>
  group_by(PID, SID) |>
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("LINESTRING") |>
  ungroup() |>
  mutate(geometry = geometry * 1000) |>
  st_set_crs(32609)

iso_ll <- iso_utm |> st_transform(crs = 4326)

crop_coast <- function(coast, bbox) {
  coast |> st_crop(y = bbox)
}

crop_isobath <- function(iso, cropped_coast) {
  iso |>
    #st_intersection(st_as_sfc(st_bbox(cropped_coast), crs=st_crs(cropped_coast))) # no idea why st_crop isn't doing the same as this
    st_crop(y = st_bbox(cropped_coast))
}


cropped_coast <- crop_coast(coast_ll, c(xmin = -129, ymin = 47.5, xmax = -123, ymax = 52))
cropped_isobath <- crop_isobath(iso_ll, cropped_coast)

close_coast <- crop_coast(coast_ll, st_bbox(mssm_grid_sf) + c(-0.5, -1.2, 1.2, 0.5))
close_isobath <- crop_isobath(iso_ll, close_coast)

ggplot() +
  geom_sf(data = iso_ll, colour = 'grey80', linewidth = 0.3) +
  geom_sf(data = coast_ll) +
  geom_sf(data = mssm_grid_sf) +
  coord_sf(xlim = c(-127.9, -124), ylim = c(48.4, 51))


ggplot() +
  geom_sf(data = cropped_isobath, colour = 'grey80') +
  geom_sf(data = cropped_coast) +
  geom_sf(data = mssm_grid_sf)


# ---- Compare grid cell size -----
# Make 3x3 km grid ---
mssm_grid_3km <- pcod_dat |>
  mk_mssm_grid(grid_spacing = 3000)

mssm_grid_3km[[1]] |>
  dplyr::filter(year >= 2009 & year <= 2021) |>
  dplyr::distinct(X, Y, .keep_all = TRUE)

mssm_grid_3km[[2]] |>
  filter(year >= 2009 & year <= 2021) |>
  distinct(geometry, .keep_all = TRUE) |>
  ggplot() +
  geom_sf(aes(fill = year)) +
  geom_sf(data = mssm_grid_3km[[2]] |> filter(year == 2021), fill = 'pink') +
  geom_sf(data = pcod_sf |> filter(year == 2021), shape = 21, size = 3, fill = 'white') +
  geom_sf(data = mssm_grid_3km[[2]] |> filter(year == 2019), colour = 'purple', fill = NA) +
  geom_sf(data = pcod_sf |> filter(year == 2019), shape = 21, size = 3, fill = 'white')

# mssm_grid_3km[[1]] |>
#   filter(year >= 2009 & year <= 2021) |>
#   dplyr::distinct(X, Y, .keep_all = TRUE) |>
# saveRDS(file.path(grid_dir, 'mssm-grid-3km_2009-2021.rds'))

# Make 2x2 km grid
mssm_grid_2km <- pcod_dat |>
  mk_mssm_grid(grid_spacing = 2000)

km2 <-
  ggplot(data = mssm_grid_2km[[2]] |> filter(year >= 2009 & year <= 2021)) +
  geom_sf(data = pcod_sf, shape = 1, colour = 'grey50', alpha = 0.8, size = 0.1) +
  geom_sf(alpha = 0.2) +
  scale_fill_manual(values = grid_colours) +
  scale_x_continuous(breaks = seq(-127.4, -126.0, by = 0.4))

km3 <-
  ggplot(data = mssm_grid_3km[[2]] |> filter(year >= 2009 & year <= 2021)) +
  geom_sf(data = pcod_sf, shape = 1, colour = 'grey50', alpha = 0.8, size = 0.1) +
  geom_sf(alpha = 0.2) +
  theme(axis.text.y = element_blank()) +
  scale_fill_manual(values = grid_colours) +
  scale_x_continuous(breaks = seq(-127.4, -126.0, by = 0.4))
km2 + km3

ggsave(filename = file.path(mssm_figs, '2km-3km-grid-comp.png'), width = 6.7, height = 4.6)

# --- Look at spatial distribution of sampling ------------
# Grid from GFBioField
sgrid <- sf::st_read(file.path(mssm_data, "SMMS-grid/SMMS_Survey_Blocks.shp"))

gfbio_grid <- sgrid |>
  # Select only sites off WCVI (since there is no ssid corresponding to 7 or 'MSSM WCVI')
  filter(GROUPING_C %in% c(112, 113)) |>
  sf::st_crop(sgrid, sf::st_bbox(c(xmin = -128, ymin = 48.5, xmax = -126, ymax = 50))) %>%
  sf::st_set_crs('WGS84') %>%
  mutate(area = units::set_units(sf::st_area(.), km^2))

# --- Grid used in synopsis

grid_plot <- mssm_grid_sf |>
  filter(year >= 2009 & year <= 2019) |>
  distinct(geometry) |>
  ggplot() +
    geom_sf(data = pcod_sf, colour = 'grey50', shape = 1, alpha = 0, size = 0.1) +
    geom_sf(aes(fill = '2009'), alpha = 0.5) +
    scale_fill_manual(values = grid_colours) +
    labs(fill = "Grid") +
    theme(legend.position = c(0.8, 0.9)) +
    scale_x_continuous(breaks = seq(-127.4, -126.0, by = 0.4))
grid_plot + geom_sf(data = gfbio_grid, alpha = 0, colour = NA) +
    theme(axis.text = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.title = element_blank())

ggsave(file.path(mssm_figs, 'grid-prediction-2009_no-points.png'), width = 3.5, height = 3.7)

grid_plot_2009_points <- mssm_grid_sf |>
  filter(year >= 2009 & year <= 2019) |>
  distinct(geometry) |>
  ggplot() +
    geom_sf(data = pcod_sf |> filter(year < 2009), shape = 1, colour = 'grey50', alpha = 0.5, size = 0.1) +
    geom_sf(aes(fill = '2009'), alpha = 0.5) +
    geom_sf(data = pcod_sf |> filter(year >= 2009 & year <= 2019), shape = 1, colour = 'black', alpha = 0.5, size = 0.1) +
    scale_fill_manual(values = grid_colours) +
    labs(fill = "Grid") +
    theme(legend.position = c(0.8, 0.9))
grid_plot_2009_points + geom_sf(data = gfbio_grid, alpha = 0, colour = NA) +
    theme(axis.text = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.title = element_blank()) +
    scale_x_continuous(breaks = seq(-127.4, -126.0, by = 0.4)) +
    guides(fill = "none")

ggsave(file.path(mssm_figs, 'grid-prediction-2009.png'), width = 3.5, height = 3.7)
#ggsave(file.path(here::here('report', 'tech-report', 'figure'), 'grid-prediction-2009.png'),
#  width = 3.5, height = 3.7)
#system("optipng -strip all report/tech-report/figure/grid-prediction-2009.png")

# --- Overlay blocks shown/used in GFBioField
gfbio_field_grid_plot1 <- mssm_grid_sf |>
  filter(year >= 2009 & year <= 2019) |>
  distinct(geometry) |>
  ggplot() +
  geom_sf(data = pcod_sf, shape = 1, colour = 'grey50', alpha = 0.5, size = 0.1) +
  geom_sf(aes(fill = '2009'), alpha = 0.7) +
  geom_sf(data = gfbio_grid, aes(fill = 'GFBioField'), alpha = 0.7) +
  scale_fill_manual(values = grid_colours) +
    labs(fill = "Grid") +
    theme(legend.position = c(0.8, 0.9)) +
    scale_x_continuous(breaks = seq(-127.4, -126.0, by = 0.4))
gfbio_field_grid_plot1 +
  theme(axis.text = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.title = element_blank())

ggsave(file.path(mssm_figs, 'grid-prediction-gfbiofield-1.png'), width = 3.5, height = 3.7)

# --- Historical survey domain
# The grid was created as any 3x3 km grid cell, that overlapped with at least one
# sampling location.
# The overlay grid covered the bounding box of all sampling locations
df_2022 <- mssm_grid_sf |> filter(year == 2022)
df_2009_2021 <- mssm_grid_sf |> filter(year >= 2009 & year < 2022)

grid_historical_plot <-
  ggplot(data = mssm_grid_sf |> distinct(geometry)) +
    geom_sf(aes(fill = "1975 (Loran A)"), alpha = 1) +
    geom_sf(data = mssm_grid_sf |> filter(year >= 1979) |> distinct(geometry),
      aes(fill = "1979 (Loran C)"), alpha = 1) +
    geom_sf(data = mssm_grid_sf |> filter(year >= 1998) |> distinct(geometry),
      aes(fill = "1998 (GPS)"), alpha = 1) +
    geom_sf(data = mssm_grid_sf |> filter(year >= 2009 & year < 2022) |> distinct(geometry),
      aes(fill = "2009-2021"), alpha = 1) +
    geom_sf(data = df_2022[(!df_2022$geometry %in% df_2009_2021$geometry), ],
      aes(colour = "2022"), linewidth = 0.6, fill = NA) +
    scale_fill_manual(values = grid_colours) +
    scale_colour_manual(values = grid_colours, name = NULL) +
    labs(fill = "Last year sampled") +
    theme(legend.spacing = unit(-85, "pt")) +
    theme(legend.position = c(0.77, 0.75)) +
    theme(axis.title = element_blank()) +
    scale_x_continuous(breaks = seq(-127.4, -126.0, by = 0.4)) +
    geom_sf(data = gfbio_grid, alpha = 0, colour = NA) +
    theme(axis.text = element_text(size = 6),
          legend.text = element_text(size = 6),
          legend.title = element_text(size = 6))

grid_historical_plot
ggsave(filename = file.path(mssm_figs, 'grid-historical-nav-changes_no-points.png'),
  width = 4, height = 3.8)

grid_historical_plot +
  geom_point(data = pcod_dat, aes(x = longitude, y = latitude), shape = 1,
             size = 0.5, alpha = 0.2)
ggsave(file.path(mssm_figs, 'grid-historical-nav-changes_points.png'), width = 4, height = 3.8)

spatial_shift_plot <-
  ggplot() +
    geom_sf(data = mssm_grid_sf |>
        dplyr::filter(year >= 2009 & year <= 2021) |>
        distinct(geometry),
      aes(fill = "2009"), alpha = 0.8, colour = 'grey50') +
    geom_point(data = pcod_dat |>
      filter(year %in% c(1975, 1976, 1977, 1978, 1979, 1985, 1995, 1998, 2003, 2013, 2021, 2022)),
      aes(x = longitude, y = latitude), alpha = 1, size = 1, stroke = 0.5, shape = 21, fill = 'white') +
    scale_fill_manual(values = grid_colours) +
    facet_wrap(~ year, nrow = 2) +
    guides(fill = "none") +
    theme(legend.position = c(0.95, 0.95),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 6))
spatial_shift_plot

ggsave(file.path(mssm_figs, 'grid-spatial-sampling-changes.png'), plot = spatial_shift_plot,
  width = 8, height = 3.8)

ggsave(file.path(mssm_figs, 'grid-spatial-sampling-changes_wide.png'), plot = spatial_shift_plot,
  width = 11, height = 7.2)