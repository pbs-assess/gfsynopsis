survey_lookup <- data.frame(
  label = c(
    "West Coast Haida Gwaii (WCHG)",
    "West Coast Vancouver Island (WCVI)",
    "Hecate Strait (HS)",
    "Queen Charlotte Sound (QCS)",
    "Outside Hard Bottom Longline (N)",
    "Outside Hard Bottom Longline (S)",
    "Inside Hard Bottom Longline (N & S)",
    "Inside Hard Bottom Longline (N & S)",
    "Multi-Species Small Mesh (MSSM WCVI)"
  ),
  survey_abbrev = c(
    "SYN WCHG",
    "SYN WCVI",
    "SYN HS",
    "SYN QCS",
    "HBLL OUT N",
    "HBLL OUT S",
    "HBLL INS N",
    "HBLL INS S",
    "MSSM WCVI"
  ),
  stringsAsFactors = FALSE
)

cols <- c(
      "West Coast Haida Gwaii (WCHG)" = "#E41A1C",
      "Hecate Strait (HS)" = "#377EB8",
      "Queen Charlotte Sound (QCS)" = "#4DAF4A",
      "West Coast Vancouver Island (WCVI)" = "#984EA3",
      "Outside Hard Bottom Longline (N)" = "#FF7F00",
      "Outside Hard Bottom Longline (S)" = "#FDBF6F",
      "Inside Hard Bottom Longline (N & S)" = "#A65628",
      "Multi-Species Small Mesh (MSSM)" = "#6c6c6c"
    )

sb <- gfdata::survey_blocks |>
  filter(active_block == TRUE) |>
  left_join(survey_lookup, by = c("survey_abbrev" = "survey_abbrev")) |>
  sf::st_transform(crs = 32609) |>
  group_by(survey_abbrev, label) |>
  summarise(geometry = sf::st_union(geometry))

syn_grid <- filter(sb, survey_abbrev %in% c("SYN WCHG", "SYN WCVI", "SYN HS", "SYN QCS")) |>
  mutate(label = factor(label, levels = c("Hecate Strait (HS)", "West Coast Haida Gwaii (WCHG)", "Queen Charlotte Sound (QCS)", "West Coast Vancouver Island (WCVI)")))
hbll_grid <- filter(sb, survey_abbrev %in% c("HBLL OUT N", "HBLL OUT S", "HBLL INS N", "HBLL INS S")) |>
  mutate(label = factor(label, levels = c("Outside Hard Bottom Longline (N)", "Outside Hard Bottom Longline (S)", "Inside Hard Bottom Longline (N & S)")))

data("isobath", package = "PBSdata")

isobath_sf <- isobath |>
  as.data.frame() |>
  filter(PID %in% c(100, 200, 500, 1800)) |>
  sf::st_as_sf(coords = c('X', 'Y'), crs = 4326) |>
  group_by(PID, SID) |>
  summarise(geometry = sf::st_combine(geometry)) %>%
  sf::st_cast("LINESTRING") |>
  sf::st_transform(crs = 32609)

coast <- pacea::bc_coast

xlim <- sf::st_bbox(sb)[c("xmin", "xmax")]
ylim <- sf::st_bbox(sb)[c("ymin", "ymax")]

if (!is.null(shapefile)) {
  y_max_buffer <- 10000
  shape_bb <- sf::st_bbox(shapefile)
  xlim_shp <- shape_bb[c("xmin", "xmax")]
  ylim_shp <- shape_bb[c("ymin", "ymax")]

  if (ylim["ymax"] < ylim_shp["ymax"]) {
    ylim["ymax"] <- ylim_shp["ymax"] + y_max_buffer
  }
}

albers_bbox_sfc <- sf::st_as_sfc(sf::st_bbox(c(xlim, ylim), crs = sf::st_crs(sb)))
wgs84_bbox <- sf::st_bbox(sf::st_transform(albers_bbox_sfc, crs = 4326))
xlim <- wgs84_bbox[c("xmin", "xmax")]
ylim <- wgs84_bbox[c("ymin", "ymax")]

g <- ggplot() +
  gfplot::theme_pbs() +
  ggplot2::geom_sf(data = coast, fill = "grey91", colour = "grey72", linewidth = 0.1) +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(n = 4)(xlim)
    ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 4)(ylim)
    ) +
  labs(fill = "", colour = "", y = en2fr("Latitude", french), x = en2fr("Longitude", french)) +
  theme(legend.justification = c(0, 0),
        legend.position = "inside",
        legend.position.inside = c(-0.01, -0.01))

g1 <- g +
  ggplot2::geom_sf(data = syn_grid,
    aes(fill = label), linewidth = 0, alpha = 0.7) +
  ggplot2::geom_sf(data = isobath_sf, colour = "grey70", alpha = 0.4) +
  ggplot2::coord_sf(xlim = xlim, ylim = ylim, datum = 4326, expand = FALSE)

g2 <- g +
  ggplot2::geom_sf(data = hbll_grid,
    aes(fill = label), linewidth = 0, alpha = 0.7) +
  ggplot2::geom_sf(data = isobath_sf, colour = "grey70", alpha = 0.4) +
  ggplot2::coord_sf(xlim = xlim, ylim = ylim, datum = 4326, expand = FALSE)

if (!is.null(shapefile)) {
  g1 <- g1 +
    ggplot2::geom_sf(data = shapefile, fill = NA, colour = "black", linewidth = 0.5) +
    ggplot2::geom_rect(aes(xmin = xlim[1], xmax = -130, ymin = ylim[1], ymax = 50),
      fill = "white", alpha = 0.9) +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, datum = 4326, expand = FALSE)

  g2 <- g2 +
    ggplot2::geom_sf(data = shapefile, fill = NA, colour = "black", linewidth = 0.5) +
    ggplot2::geom_rect(aes(xmin = xlim[1], xmax = -130, ymin = ylim[1], ymax = 50),
      fill = "white", alpha = 0.9) +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, datum = 4326, expand = FALSE)
}

gridExtra::grid.arrange(g1, g2, nrow = 1)

