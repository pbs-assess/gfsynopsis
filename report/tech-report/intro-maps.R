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
  dplyr::filter(active_block == TRUE) |>
  dplyr::left_join(survey_lookup, by = c("survey_abbrev" = "survey_abbrev")) |>
  sf::st_transform(crs = 32609) |>
  dplyr::group_by(survey_abbrev, label) |>
  dplyr::summarise(geometry = sf::st_union(geometry))

syn_grid <- dplyr::filter(sb, survey_abbrev %in% c("SYN WCHG", "SYN WCVI", "SYN HS", "SYN QCS")) |>
  dplyr::mutate(label = factor(label, levels = c("Hecate Strait (HS)", "West Coast Haida Gwaii (WCHG)", "Queen Charlotte Sound (QCS)", "West Coast Vancouver Island (WCVI)")))
hbll_grid <- dplyr::filter(sb, survey_abbrev %in% c("HBLL OUT N", "HBLL OUT S", "HBLL INS N", "HBLL INS S")) |>
  dplyr::mutate(label = factor(label, levels = c("Outside Hard Bottom Longline (N)", "Outside Hard Bottom Longline (S)", "Inside Hard Bottom Longline (N & S)")))

data("isobath", package = "PBSdata")

isobath_sf <- isobath |>
  as.data.frame() |>
  dplyr::filter(PID %in% c(100, 200, 500, 1800)) |>
  sf::st_as_sf(coords = c('X', 'Y'), crs = 4326) |>
  dplyr::group_by(PID, SID) |>
  dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
  sf::st_cast("LINESTRING") |>
  sf::st_transform(crs = 32609)

coast <- pacea::bc_coast |> sf::st_transform(crs = 32609)

xlim <- sf::st_bbox(sb)[c("xmin", "xmax")]
ylim <- sf::st_bbox(sb)[c("ymin", "ymax")]

g <- ggplot() +
  gfplot::theme_pbs() +
  ggplot2::geom_sf(data = coast, fill = "grey91", colour = "grey72", linewidth = 0.1) +
  ggplot2::scale_fill_manual(values = cols) +
  ggplot2::scale_colour_manual(values = cols) +
  ggplot2::scale_x_continuous(
    breaks = scales::breaks_pretty(n = 3)(xlim),
    labels = scales::label_number(scale = 1e-3, big.mark = "")
    ) +
  ggplot2::scale_y_continuous(
    breaks = scales::breaks_pretty(n = 4)(ylim),
    labels = scales::label_number(scale = 1e-3, big.mark = "")
    ) +
  ggplot2::labs(fill = "", colour = "", y = en2fr("Northing", french), x = en2fr("Easting", french)) +
  ggplot2::theme(legend.justification = c(0, 0),
        legend.position = "inside",
        legend.position.inside = c(-0.01, -0.01))

g1 <- g +
  ggplot2::geom_sf(data = syn_grid,
    aes(fill = label), linewidth = 0, alpha = 0.7) +
  ggplot2::geom_sf(data = isobath_sf, colour = "grey70", alpha = 0.4) +
  ggplot2::coord_sf(xlim = xlim, ylim = ylim, datum=sf::st_crs(sb), expand = FALSE)

g2 <- g +
  ggplot2::geom_sf(data = hbll_grid,
    aes(fill = label), linewidth = 0, alpha = 0.7) +
  ggplot2::geom_sf(data = isobath_sf, colour = "grey70", alpha = 0.4) +
  ggplot2::coord_sf(xlim = xlim, ylim = ylim, datum=sf::st_crs(sb), expand = FALSE)

if (!is.null(shapefile)) {
  g1 <- g1 +
    ggplot2::geom_sf(data = shapefile, fill = NA, colour = "black", linewidth = 0.5) +
    ggplot2::geom_rect(aes(xmin = xlim[1], xmax = 300000, ymin = ylim[1], ymax = 5470000),
      fill = "white", alpha = 0.9) +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, datum=sf::st_crs(sb), expand = FALSE)

  g2 <- g2 +
    ggplot2::geom_sf(data = shapefile, fill = NA, colour = "black", linewidth = 0.5) +
    ggplot2::geom_rect(aes(xmin = xlim[1], xmax = 300000, ymin = ylim[1], ymax = 5440000),
      fill = "white", alpha = 0.9) +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, datum=sf::st_crs(sb), expand = FALSE)
}

gridExtra::grid.arrange(g1, g2, nrow = 1)

