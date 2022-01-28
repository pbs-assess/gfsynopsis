xlim <- c(122, 890)
ylim <- c(5373, 6027)
bath <- c(100, 200, 500)
utm_zone <- 9
ll_range <- gfplot:::utm2ll(cbind(X = xlim, Y = ylim), utm_zone = 9)
coastline_utm <- gfplot:::load_coastline(
  xlim_ll = ll_range[, "X"] + c(-5, 5),
  ylim_ll = ll_range[, "Y"] + c(-5, 5),
  utm_zone = utm_zone
)
isobath_utm <- gfplot:::load_isobath(
  xlim_ll = ll_range[, "X"] + c(-5, 5),
  ylim_ll = ll_range[, "Y"] + c(-12, 12),
  bath = bath, utm_zone = utm_zone
)

# synoptic surveys -------------------------------------------------------------

hs_utm <- gfplot:::ll2utm(gfplot::survey_boundaries$HS, utm_zone = 9)
qcs_utm <- gfplot:::ll2utm(gfplot::survey_boundaries$QCS, utm_zone = 9)
wcvi_utm <- gfplot:::ll2utm(gfplot::survey_boundaries$WCVI, utm_zone = 9)
wchg_utm <- gfplot:::ll2utm(gfplot::survey_boundaries$WCHG, utm_zone = 9)
ss <- dplyr::bind_rows(
  list(data.frame(hs_utm, survey = "Hecate Strait (HS)", stringsAsFactors = FALSE),
    data.frame(qcs_utm, survey = "Queen Charlotte Sound (QCS)", stringsAsFactors = FALSE),
    data.frame(wcvi_utm, survey = "West Coast Vancouver Island (WCVI)", stringsAsFactors = FALSE),
    data.frame(wchg_utm, survey = "West Coast Haida Gwaii (WCHG)", stringsAsFactors = FALSE)))

g <- ggplot()

cols <- paste0(c(RColorBrewer::brewer.pal(5L, "Set1"),
  RColorBrewer::brewer.pal(8L, "Set1")[7:8],
  "#303030", "#a8a8a8", "#a8a8a8", "#a8a8a8"), "80")
g <- g + geom_polygon(data = ss, aes_string(x = "X", y = "Y", fill = "survey")) +
  scale_fill_manual(values = c(
    "Hecate Strait (HS)" = cols[2],
    "Queen Charlotte Sound (QCS)" = cols[3],
    "West Coast Vancouver Island (WCVI)" = cols[4],
    "West Coast Haida Gwaii (WCHG)" = cols[1]))

g <- g + geom_path(
  data = isobath_utm, aes_string(
    x = "X", y = "Y",
    group = "paste(PID, SID)"
  ),
  inherit.aes = FALSE, lwd = 0.4, col = "grey70", alpha = 0.4
)
g <- g + geom_polygon(
  data = coastline_utm,
  aes_string(x = "X", y = "Y", group = "PID"),
  inherit.aes = FALSE, lwd = 0.2, fill = "grey87", col = "grey70"
) +
  coord_equal(xlim = xlim, ylim = ylim) +
  theme_pbs() + labs(fill = "", colour = "", y = en2fr("Northing", french), x = en2fr("Easting", french))

g <- g + theme(legend.justification = c(0, 0), legend.position = c(0, 0))


# HBLL -------------------------------------------------------------------------

hbll_n <- gfplot:::ll2utm(gfplot::hbll_n_grid$grid, utm_zone = 9)
hbll_s <- gfplot:::ll2utm(gfplot::hbll_s_grid$grid, utm_zone = 9)

hbll <- dplyr::bind_rows(
  list(data.frame(hbll_n, survey = "Outside Hard Bottom Long Line (N)", stringsAsFactors = FALSE),
    data.frame(hbll_s, survey = "Outside Hard Bottom Long Line (S)", stringsAsFactors = FALSE)))

g2 <- ggplot()
g2 <- g2 + geom_rect(data = hbll,
  aes_string(xmax = "X + 1", ymax = "Y + 1", xmin = "X - 1", ymin = "Y - 1", fill = "survey")) +
  scale_fill_manual(values = c(
    "Outside Hard Bottom Long Line (N)" = cols[5],
    "Outside Hard Bottom Long Line (S)" = cols[6])) +
  geom_path(
    data = isobath_utm, aes_string(
      x = "X", y = "Y",
      group = "paste(PID, SID)"
    ),
    inherit.aes = FALSE, lwd = 0.4, col = "grey70", alpha = 0.4
  )
g2 <- g2 + geom_polygon(
  data = coastline_utm,
  aes_string(x = "X", y = "Y", group = "PID"),
  inherit.aes = FALSE, lwd = 0.2, fill = "grey87", col = "grey70"
) +
  coord_equal(xlim = xlim, ylim = ylim) +
  theme_pbs() + labs(fill = "", colour = "", y = en2fr("Northing", french), x = en2fr("Easting", french))

g2 <- g2 + theme(legend.justification = c(0, 0), legend.position = c(0, 0))

gridExtra::grid.arrange(g, g2, nrow = 1)
