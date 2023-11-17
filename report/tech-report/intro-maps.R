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
french <- TRUE

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

alpha <- 95
cols <- c(
      "West Coast Haida Gwaii (WCHG)" = paste0("#E41A1C", alpha),
      "West Coast Vancouver Island (WCVI)" = paste0("#984EA3", alpha),
      "Hecate Strait (HS)" = paste0("#377EB8", alpha),
      "Queen Charlotte Sound (QCS)" = paste0("#4DAF4A", alpha),
      "Outside Hard Bottom Long Line (N)" = paste0("#FF7F00", alpha),
      "Outside Hard Bottom Long Line (S)" = paste0("#FDBF6F", alpha),
      "Inside Hard Bottom Long Line (N & S)" = paste0("#A65628", alpha),
      "Multi-Species Small Mesh (MSSM)" = paste0("#6c6c6c", alpha)
    )

g <- ggplot()

g <- g + geom_polygon(data = ss, aes_string(x = "X", y = "Y", fill = "survey")) +
  scale_fill_manual(values = c(cols))

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
  inherit.aes = FALSE, lwd = 0.1, fill = "grey91", col = "grey72"
) +
  coord_equal(xlim = xlim, ylim = ylim) +
  gfplot::theme_pbs() + labs(fill = "", colour = "", y = en2fr("Northing", french), x = en2fr("Easting", french))

g <- g + theme(legend.justification = c(0, 0), legend.position = c(0, 0))


# HBLL -------------------------------------------------------------------------

hbll_n <- gfplot:::ll2utm(gfplot::hbll_n_grid$grid, utm_zone = 9)
hbll_s <- gfplot:::ll2utm(gfplot::hbll_s_grid$grid, utm_zone = 9)

hbll_n_in <- gfplot:::ll2utm(gfplot::hbll_inside_n_grid$grid, utm_zone = 9)
hbll_s_in <- gfplot:::ll2utm(gfplot::hbll_inside_s_grid$grid, utm_zone = 9)

hbll <- dplyr::bind_rows(
  list(
    data.frame(hbll_n, survey = "Outside Hard Bottom Long Line (N)", stringsAsFactors = FALSE),
    data.frame(hbll_s, survey = "Outside Hard Bottom Long Line (S)", stringsAsFactors = FALSE),
    data.frame(hbll_n_in, survey = "Inside Hard Bottom Long Line (N & S)", stringsAsFactors = FALSE),
    data.frame(hbll_s_in, survey = "Inside Hard Bottom Long Line (N & S)", stringsAsFactors = FALSE)))

g2 <- ggplot()
g2 <- g2 + geom_rect(data = hbll,
  aes_string(xmax = "X + 1", ymax = "Y + 1", xmin = "X - 1", ymin = "Y - 1", fill = "survey")) +
  scale_fill_manual(values = cols) +
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
  inherit.aes = FALSE, lwd = 0.1, fill = "grey91", col = "grey72"
) +
  coord_equal(xlim = xlim, ylim = ylim) +
  gfplot::theme_pbs() + labs(fill = "", colour = "", y = en2fr("Northing", french), x = en2fr("Easting", french))

g2 <- g2 + theme(legend.justification = c(0, 0), legend.position = c(0, 0))

gridExtra::grid.arrange(g, g2, nrow = 1)
