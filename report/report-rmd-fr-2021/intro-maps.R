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

#   1. Relevé synoptique, bassin de la Reine-Charlotte (SYN BRC) : @williams2018synqcs
#
# 1. Relevé synoptique, côte ouest de l’île de Vancouver (SYN COIV) : @nottingham2017synwcvi
#
# 1. Relevé synoptique, détroit d’Hécate (SYN DH) : @wyeth2018synhs
#
# 1. Relevé synoptique, côte ouest de Haida Gwaii (SYN COHG) : @williams2018synwchg
#
# 1. Relevé à la palangre sur fond dur dans les eaux extérieures (RPFD EXT) : @doherty2019hbllout
#
# 1. Relevé à la palangre sur fond dur dans les eaux intérieures (RPFD INT) : @lochead2007irf
#
# 1. Relevé de la communauté d’espèces dans le détroit d’Hécate (RCE DH) : @choromanski2004hsmsa
#
# 1. Relevé indépendant de la pêche de la Commission internationale du flétan du Pacifique (RIP CIFP)

hs_utm <- gfplot:::ll2utm(gfplot::survey_boundaries$HS, utm_zone = 9)
qcs_utm <- gfplot:::ll2utm(gfplot::survey_boundaries$QCS, utm_zone = 9)
wcvi_utm <- gfplot:::ll2utm(gfplot::survey_boundaries$WCVI, utm_zone = 9)
wchg_utm <- gfplot:::ll2utm(gfplot::survey_boundaries$WCHG, utm_zone = 9)
ss <- dplyr::bind_rows(
  list(data.frame(hs_utm, survey = "SYN HS", stringsAsFactors = FALSE),
    data.frame(qcs_utm, survey = "SYN QCS", stringsAsFactors = FALSE),
    data.frame(wcvi_utm, survey = "SYN WCVI", stringsAsFactors = FALSE),
    data.frame(wchg_utm, survey = "SYN WCHG", stringsAsFactors = FALSE)))

g <- ggplot()

cols <- paste0(c(RColorBrewer::brewer.pal(5L, "Set1"),
  RColorBrewer::brewer.pal(8L, "Set1")[7:8],
  # "#303030", "#a8a8a8", "#a8a8a8", "#a8a8a8"), "80")
  "#303030", "#0a0a0a", "#0a0a0a", "#0a0a0a"), "95")
  # "#60b6bb", "#60b6bb", "#1d989e", "#a8a8a8"), "80")

g <- g + geom_polygon(data = ss, aes_string(x = "X", y = "Y", fill = "survey")) +
  scale_fill_manual(values = c(
    "SYN HS" = cols[2],
    "SYN QCS" = cols[3],
    "SYN WCVI" = cols[4],
    "SYN WCHG" = cols[1]))

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
  theme_pbs() + labs(fill = "", colour = "", y = en2fr("Northing", french), x = en2fr("Easting", french))

g <- g + theme(legend.justification = c(0, 0), legend.position = c(0, 0))


# HBLL -------------------------------------------------------------------------

hbll_n <- gfplot:::ll2utm(gfplot::hbll_n_grid$grid, utm_zone = 9)
hbll_s <- gfplot:::ll2utm(gfplot::hbll_s_grid$grid, utm_zone = 9)

hbll_n_in <- gfplot:::ll2utm(gfplot::hbll_inside_n_grid$grid, utm_zone = 9)
hbll_s_in <- gfplot:::ll2utm(gfplot::hbll_inside_s_grid$grid, utm_zone = 9)

hbll <- dplyr::bind_rows(
  list(
    data.frame(hbll_n, survey = "HBLL OUT (N)", stringsAsFactors = FALSE),
    data.frame(hbll_s, survey = "HBLL OUT (S)", stringsAsFactors = FALSE),
    data.frame(hbll_n_in, survey = "HBLL INS (N & S)", stringsAsFactors = FALSE),
    data.frame(hbll_s_in, survey = "HBLL INS (N & S)", stringsAsFactors = FALSE)))

g2 <- ggplot()
g2 <- g2 + geom_rect(data = hbll,
  aes_string(xmax = "X + 1", ymax = "Y + 1", xmin = "X - 1", ymin = "Y - 1", fill = "survey")) +
  scale_fill_manual(values = c(
    "HBLL OUT (N)" = cols[5],
    "HBLL OUT (S)" = cols[6],
    # "Inside Hard Bottom Long Line (N)" = cols[9],
    "HBLL INS (N & S)" = cols[10]
    )) +
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
  theme_pbs() + labs(fill = "", colour = "", y = en2fr("Northing", french), x = en2fr("Easting", french))

g2 <- g2 + theme(legend.justification = c(0, 0), legend.position = c(0, 0))

g <- g +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_x_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE))

g2 <- g2 +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_x_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE))

gridExtra::grid.arrange(g, g2, nrow = 1)
