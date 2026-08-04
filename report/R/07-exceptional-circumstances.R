# check exceptional circumstances where reported

# Dover Sole:
# 50% change (decline?) in the 3-year running mean compared to the 2024 value of
# the coastwide synoptic index:

d2025 <- readRDS("report/cache-main/stitch-cache/min_aic/dover-sole.rds")
d2025 <- dplyr::filter(d2025, survey_abbrev == "synoptic")
x <- d2025$biomass
y <- zoo::rollmean(x, k = 3, fill = NA, align = "right")
d2025$rolling_mean <- y
ggplot(d2025, aes(year, biomass)) + geom_line(aes(y = rolling_mean)) + geom_point() +
  geom_vline(xintercept = 2024) + geom_hline(yintercept = x[d2025$year == 2024] * 0.5, lty = 2) +
  coord_cartesian(ylim = c(0, NA))
thresh <- round(y[d2025$year == 2025] / x[d2025$year == 2024], 2)

