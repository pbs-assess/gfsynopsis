f1 <- list.files("report/stitch-cache/synoptic-delta-gamma/", pattern = ".rds", full.names = TRUE)
f1b <- list.files("report/stitch-cache/synoptic-delta-gamma/", pattern = ".rds")
sp <- gsub("-", " ", gsub("_st-rw.rds", "", f1b))
df_dg <- purrr::map_dfr(seq_along(f1), function(.x) {
  d <- readRDS(f1[.x])
  if (length(d) > 1L) {
    return(dplyr::mutate(d, species = sp[.x], family = "delta-gamma"))
  }
})

f1 <- list.files("report/stitch-cache/synoptic/", pattern = ".rds", full.names = TRUE)
df_tw <- purrr::map_dfr(seq_along(f1), function(.x) {
  d <- readRDS(f1[.x])
  if (length(d) > 1L) {
    return(dplyr::mutate(d, species = sp[.x], family = "tweedie"))
  }
})

library(dplyr)
library(ggplot2)
theme_set(gfplot::theme_pbs())

df <- bind_rows(df_dg, df_tw)

ggplot(df, aes(year, biomass, ymin = lowerci, ymax = upperci, fill = family)) + geom_line(aes(colour = family)) + geom_ribbon(alpha = 0.5) +
  facet_wrap(~species, scales = "free_y")
