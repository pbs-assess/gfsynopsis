library(dplyr)
library(ggplot2)
theme_set(gfplot::theme_pbs())

get_indexes <- function(folder, .family = "delta_gamma", model_tag = "st-rw") {
  paths <- list.files(folder, pattern = ".rds", full.names = TRUE)
  file_names <- list.files(folder, pattern = ".rds")
  fits <- list.files(paste0(folder, "fits/"), pattern = ".rds", full.names = TRUE)
  sp <- gsub("-", " ", gsub(paste0("_", model_tag, ".rds"), "", file_names))
  df_dg <- purrr::map_dfr(seq_along(paths), function(.x) {
    d <- readRDS(paths[.x])
    if (length(d) > 1L) {
      if (!"aic" %in% names(d)) {
        fit <- tryCatch(readRDS(fits[.x]), error = function(.e) NA)
        if (length(fit)) {
          aic <- tryCatch(AIC(fit), error = function(.e) NA)
        } else {
          aic <- NA
        }
        return(dplyr::mutate(d, species = sp[.x], family = .family, aic = aic))
      } else {
        return(dplyr::mutate(d, species = sp[.x], family = .family))
      }
    }
  })
}

df_dg <- get_indexes("report/stitch-cache/synoptic-delta-gamma/", "delta-gamma")
df_tw <- get_indexes("report/stitch-cache/synoptic/", "tweedie")
df <- bind_rows(df_dg, df_tw)

df |>
  filter(!is.na(aic)) |>
  group_by(species, family) |>
  summarise(aic = aic[1]) |>
  group_by(species) |>
  summarise(delta_aic = aic[family == "delta-gamma"] - aic[family == "tweedie"]) |>
  ggplot(aes(delta_aic, species)) + geom_point() +
  geom_vline(xintercept = 0, lty = 2)

ggplot(df, aes(year, biomass, ymin = lowerci, ymax = upperci, fill = family)) +
  geom_line(aes(colour = family)) + geom_ribbon(alpha = 0.5) +
  facet_wrap(~species, scales = "free_y")

