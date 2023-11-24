library(dplyr)
library(ggplot2)
theme_set(gfplot::theme_pbs())

get_indexes <- function(folder, .family = "delta-gamma", model_tag = "st-rw") {
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

df_dg <- get_indexes("report/stitch-cache/mssm-delta-gamma/", "delta-gamma")
df_tw <- get_indexes("report/stitch-cache/mssm-tweedie/", "tweedie")
df_dpg <- get_indexes("report/stitch-cache/mssm-delta-poisson-link-gamma/", "delta-poisson-link-gamma")
df_dpl <- get_indexes("report/stitch-cache/mssm-delta-poisson-link-lognormal/", "delta-poisson-link-lognormal")
df_dl <- get_indexes("report/stitch-cache/mssm-lognormal/", "delta-lognormal")
df <- bind_rows(df_dg, df_tw, df_dpl, df_dpg, df_dl)

df_dg8 <- get_indexes("report/stitch-cache/mssm-delta-gamma/", "delta-gamma-8")
df_dg5 <- get_indexes("report/stitch-cache/mssm-delta-gamma-5/", "delta-gamma-5")
dfc <- bind_rows(df_dg8, df_dg5) |>
  rename(cutoff = family)

ggplot(dfc, aes(year, biomass, ymin = lowerci, ymax = upperci, fill = cutoff)) +
  geom_line(aes(colour = cutoff)) + geom_ribbon(alpha = 0.5) +
  facet_wrap(~species, scales = "free_y")

df_tw8 <- get_indexes("report/stitch-cache/mssm-tweedie/", "delta-tweedie-8")
df_tw5 <- get_indexes("report/stitch-cache/mssm-tweedie-5/", "delta-tweedie-5")
dfc <- bind_rows(df_tw8, df_tw5) |>
  rename(cutoff = family)

ggplot(dfc, aes(year, biomass, ymin = lowerci, ymax = upperci, fill = cutoff)) +
  geom_line(aes(colour = cutoff)) + geom_ribbon(alpha = 0.5) +
  facet_wrap(~species, scales = "free_y")



if (FALSE) {
  df |>
    # filter(family %in% c("delta-gamma", "tweedie")) |>
    group_by(species, family) |>
    summarise(aic = aic[1]) |>
    group_by(species) |>
    View()
}

df |>
  # filter(family %in% c("delta-gamma", "tweedie")) |>
  group_by(species, family) |>
  summarise(aic = aic[1]) |>
  group_by(species) |>
  reframe(delta_aic = aic[family == "delta-gamma"] - aic[family == "tweedie"]) |>
  ggplot(aes(delta_aic, species)) + geom_point() +
  geom_vline(xintercept = 0, lty = 2)

df |>
  group_by(species, family) |>
  summarise(aic = aic[1]) |>
  group_by(species) |>
  reframe(delta_aic = aic[family == "delta-gamma"] - aic[family == "delta-poisson-link-gamma"]) |>
  ggplot(aes(delta_aic, species)) + geom_point() +
  geom_vline(xintercept = 0, lty = 2)

df |>
  group_by(species, family) |>
  summarise(aic = aic[1]) |>
  group_by(species) |>
  reframe(delta_aic = aic[family == "delta-lognormal"] - aic[family == "delta-poisson-link-lognormal"]) |>
  ggplot(aes(delta_aic, species)) + geom_point() +
  geom_vline(xintercept = 0, lty = 2)

f <- df |>
  group_by(species) |>
  filter(year == min(year)) |>
  reframe(lowest_aic = family[aic == min(aic)])

table(f$lowest_aic)

df |>
  group_by(species, family) |>
  summarise(aic = aic[1]) |>
  group_by(species) |>
  mutate(delta_aic = aic - min(aic)) |>
  ggplot(aes(delta_aic + 1, species, colour = family)) + geom_point() +
  scale_x_log10()
  # geom_vline(xintercept = 0, lty = 2)

df |>
  group_by(species, family) |>
  summarise(mean_cv = mean(mean_cv)) |>
  ggplot(aes(mean_cv, species, colour = family)) + geom_point() +
  geom_vline(xintercept = 0, lty = 2)

df |>
  group_by(species, family) |>
  summarise(mean_cv = mean_cv[1], aic = aic[1]) |>
  group_by(species) |>
  mutate(scaled_mean_cv = scale(mean_cv), scaled_aic = scale(aic)) |>
  ggplot(aes(scaled_mean_cv, scaled_aic, colour = family)) + geom_point() +
  geom_vline(xintercept = 0, lty = 2)

ggplot(df, aes(year, biomass, ymin = lowerci, ymax = upperci, fill = family)) +
  geom_line(aes(colour = family)) + geom_ribbon(alpha = 0.5) +
  facet_wrap(~species, scales = "free_y")

f_lu <- df |>
  group_by(species) |>
  filter(year == min(year)) |>
  reframe(family = family[aic == min(aic)])

df |>
  right_join(f_lu) |>
  ggplot(aes(year, biomass, ymin = lowerci, ymax = upperci, fill = family)) +
  geom_line(aes(colour = family)) + geom_ribbon(alpha = 0.5) +
  facet_wrap(~species, scales = "free_y")

lu <- expand.grid(family = unique(df$family), species = unique(df$species))

didnt_converge <- anti_join(lu, df) |> left_join(f)
didnt_converge

