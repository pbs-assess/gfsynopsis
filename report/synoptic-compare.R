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

df_dg <- get_indexes("report/stitch-cache/synoptic-delta-gamma/", "delta-gamma")
df_tw <- get_indexes("report/stitch-cache/synoptic-tweedie/", "tweedie")
df_dpg <- get_indexes("report/stitch-cache/synoptic-delta-poisson-link-gamma/", "delta-poisson-link-gamma")
df_dpl <- get_indexes("report/stitch-cache/synoptic-delta-poisson-link-lognormal/", "delta-poisson-link-lognormal")
df_dl <- get_indexes("report/stitch-cache/synoptic-delta-lognormal/", "delta-lognormal")
df <- bind_rows(df_dg, df_tw, df_dpl, df_dpg, df_dl)

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

# ----- Compare with design-based index ----
spp_vector  <- gfsynopsis::get_spp_names()$species_common_name
data_cache <- here::here('report', 'data-cache-nov-2023')
stitch_cache <- here::here('report', 'stitch-cache')

syn_regions <- c('SYN WCHG', 'SYN HS', 'SYN QCS', 'SYN WCVI')
folders <- file.path(stitch_cache, paste0('synoptic-', syn_regions, '-delta-gamma'))

dg_syn <- folders |>
  purrr::map(\(folder) get_indexes(folder, 'delta-gamma')) |>
  bind_rows() |>
  as_tibble() |>
  mutate(index_type = "Model")

# SYN design index
syn_design <- spp_vector |>
  purrr::map(\(sp) readRDS(file.path(data_cache, paste0(gfsynopsis:::clean_name(sp), '.rds')))$survey_index) |>
  setNames(spp_vector) |>
  bind_rows(.id = 'species') |>
  as_tibble() |>
  filter(stringr::str_detect(survey_abbrev, 'SYN')) |>
  mutate(index_type = "Design")

# Combine and scale indexes for comparison
df <- bind_rows(syn_design, dg_syn) |>
  group_by(survey_abbrev, index_type, species) |>
  mutate(geomean = exp(mean(log(biomass))),
         scaled_biomass = biomass / geomean,
         scaled_lowerci = lowerci / geomean,
         scaled_upperci = upperci / geomean) |>
  ungroup()

df |>
  filter(survey_abbrev == 'SYN WCHG') |>
  group_by(species) |>
  filter(n() > 1) |>
  group_by() |>
ggplot(aes(x = year, y = scaled_biomass,
  ymin = scaled_lowerci, ymax = scaled_upperci, fill = index_type)) +
  geom_line(aes(colour = index_type)) + geom_ribbon(alpha = 0.5) +
  facet_wrap(~species, scales = "free_y")

df |>
  filter(survey_abbrev == 'SYN HS') |>
  group_by(species) |>
  filter(n() > 1) |>
  group_by() |>
ggplot(aes(x = year, y = scaled_biomass,
  ymin = scaled_lowerci, ymax = scaled_upperci, fill = index_type)) +
  geom_line(aes(colour = index_type)) + geom_ribbon(alpha = 0.5) +
  facet_wrap(~species, scales = "free_y")


df |>
  filter(survey_abbrev == 'SYN QCS') |>
  group_by(species) |>
  filter(n() > 1) |>
  group_by() |>
ggplot(aes(x = year, y = scaled_biomass,
  ymin = scaled_lowerci, ymax = scaled_upperci, fill = index_type)) +
  geom_line(aes(colour = index_type)) + geom_ribbon(alpha = 0.5) +
  facet_wrap(~species, scales = "free_y")

df |>
  filter(survey_abbrev == 'SYN WCVI') |>
  group_by(species) |>
  filter(n() > 1) |>
  group_by() |>
ggplot(aes(x = year, y = scaled_biomass,
  ymin = scaled_lowerci, ymax = scaled_upperci, fill = index_type)) +
  geom_line(aes(colour = index_type)) + geom_ribbon(alpha = 0.5) +
  facet_wrap(~species, scales = "free_y")
