library(dplyr)
library(ggplot2)
theme_set(gfplot::theme_pbs())

spp_vector  <- gfsynopsis::get_spp_names()$species_common_name
data_cache <- here::here('report', 'data-cache-oct-2023')
stitch_cache <- here::here('report', 'stitch-cache')

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
