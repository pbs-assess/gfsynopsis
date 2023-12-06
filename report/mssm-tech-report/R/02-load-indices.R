if (!('mssm_loaded' %in% ls())) {
  source(here::here('report', 'mssm-tech-report', 'R', '00-load.R'))
}

# Load index dataframes ----

# MSSM design index
mssm_d_inds <- spp_vector |>
  map(\(sp) readRDS(file.path(data_cache, paste0(gfsynopsis:::clean_name(sp), '.rds')))$survey_index) |>
  setNames(spp_vector) |>
  bind_rows(.id = 'species') |>
  as_tibble() |>
  filter(survey_abbrev == 'MSSM WCVI') |>
  mutate(survey_abbrev = "MSSM Design")


# Load SYN WCVI and MSSM best models
get_index <- function(folder, spp, .family = "", model_tag = "st-rw") {
  paths <- list.files(folder, pattern = ".rds", full.names = TRUE)
  path <- paths[grepl(spp, paths)]
  if (length(path) > 1) {
    path <- path[grepl(model_tag, path)]
  }
  if (length(path)) {
    file_names <- list.files(folder, pattern = ".rds")
    sp <- gsub("-", " ", spp)
    if (file.exists(path)) {
      # if (grepl("QCS", path)) browser()
      d <- readRDS(path)
      if (length(d) > 1L) {
        return(dplyr::mutate(d, species = sp, family = .family))
      }
    }
  }
}

families <- c(
  "delta-gamma",
  "delta-poisson-link-gamma",
  "tweedie",
  "delta-poisson-link-lognormal",
  "delta-lognormal"
)
take_min_aic <- function(x) {
  if (nrow(x)) {
    filter(x, aic == min(aic))
  }
}

syn_wcvi <- tidyr::expand_grid(.s = gfsynopsis::clean_name(spp_vector), f = families) |>
  purrr::pmap_dfr(function(.s, f) {
  get_index(paste0("report/stitch-cache/synoptic-SYN WCVI-", f, "/"), .s, .family = f)
}) |> group_by(species) |>
  take_min_aic()

syn_wcvi$species <- gsub("rougheye blackspotted", "rougheye/blackspotted", syn_wcvi$species)

syn_mssm_grid_inds <- syn_wcvi |>
  select(.sp = species, .family = family) |>
  distinct() |>
  purrr::pmap(\(.sp, .family) {
    get_index(paste0("report/stitch-cache/synoptic-mssm-SYN WCVI-", .family, "/"), clean_name(.sp), .family = .family)
  }) |>
  bind_rows() |>
  as_tibble()

mssm_inds <- tidyr::expand_grid(.s = gfsynopsis::clean_name(spp_vector), f = families) |>
  purrr::pmap_dfr(function(.s, f) {
  get_index(paste0("report/stitch-cache/mssm-", f, "/"), .s, .family = f)
}) |> group_by(species) |>
  take_min_aic()

# Old tweedie MSSM indexes comparing 2km and 3km
mssm_2km_inds <- spp_vector |>
  map(\(sp) readRDS(file.path(mssm_sc, '2km-grid', paste0(gfsynopsis:::clean_name(sp), '_st-rw.rds')))) |>
  setNames(spp_vector) |>
  keep(\(x) inherits(x, 'data.frame')) |>
  bind_rows(.id = 'species') |>
  as_tibble() |>
  mutate(year_bins = "~ 1") |>
  mutate(grid = '2km') |>
  group_by(species) |>
  mutate(extreme_uci = max(upperci) > 10 * max(biomass)) |>
  ungroup()

mssm_3km_inds <- spp_vector |>
  map(\(sp) readRDS(file.path(mssm_sc, '3km-grid', paste0(gfsynopsis:::clean_name(sp), '_st-rw.rds')))) |>
  setNames(spp_vector) |>
  keep(\(x) inherits(x, 'data.frame')) |>
  bind_rows(.id = 'species') |>
  as_tibble() |>
  mutate(grid = "3km") |>
  group_by(species) |>
  mutate(extreme_uci = max(upperci) > 10 * max(biomass)) |>
  ungroup()

# SYN WCVI index
syn_inds <-
  spp_vector |>
    map(\(sp) readRDS(file.path(syn_sc, 'syn-wcvi-grid', paste0(gfsynopsis:::clean_name(sp), '_st-rw.rds')))) |>
    setNames(spp_vector) |>
    keep(\(x) inherits(x, 'data.frame')) |>
    bind_rows(.id = 'species') |>
    mutate(grid = 'SYN WCVI') |>
    as_tibble()

syn_mssm_grid_inds <-
  spp_vector |>
    map(\(sp) readRDS(file.path(syn_sc, 'mssm-grid-3km', paste0(gfsynopsis:::clean_name(sp), '_st-rw.rds')))) |>
    setNames(spp_vector) |>
    keep(\(x) inherits(x, 'data.frame')) |>
    bind_rows(.id = 'species') |>
    as_tibble() |>
    mutate(survey_abbrev = 'SYN WCVI on MSSM Grid') |>
    mutate(grid = "3km MSSM")

# --- CPUE index ---
cpue_ind <- spp_vector |>
  map(\(sp) readRDS(file.path(cpue_cache, paste0(gfsynopsis:::clean_name(sp), '.rds')))) |>
  setNames(spp_vector) |>
  keep(\(x) inherits(x, 'data.frame')) |>
  bind_rows(.id = 'species') |>
  filter(area == '3CD') |>
  rename(biomass = est, lowerci = lwr, upperci = upr) |>
  mutate(survey_abbrev = 'CPUE 3CD')

# --- Scale indices ---
# Get overlapping years to scale based on geometric means of indexes
syn_years <- unique(sw_dat$year)
mssm_years <- unique(mssm_dat$year)
cpue_years <- unique(cpue_ind$year)

#spp_in_mssm <- unique(mssm_3km_inds$species)
spp_in_mssm <- mssm_3km_inds |>
  filter(!extreme_uci | is.na(extreme_uci)) |>
  filter(mean_cv < 4 | is.na(mean_cv)) |>
  distinct(species) |>
  pluck('species')

spp_in_mssm_design <- mssm_d_inds |>
  count(species) |>
  filter(n > 2) |>
  distinct(species) |>
  pluck('species')

spp_in_mssm_design_only <- setdiff(spp_in_mssm_design, spp_in_mssm)

syn_mssm_overlap <- intersect(syn_years, mssm_years)
cpue_mssm_overlap <- intersect(cpue_years, mssm_years)

# MSSM with SYN WCVI - Use 3km grid
inds <- bind_rows(mssm_3km_inds, syn_inds, syn_mssm_grid_inds, cpue_ind, mssm_d_inds) |>
  mutate(syn_overlap = ifelse(year %in% syn_mssm_overlap, TRUE, FALSE),
         cpue_overlap = ifelse(year %in% cpue_mssm_overlap, TRUE, FALSE))

mssm_geomeans <- inds |>
  filter(survey_abbrev %in% c("MSSM WCVI", "MSSM Design")) |>
  group_by(species, survey_abbrev) |>
  summarise(mssm_geomean = exp(mean(log(biomass))), .groups = 'drop')

syn_overlap_geomeans <- inds |>
  filter(syn_overlap, survey_abbrev %in% c("MSSM WCVI", "SYN WCVI", "SYN WCVI on MSSM Grid")) |>
  group_by(species, survey_abbrev) |>
  summarise(syn_overlap_geomean = exp(mean(log(biomass))), .groups = 'drop')

cpue_overlap_geomeans <- inds |>
  filter(cpue_overlap, survey_abbrev %in% c("MSSM WCVI", "CPUE 3CD")) |>
  group_by(species, survey_abbrev) |>
  summarise(cpue_overlap_geomean = exp(mean(log(biomass))), .groups = 'drop')

scaled_inds <- left_join(inds, syn_overlap_geomeans) |>
  left_join(cpue_overlap_geomeans) |>
  left_join(mssm_geomeans) |>
  mutate(syn_scaled_biomass = biomass / syn_overlap_geomean,
         syn_scaled_lowerci = lowerci / syn_overlap_geomean,
         syn_scaled_upperci = upperci / syn_overlap_geomean,
         cpue_scaled_biomass = biomass / cpue_overlap_geomean,
         cpue_scaled_lowerci = lowerci / cpue_overlap_geomean,
         cpue_scaled_upperci = upperci / cpue_overlap_geomean,
         mssm_scaled_biomass = biomass / mssm_geomean,
         mssm_scaled_lowerci = lowerci / mssm_geomean,
         mssm_scaled_upperci = upperci / mssm_geomean,
       ) |>
  mutate(survey_abbrev = gsub("MSSM WCVI", "MSSM Model", survey_abbrev)) |>
  order_spp()

indices_loaded <- TRUE


