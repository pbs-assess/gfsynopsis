if (!('mssm_loaded' %in% ls())) {
  source(here::here('report', 'mssm-tech-report', 'R', '00-load.R'))
}

# Load index dataframes ----

# MSSM design index
if (!file.exists(file.path(mssm_data, 'mssm-design-inds.rds'))) {
mssm_d_inds <- spp_vector |>
  map(\(sp) readRDS(file.path(data_cache, paste0(gfsynopsis:::clean_name(sp), '.rds')))$survey_index) |>
  setNames(spp_vector) |>
  bind_rows(.id = 'species') |>
  as_tibble() |>
  filter(survey_abbrev == 'MSSM WCVI') |>
  mutate(survey_abbrev = "MSSM Design")
  saveRDS(mssm_d_inds, file = file.path(mssm_data, 'mssm-design-inds.rds'))
} else {
  mssm_d_inds <- readRDS(file.path(mssm_data, 'mssm-design-inds.rds')) |>
    group_by(species) |>
    mutate(mean_cv = mean(re, na.rm = TRUE)) |>
    ungroup()
}

# 2023 data
# mssm_d_inds <- readRDS(file.path(mssm_data, 'mssm-index-dat.rds')) |>
#     rename(species = 'species_common_name') |>
#     mutate(survey_abbrev = "MSSM Design") |>
#     group_by(species) |>
#     mutate(mean_cv = mean(re, na.rm = TRUE)) |>
#     ungroup()

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

syn_wcvi_inds <- tidyr::expand_grid(.s = gfsynopsis::clean_name(spp_vector), f = families) |>
  purrr::pmap_dfr(function(.s, f) {
  get_index(paste0("report/stitch-cache/synoptic-SYN WCVI-", f, "/"), .s, .family = f)
}) |> group_by(species) |>
  take_min_aic() |>
  ungroup() |>
  mutate(grid = 'SYN WCVI')

syn_wcvi_inds$species <- gsub("rougheye blackspotted", "rougheye/blackspotted", syn_wcvi_inds$species)

syn_mssm_grid_inds <- syn_wcvi_inds |>
  select(.sp = species, .family = family) |>
  distinct() |>
  purrr::pmap(\(.sp, .family) {
    get_index(paste0("report/stitch-cache/synoptic-mssm-SYN WCVI-", .family, "/"), clean_name(.sp), .family = .family)
  }) |>
  bind_rows() |>
  as_tibble() |>
  mutate(survey_abbrev = 'SYN WCVI on MSSM Grid') |>
  mutate(grid = 'MSSM 3km')


mssm_inds <- tidyr::expand_grid(.s = gfsynopsis::clean_name(spp_vector), f = families) |>
  purrr::pmap_dfr(function(.s, f) {
  get_index(paste0("report/stitch-cache/mssm-", f, "/"), .s, .family = f)
}) |> group_by(species) |>
  take_min_aic() |>
  ungroup() |>
  mutate(survey_abbrev = "MSSM Model") |>
  mutate(extreme_uci = max(upperci) > 10 * max(biomass))

# Old tweedie MSSM indexes comparing 2km and 3km
# mssm_2km_inds <- spp_vector |>
#   map(\(sp) readRDS(file.path(mssm_sc, '2km-grid', paste0(gfsynopsis:::clean_name(sp), '_st-rw.rds')))) |>
#   setNames(spp_vector) |>
#   keep(\(x) inherits(x, 'data.frame')) |>
#   bind_rows(.id = 'species') |>
#   as_tibble() |>
#   mutate(year_bins = "~ 1") |>
#   mutate(grid = '2km') |>
#   group_by(species) |>
#   mutate(extreme_uci = max(upperci) > 10 * max(biomass)) |>
#   ungroup()

# mssm_3km_inds <- spp_vector |>
#   map(\(sp) readRDS(file.path(mssm_sc, '3km-grid', paste0(gfsynopsis:::clean_name(sp), '_st-rw.rds')))) |>
#   setNames(spp_vector) |>
#   keep(\(x) inherits(x, 'data.frame')) |>
#   bind_rows(.id = 'species') |>
#   as_tibble() |>
#   mutate(grid = "3km") |>
#   group_by(species) |>
#   mutate(extreme_uci = max(upperci) > 10 * max(biomass)) |>
#   ungroup()

# SYN WCVI index
# syn_inds <-
#   spp_vector |>
#     map(\(sp) readRDS(file.path(syn_sc, 'syn-wcvi-grid', paste0(gfsynopsis:::clean_name(sp), '_st-rw.rds')))) |>
#     setNames(spp_vector) |>
#     keep(\(x) inherits(x, 'data.frame')) |>
#     bind_rows(.id = 'species') |>
#     mutate(grid = 'SYN WCVI') |>
#     as_tibble()

# syn_mssm_grid_inds <-
#   spp_vector |>
#     map(\(sp) readRDS(file.path(syn_sc, 'mssm-grid-3km', paste0(gfsynopsis:::clean_name(sp), '_st-rw.rds')))) |>
#     setNames(spp_vector) |>
#     keep(\(x) inherits(x, 'data.frame')) |>
#     bind_rows(.id = 'species') |>
#     as_tibble() |>
#     mutate(survey_abbrev = 'SYN WCVI on MSSM Grid') |>
#     mutate(grid = "3km MSSM")

# --- CPUE index ---
cpue_ind <- spp_vector |>
  map(\(sp) readRDS(file.path(cpue_cache, paste0(gfsynopsis:::clean_name(sp), '.rds')))) |>
  setNames(spp_vector) |>
  keep(\(x) inherits(x, 'data.frame')) |>
  bind_rows(.id = 'species') |>
  filter(area == '3CD') |>
  rename(biomass = est, lowerci = lwr, upperci = upr) |>
  mutate(survey_abbrev = 'CPUE 3CD') |>
  group_by(species) |>
  mutate(mean_cv = mean(sqrt(exp(se_link^2) - 1))) |>
  ungroup()

# --- Scale indices ---
# Get overlapping years to scale based on geometric means of indexes
syn_years <- unique(sw_dat$year)
mssm_years <- unique(mssm_dat$year)
cpue_years <- unique(cpue_ind$year)

syn_mssm_overlap <- intersect(syn_years, mssm_years)
cpue_mssm_overlap <- intersect(cpue_years, mssm_years)

# MSSM with SYN WCVI - Use 3km grid
inds <- bind_rows(mssm_inds, syn_wcvi_inds, syn_mssm_grid_inds, cpue_ind, mssm_d_inds) |>
  mutate(syn_overlap = ifelse(year %in% syn_mssm_overlap, TRUE, FALSE),
         cpue_overlap = ifelse(year %in% cpue_mssm_overlap, TRUE, FALSE)) |>
  filter(mean_cv < 1)

mssm_geomeans <- inds |>
  filter(survey_abbrev %in% c("MSSM Model", "MSSM Design")) |>
  group_by(species, survey_abbrev) |>
  summarise(mssm_geomean = exp(mean(log(biomass))), .groups = 'drop')

syn_overlap_geomeans <- inds |>
  filter(syn_overlap, survey_abbrev %in% c("MSSM Model", "SYN WCVI", "SYN WCVI on MSSM Grid")) |>
  group_by(species, survey_abbrev) |>
  summarise(syn_overlap_geomean = exp(mean(log(biomass))), .groups = 'drop')

cpue_overlap_geomeans <- inds |>
  filter(cpue_overlap, survey_abbrev %in% c("MSSM Model", "CPUE 3CD")) |>
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
  mutate(species = gsub("rougheye blackspotted", "rougheye/blackspotted", species)) |>
  order_spp()

raw_inds <- bind_rows(mssm_inds, syn_wcvi_inds, syn_mssm_grid_inds, cpue_ind, mssm_d_inds) |>
  mutate(syn_overlap = ifelse(year %in% syn_mssm_overlap, TRUE, FALSE),
         cpue_overlap = ifelse(year %in% cpue_mssm_overlap, TRUE, FALSE)) |>
  mutate(survey_abbrev = factor(survey_abbrev, levels = names(survey_cols))) |>
  mutate(species = gsub("rougheye blackspotted", "rougheye/blackspotted", species)) |>
  order_spp()

spp_in_mssm <- mssm_inds |>
  filter(!extreme_uci | is.na(extreme_uci)) |>
  filter(mean_cv < 1 | is.na(mean_cv)) |>
  distinct(species) |>
  mutate(species = gsub("rougheye blackspotted", "rougheye/blackspotted", species)) |>
  pluck('species') |>
  stringr::str_to_title()

spp_in_mssm_design <- mssm_d_inds |>
  count(species) |>
  filter(n > 2) |>
  distinct(species) |>
  pluck('species') |>
  stringr::str_to_title()

spp_in_mssm_design_only <- setdiff(spp_in_mssm_design, spp_in_mssm)

scale_geo_design <- function(df, survey1, survey2) {
  df <- filter(df, survey_abbrev %in% c(survey1, survey2))

  lvls <- levels(df$survey_abbrev)
  both_scaled <- group_by(df, species) |>
    group_split() |>
    purrr::map_dfr(\(x) {
      both_present <- length(unique(x$survey_abbrev[!is.na(x$biomass)])) > 1L
      if (both_present) {
        x_ind1 <- filter(x, survey_abbrev == survey1)
        x_ind2 <- filter(x, survey_abbrev == survey2)

        xx <- inner_join(
          select(x_ind1, year),
          select(x_ind2, year, biomass),
          by = join_by(year)
        ) |>
          filter(biomass > 0) # can't take geometric mean of these!
        overlapping_non_zero_years <- xx$year
        x_geo_mean <- exp(mean(log(x_ind1$biomass[x_ind1$year %in% overlapping_non_zero_years])))
        x_des_mean <- exp(mean(log(x_ind2$biomass[x_ind2$year %in% overlapping_non_zero_years])))

        x_ind1 <- mutate(x_ind1,
          biomass_scaled = biomass / x_geo_mean,
          lowerci_scaled = lowerci / x_geo_mean,
          upperci_scaled = upperci / x_geo_mean
        )
        x_ind2 <- mutate(x_ind2,
          biomass_scaled = biomass / x_des_mean,
          lowerci_scaled = lowerci / x_des_mean,
          upperci_scaled = upperci / x_des_mean
        )

        max_geo <- max(x_ind1$upperci_scaled, na.rm = TRUE)

        xx <- bind_rows(x_ind1, x_ind2)
        mutate(xx,
          biomass_scaled = biomass_scaled / max_geo,
          lowerci_scaled = lowerci_scaled / max_geo,
          upperci_scaled = upperci_scaled / max_geo
        )
      } else {
        if (sum(!is.na(x$biomass))) {
          mutate(x,
            biomass_scaled = biomass / max(upperci, na.rm = TRUE),
            lowerci_scaled = lowerci / max(upperci, na.rm = TRUE),
            upperci_scaled = upperci / max(upperci, na.rm = TRUE)
          )
        } else {
          mutate(x,
            biomass_scaled = NA_real_,
            lowerci_scaled = NA_real_,
            upperci_scaled = NA_real_
          )
        }
      }
    }) |>
    #mutate(biomass = biomass_scaled, lowerci = lowerci_scaled, upperci = upperci_scaled) |>
    mutate(survey_abbrev = factor(survey_abbrev, levels = lvls)) |>
    mutate(comp = paste(survey1, survey2, sep = '-'))
}


tidy_stats_df <- function(dat, survey1, survey2, survey2_code) {
  dat |>
    #filter(species %in% spp, survey_abbrev %in% c(survey1, survey2)) |>
    filter(survey_abbrev %in% c(survey1, survey2)) |>
    distinct() |>
    select(species, survey_abbrev, mean_cv, num_sets, num_pos_sets) |>
    group_by(species, survey_abbrev) |>
    summarise(
      mean_cv = sprintf("%.2f", round(mean(mean_cv, na.rm = TRUE), 2)),
      n_pos_sets = sprintf("%.0f", round(mean(num_pos_sets, na.rm = TRUE), 0)),
      n_sets = sprintf("%.0f", round(mean(num_sets, na.rm = TRUE), 0))) |>
    mutate(sets = paste0("Mean +ve sets", ": ", n_pos_sets, "/", n_sets)) |>
    mutate(cv = paste0("Mean", " CV: ", mean_cv)) |>
    mutate(cv = ifelse(mean_cv == "NaN", "", cv)) |>
    mutate(sets = ifelse(n_pos_sets == "NaN", "", sets)) |>
    mutate(prop_pos = round(as.numeric(n_pos_sets) / as.numeric(n_sets), digits = 2)) |>
    pivot_wider(
      id_cols = species,
      names_from = 'survey_abbrev',
      values_from = c('mean_cv', 'n_pos_sets', 'n_sets', 'prop_pos')
    ) |>
    mutate(
      sets = paste0(
        "Mean +ve sets",
        ": ",
        !!sym(paste0("prop_pos_", survey1)),
        ' (M), ',
        !!sym(paste0("prop_pos_", survey2)),
        ' (', survey2_code, ')'
      ),
      cv = paste0(
        "Mean",
        " CV: ",
        !!sym(paste0("mean_cv_", survey1)),
        ' (M), ',
        !!sym(paste0("mean_cv_", survey2)),
        ' (', survey2_code, ')'
      )
    ) |>
    ungroup() |>
    mutate(comp = paste(survey1, survey2, sep = '-'))
}

comp_df <- tibble(
  'survey1' = c('MSSM Design', rep('MSSM Model', 4)),
  'survey2' = c('MSSM Design', 'MSSM Design', 'SYN WCVI', 'SYN WCVI on MSSM Grid', 'CPUE 3CD'),
  'survey2_code' = c('', 'D', 'S', 'S', 'C'))

max_ci_scaled <- comp_df |>
  select(-survey2_code) |>
purrr::pmap(\(survey1, survey2) scale_geo_design(raw_inds, # |> filter(species %in% spp_in_mssm),
  survey1, survey2)
) |>
  bind_rows() |>
  group_by(comp) |>
  filter(ifelse(comp == "MSSM Design-MSSM Design", species %in% spp_in_mssm_design_only, species %in% spp_in_mssm)) |>
  ungroup()

stats_df <- comp_df |>
  purrr::pmap(\(survey1, survey2, survey2_code)
    tidy_stats_df(max_ci_scaled, survey1, survey2, survey2_code)
  ) |>
  bind_rows() |>
  group_by(comp) |>
  filter(ifelse(comp == "MSSM Design-MSSM Design", species %in% spp_in_mssm_design_only, species %in% spp_in_mssm)) |>
  ungroup()

indices_loaded <- TRUE