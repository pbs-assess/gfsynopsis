if (!('mssm_loaded' %in% ls())) {
  source(here::here('report', 'mssm-tech-report', 'R', '00-load.R'))
}

# Get length and age distributions
if (!file.exists(file.path(mssm_dir, 'size-dat.rds'))) {
  size_dat <- spp_vector |>
    map(\(sp) readRDS(file.path(data_cache, paste0(gfsynopsis:::clean_name(sp), ".rds")))$survey_samples) |>
    bind_rows() |>
    filter(survey_abbrev %in% c('MSSM WCVI', 'SYN WCVI')) |>
    select(species_common_name, year, survey_abbrev, specimen_id, sample_id, sex, age,
          length, weight, length_type) |>
    distinct(specimen_id, .keep_all = TRUE)
  saveRDS(size_dat, file = file.path(mssm_dir, 'size-dat.rds'))
}
size_dat <- readRDS(file.path(mssm_dir, 'size-dat.rds'))

size_summary <- size_dat |>
  filter(survey_abbrev %in% c('MSSM WCVI', 'SYN WCVI')) |>
  group_by(species_common_name, survey_abbrev) |>
  summarise(q50 = quantile(length, 0.5, na.rm = TRUE),
            q25 = quantile(length, 0.25, na.rm = TRUE),
            q75 = quantile(length, 0.75, na.rm = TRUE),
            n = sum(!is.na(length))) |>
  mutate(mean_length = mean(q50)) |>
  ungroup()

size_diff_lu <-
  size_summary |>
  pivot_wider(id_cols = species_common_name, names_from = survey_abbrev, values_from = q50) |>
  mutate(syn_minus_mssm_q50 = `SYN WCVI` - `MSSM WCVI`,
         abs_diff = abs(syn_minus_mssm_q50)) |>
  select(species_common_name, syn_minus_mssm_q50, abs_diff) |>
  mutate(bigger = case_when(syn_minus_mssm_q50 > 0 ~ 'SYN',
                          syn_minus_mssm_q50 < 0 ~ 'MSSM',
                          TRUE ~ "Equal")) |>
  arrange(bigger, abs_diff) |>
  mutate(bg = ifelse(row_number() %% 2 == 1, 'grey95', NA))

size_comp_df <-
  left_join(size_summary, size_diff_lu) |>
  group_by(species_common_name) |>
  filter(n() == 2) |> # get only species that are found in both surveys
  ungroup() |>
  mutate(species_common_name = factor(species_common_name, levels = pluck(size_diff_lu, 'species_common_name'))) |>
    arrange(species_common_name)

size_comp <- size_comp_df |>
  ggplot(aes(x = species_common_name, y = q50, colour = survey_abbrev)) +
    geom_tile(aes(height = Inf, width = 1, fill = bg), colour = NA, alpha = 0.3) +
    geom_pointrange(mapping = aes(ymin = q25, ymax = q75),
       position = position_dodge(width = 0.4)) +
    scale_colour_manual(values = survey_cols) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank()) +
    scale_fill_identity() +
    facet_grid(~ bigger, drop = TRUE, scales = 'free_x', space = 'free') +
    labs(colour = "Survey", y = 'Length (cm)') +
    theme(legend.position = c(0.5, -0.5), legend.direction = "horizontal")
size_comp

ggsave(file.path(mssm_figs, 'size-comp.png'), plot = size_comp,
  width = 13, height = 4.5)

age_summary <- size_dat |>
  #filter(species_common_name %in% spp_in_mssm) |>
  filter(survey_abbrev %in% c('MSSM WCVI', 'SYN WCVI')) |>
  group_by(species_common_name, survey_abbrev) |>
  filter(!is.na(age)) |>
  summarise(q50 = quantile(age, 0.5, na.rm = TRUE),
            q25 = quantile(age, 0.25, na.rm = TRUE),
            q75 = quantile(age, 0.75, na.rm = TRUE),
            n = n()) |>
  #mutate(mean_age = mean(q50)) |>
  ungroup() |>
  filter(n != 0)

age_diff_lu <-
  age_summary |>
  pivot_wider(id_cols = species_common_name, names_from = survey_abbrev, values_from = q50) |>
  mutate(syn_minus_mssm_q50 = `SYN WCVI` - `MSSM WCVI`,
         abs_diff = abs(syn_minus_mssm_q50)) |>
  select(species_common_name, syn_minus_mssm_q50, abs_diff) |>
  mutate(bigger = case_when(syn_minus_mssm_q50 > 0 ~ 'SYN',
                          syn_minus_mssm_q50 < 0 ~ 'MSSM',
                          TRUE ~ "Equal")) |>
  arrange(bigger, abs_diff) |>
  mutate(bg = ifelse(row_number() %% 2 == 1, 'grey95', NA)) |>
  filter(!is.na(abs_diff))  # ignore species that are not measured in MSSM

age_comp <-
  left_join(age_summary, age_diff_lu) |>
    group_by(species_common_name) |>
    filter(n() > 1) |>
    ungroup() |>
    mutate(species_common_name = factor(species_common_name, levels = pluck(age_diff_lu, 'species_common_name'))) |>
    arrange(species_common_name) |>
  ggplot(aes(x = species_common_name, y = q50, colour = survey_abbrev)) +
    geom_tile(aes(height = Inf, width = 1, fill = bg), colour = NA, alpha = 0.3) +
    geom_pointrange(mapping = aes(ymin = q25, ymax = q75),
       position = position_dodge(width = 0.4)) +
    scale_colour_manual(values = survey_cols) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank()) +
    scale_fill_identity() +
    facet_grid(~ bigger, drop = TRUE, scales = 'free_x', space = 'free') +
    labs(colour = 'Survey', y = 'Age') +
    theme(legend.position = "top")
age_comp

ggsave(file.path(mssm_figs, 'age-comp.png'), plot = age_comp,
  width = 4, height = 4)

# ------------------------------------------------------------------------------
# ---- Look at size distributions at pulses --------
#- scaled to geomean (2003 - 2022)
plot_size_time <- function(sp) {
  # p1 <- mssm_3km_inds |>
  #   filter(year >= 2003) |>
  #   filter(species == sp) |>
  #   mutate(geomean = exp(mean(log(biomass))),
  #          scaled_biomass = biomass / geomean,
  #          scaled_lowerci = lowerci / geomean,
  #          scaled_upperci = upperci / geomean) |>

  p1 <- scaled_inds |>
    filter(survey_abbrev %in% c("MSSM Model", "SYN WCVI on MSSM Grid")) |>
    mutate(survey_abbrev = ifelse(survey_abbrev == "MSSM Model", "SYN WCVI", "MSSM WCVI")) |>
    filter(year > 2003) |>
    filter(species == sp) |>
    ggplot(aes(x = year, y = syn_scaled_biomass, colour = survey_abbrev, fill = survey_abbrev)) +
      geom_line() +
      geom_ribbon(aes(ymin = syn_scaled_lowerci, ymax = syn_scaled_upperci), alpha = 0.2, colour = NA) +
      scale_colour_manual(values = survey_cols, guide = "legend") +
      scale_fill_manual(values = survey_cols, guide = "legend") +
      labs(x = 'Year', y = 'Relative biomass index', colour = "Survey", fill = "Survey") +
      ggtitle(paste0('MSSM Index - ', sp)) +
      xlim(c(2003, 2022)) +
      guides(colour = 'none', fill = 'none')

  p2 <- size_dat |>
    filter(species_common_name == sp) |>
    filter(year >= 2003) |>
    group_by(species_common_name, year, survey_abbrev) |>
    summarise(q50 = quantile(length, 0.5, na.rm = TRUE),
              q25 = quantile(length, 0.25, na.rm = TRUE),
              q75 = quantile(length, 0.75, na.rm = TRUE),
              n = sum(!is.na(length))) |>
    ungroup() |>
  ggplot(aes(x = year, y = q50, colour = survey_abbrev)) +
    geom_pointrange(aes(ymin = q25, ymax = q75), position = position_dodge(width = 0.4)) +
    geom_point(data = size_dat |> filter(species_common_name == sp) |>
    filter(year >= 2003), aes(x = year, y = length), alpha = 0.3, position = position_dodge(width = 0.5)) +
    scale_colour_manual(values = survey_cols, guide = "legend") +
    labs(x = "Year", y = 'Length (cm)', colour = "Survey") +
    xlim(c(2003, 2022))

  (p1 / p2) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')


}

plot_size_time(sp)

size_summary_spp <- distinct(size_comp_df, species_common_name) |>
  pluck('species_common_name') |>
  as.character()

size_p_list <-
size_summary_spp |>
  purrr::map(plot_size_time)

save_plots_to_pdf <- function(ggplot_list, filename, width = 7, height = 3.7) {
  pdf(filename, width = width, height = height)
  purrr::map(ggplot_list, print)
  dev.off()
}

save_plots_to_pdf(size_p_list, filename = file.path(mssm_figs, 'size_time_plots.pdf'),
  width = 7.8, height = 6)

sp <- 'bocaccio'
sp <- 'rougheye/blackspotted rockfish complex'
sp <- 'pacific hake'
sp <- 'pacific cod'
sp <- 'arrowtooth flounder'
sp <- 'spotted ratfish'
sp <- 'dover sole'
sp <- 'pacific ocean perch'
sp <- 'rex sole'

plot_size_time(sp)



