if (!('mssm_loaded' %in% ls())) {
  source(here::here('report', 'mssm-tech-report', 'R', '00-load.R'))
}

# Get length and age distributions
if (!file.exists(file.path(mssm_data_out, 'size-dat.rds'))) {
  size_dat <- spp_vector |>
    map(\(sp) readRDS(file.path(data_cache, paste0(gfsynopsis:::clean_name(sp), ".rds")))$survey_samples) |>
    bind_rows() |>
    filter(survey_abbrev %in% c('MSSM WCVI', 'SYN WCVI')) |>
    select(species_common_name, year, survey_abbrev, specimen_id, sample_id, sex, age,
          length, weight, length_type) |>
    distinct(specimen_id, .keep_all = TRUE) |>
    filter(!(species_common_name == 'eulachon' & length > 40))
  saveRDS(size_dat, file = file.path(mssm_data_out, 'size-dat.rds'))
  beepr::beep()
}
size_dat <- readRDS(file.path(mssm_data_out, 'size-dat.rds'))

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
  mutate(bg = ifelse(row_number() %% 2 == 1, 'grey95', NA),
         species = stringr::str_to_title(species_common_name)) |>
  mutate(species = factor(species, levels = unique(species)))

size_comp_df <-
  left_join(size_summary, size_diff_lu) |>
  group_by(species_common_name) |>
  filter(n() == 2) |> # get only species that are found in both surveys
  ungroup() |>
  mutate(species = factor(species, levels = pull(size_diff_lu, species))) |>
    arrange(species)

raw_size_dat <- size_dat |>
  left_join(size_diff_lu) |>
  drop_na(abs_diff) |>
  mutate(species = factor(species, levels = pull(size_diff_lu, species))) |>
  distinct(species, survey_abbrev, year, length, .keep_all = TRUE)


size_comp <- size_comp_df |>
  ggplot(aes(x = species, y = q50, colour = survey_abbrev)) +
  geom_point(data = raw_size_dat,
      aes(x = species, y = length, fill = survey_abbrev),
    colour = 'grey80', alpha = 0.2, shape = 21,
    position = position_jitterdodge(dodge.width = 0.5,
    jitter.height = 0.4, jitter.width = 0.2)
    ) +
#    geom_tile(aes(height = Inf, width = 1, fill = bg), colour = NA, alpha = 0.3) +
    geom_pointrange(mapping = aes(ymin = q25, ymax = q75),
       position = position_dodge(width = 0.4)) +
    scale_colour_manual(values = survey_cols) +
    scale_fill_manual(values = c('#c3f5e6', '#d8d6e9'), guide = "legend") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),
      axis.title.x = element_blank()) +
    #scale_fill_identity() +
    guides(fill = 'none') +
    facet_grid(~ bigger, drop = TRUE, scales = 'free_x', space = 'free') +
    labs(colour = "Survey", y = 'Length (cm)') +
    theme(legend.position = c(0.5, -0.5), legend.direction = "horizontal")
size_comp

ggsave(file.path(mssm_figs, 'size-comp.png'), plot = size_comp,
  #width = 13, height = 4.5)
  width = 10, height = 5.2)

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
  mutate(bg = ifelse(row_number() %% 2 == 1, 'grey95', NA),
         species = stringr::str_to_title(species_common_name)) |>
  mutate(species = factor(species, levels = unique(species))) |>
  drop_na(abs_diff)  # ignore species that are not measured in MSSM

raw_age_dat <- size_dat |>
  left_join(age_diff_lu) |>
  drop_na(abs_diff) |>
  mutate(species = factor(species, levels = pull(age_diff_lu, species))) |>
  distinct(species, survey_abbrev, year, age, .keep_all = TRUE)

age_comp <-
  left_join(age_summary, age_diff_lu) |>
    group_by(species_common_name) |>
    filter(n() > 1) |>
    ungroup() |>
  ggplot(aes(x = species, y = q50, colour = survey_abbrev)) +
    geom_point(data = raw_age_dat,
      aes(x = species, y = age, fill = survey_abbrev),
    colour = 'grey80', alpha = 0.2, shape = 21,
    position = position_jitterdodge(dodge.width = 0.5,
    jitter.height = 0.4, jitter.width = 0.2)
    ) +
    #geom_tile(aes(height = Inf, width = 1, fill = bg), colour = NA, alpha = 0.3) +
    geom_pointrange(mapping = aes(ymin = q25, ymax = q75),
       position = position_dodge(width = 0.4)) +
    scale_colour_manual(values = survey_cols) +
    scale_fill_manual(values = c('#c3f5e6', '#d8d6e9'), guide = "legend") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank()) +
    #scale_fill_identity() +
    guides(fill = 'none') +
    facet_grid(~ bigger, drop = TRUE, scales = 'free_x', space = 'free') +
    labs(colour = 'Survey', y = 'Age') #+
    #theme(legend.position = "top")
age_comp

ggsave(file.path(mssm_figs, 'age-comp.png'), plot = age_comp,
  width = 4.2, height = 4.5)

# ------------------------------------------------------------------------------
# ---- Look at size distributions at pulses --------
#- scaled to geomean (2003 - 2022)
plot_size_time <- function(sp, raw_data = FALSE) {
  buffer <- 0.4
  p1 <- max_ci_scaled |>
    filter(comp %in% c("MSSM Model-SYN WCVI on MSSM Grid")) |>
    mutate(survey_abbrev = ifelse(survey_abbrev == "MSSM Model", "MSSM WCVI", "SYN WCVI")) |>
    filter(year > 2003) |>
    filter(tolower(species) == sp) |>
    ggplot(aes(x = year, y = biomass_scaled, colour = survey_abbrev, fill = survey_abbrev)) +
      geom_line() +
      geom_ribbon(aes(ymin = lowerci_scaled, ymax = upperci_scaled), alpha = 0.2, colour = NA) +
      scale_colour_manual(values = survey_cols, guide = "legend") +
      scale_fill_manual(values = survey_cols, guide = "legend") +
      labs(x = 'Year', y = 'Relative biomass index', colour = "Survey", fill = "Survey") +
      ggtitle(paste0('MSSM Index - ', sp)) +
      #guides(colour = 'none', fill = 'none')
      coord_cartesian(xlim = c(2004 - buffer, 2022 + buffer), expand = FALSE)

  size_df <- size_dat |>
    filter(species_common_name == sp) |>
    filter(year > 2003)

  size_summ_df <- size_df |>
    group_by(species_common_name, year, survey_abbrev) |>
    summarise(q50 = quantile(length, 0.5, na.rm = TRUE),
              q25 = quantile(length, 0.25, na.rm = TRUE),
              q75 = quantile(length, 0.75, na.rm = TRUE),
              n = sum(!is.na(length))) |>
    ungroup()

  p2 <- ggplot(data = size_summ_df, aes(x = year, y = q50, colour = survey_abbrev))

  if (raw_data) {
    p2 <- p2 + geom_point(data = size_df, aes(x = year, y = length, fill = survey_abbrev),
      colour = 'grey80', alpha = 0.2, shape = 21,
      position = position_jitterdodge(dodge.width = 0.5,
        jitter.height = 0.4, jitter.width = 0.2)
    )
      #position = position_dodge(width = 0.4))
  }

  p2 <- p2 +
    geom_pointrange(aes(ymin = q25, ymax = q75), position = position_dodge(width = 0.4)) +
    scale_colour_manual(values = survey_cols, guide = "legend") +
    scale_fill_manual(values = c('#c3f5e6', '#d8d6e9'), guide = "legend") +
    labs(x = "Year", y = 'Length (cm)', colour = "Survey") +
    guides(fill = 'none') +
    coord_cartesian(xlim = c(2004 - buffer, 2022 + buffer), expand = FALSE)

  (p1 / p2) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

}

sp <- 'eulachon'
sp <- 'petrale sole'
plot_size_time(sp, raw_data = TRUE)
plot_size_time(sp, raw_data = FALSE)

size_summary_spp <- distinct(size_comp_df, species_common_name) |>
  pull(species_common_name)

size_p_list <-
size_summary_spp |>
  purrr::map(plot_size_time, raw_data = TRUE)

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



