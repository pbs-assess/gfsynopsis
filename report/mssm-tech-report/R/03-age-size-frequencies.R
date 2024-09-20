if (!('mssm_loaded' %in% ls())) {
  source(here::here('report', 'mssm-tech-report', 'R', '00-load.R'))
}

if (!('indices_loaded' %in% ls())) {
  source(here::here('report', 'mssm-tech-report', 'R', '02-load-indices.R'))
}

save_plots_to_pdf <- function(ggplot_list, filename, width = 7, height = 3.7) {
  pdf(filename, width = width, height = height)
  purrr::map(ggplot_list, print)
  dev.off()
}

# Get length and age distributions
dir.create(mssm_data_out, showWarnings = FALSE)
if (!file.exists(file.path(mssm_data_out, 'size-dat.rds'))) {
  size_dat <- spp_hyphens |>
    map_df(\(sp) readRDS(file.path(data_cache, paste0(sp, ".rds")))$survey_samples) |>
    filter(survey_abbrev %in% c('MSSM WCVI', 'SYN WCVI')) |>
    select(species_common_name, year, survey_abbrev, specimen_id, sample_id, sex, age,
          length, weight, length_type) |>
    distinct(specimen_id, .keep_all = TRUE) |>
    filter(!(species_common_name == 'eulachon' & length > 40)) |>
    mutate(survey_abbrev = gsub("MSSM", "SMMS", survey_abbrev))
  saveRDS(size_dat, file = file.path(mssm_data_out, 'size-dat.rds'))
  beepr::beep()
}

size_dat <- readRDS(file.path(mssm_data_out, 'size-dat.rds')) |>
  mutate(species_common_name = gsub("north pacific spiny dogfish", "pacific spiny dogfish", species_common_name)) |>
  mutate(survey_abbrev = gsub('SMMS WCVI', 'SMMS', survey_abbrev)) |>
  mutate(fspecies = as.factor(species_common_name), fregion = as.factor(survey_abbrev)) |>
  filter(!is.na(length))

# Do a quick check with a model to make sure that pseudoreplication of measured
# wasn't changing the results.
# When considering 10% differences then it makes no difference
# length_fit <- lme4::lmer(formula = log(length) ~ 0 + fspecies:fregion + (fspecies | sample_id), data = size_dat)
# Compute estimated marginal means for species in each region
# means <- emmeans::emmeans(length_fit, ~ fspecies:fregion, by = "fspecies", type = "response") |>
#   as_tibble() |>
#   drop_na(emmean)

# # Pairwise comparison of regions within each species
# contrast_means <- emmeans::contrast(means, method = "pairwise", by = "fspecies")
# contrast_df <- as_tibble(contrast_means) |>
#   drop_na(estimate)

# means <- means |>
#   pivot_wider(id_cols = fspecies, names_from = "fregion", values_from = c("emmean", "SE")) |>
#   mutate(emmean_abs_diff = `emmean_MSSM WCVI` - `emmean_SYN WCVI`,
#          emmean_pct_diff = emmean_abs_diff / ((`emmean_SYN WCVI` + `emmean_MSSM WCVI`) / 2) * 100) |>
#   mutate(emmean_bigger = case_when(emmean_pct_diff <= -10 ~ "SYN WCVI",
#                                    emmean_pct_diff >= 10 ~ "SMMS",
#                                    TRUE ~ "< 10% difference")) |>
#   mutate(species_common_name = as.character(fspecies), survey_abbrev = as.character(fregion)) |>
#   select(species_common_name, survey_abbrev, emmean, SE, lci = "asymp.LCL", uci = "asymp.UCL")

size_summary <- size_dat |>
  filter(survey_abbrev %in% c('SMMS', 'SYN WCVI')) |>
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
  mutate(syn_minus_mssm_q50 = `SYN WCVI` - `SMMS`,
         abs_diff = abs(syn_minus_mssm_q50),
         pct_diff = syn_minus_mssm_q50 / ((`SYN WCVI` + `SMMS`) / 2) * 100
         ) |>
  drop_na(abs_diff) |>
  select(species_common_name, syn_minus_mssm_q50, abs_diff, pct_diff) |>
  mutate(bigger = case_when(pct_diff > 10 ~ 'SYN WCVI\nlarger',
                          pct_diff < -10 ~ 'SMMS\nlarger',
                          TRUE ~ "< 10% difference")) |>
  mutate(bigger = factor(bigger, levels = c("SMMS\nlarger", "< 10% difference", "SYN WCVI\nlarger"))) |>
  arrange(bigger, abs_diff) |>
  mutate(bg = ifelse(row_number() %% 2 == 1, 'grey95', NA),
         species = stringr::str_to_title(species_common_name)) |>
  mutate(species = factor(species, levels = unique(species)))

# size_diff_lu <- test |>
#   mutate(species_common_name = as.character(fspecies), bigger = emmean_bigger) |>
#   arrange(bigger, emmean_abs_diff) |>
#   select(species, bigger = emmean_bigger) |>
#   mutate(bg = ifelse(row_number() %% 2 == 1, 'grey95', NA),
#          species = factor(species_common_name, levels = unique(species_common_name)))


size_comp_df <-
  left_join(size_summary, size_diff_lu) |>
  group_by(species_common_name) |>
  filter(n() == 2) |> # get only species that are found in both surveys
  ungroup() |>
  mutate(species = factor(species, levels = pull(size_diff_lu, species))) |>
  arrange(species) |>
  mutate(species = forcats::fct_recode(species, "Rougheye/Blackspotted\nRockfish Complex" = "Rougheye/Blackspotted Rockfish Complex"))
  # left_join(means)

raw_size_dat <- size_dat |>
  left_join(size_diff_lu) |>
  drop_na(abs_diff) |>
  mutate(species = factor(species, levels = pull(size_diff_lu, species))) |>
  mutate(species = forcats::fct_recode(species, "Rougheye/Blackspotted\nRockfish Complex" = "Rougheye/Blackspotted Rockfish Complex")) |>
  distinct(species, survey_abbrev, year, length, .keep_all = TRUE)

size_comp <- size_comp_df |>
  ggplot(aes(x = species, y = q50, colour = survey_abbrev)) +
  geom_point(data = raw_size_dat,
      aes(x = species, y = length, fill = survey_abbrev),
    colour = 'grey80', alpha = 0.2, shape = 21,
    position = position_jitterdodge(dodge.width = 0.5,
    jitter.height = 0.4, jitter.width = 0.125)
    ) +
#    geom_tile(aes(height = Inf, width = 1, fill = bg), colour = NA, alpha = 0.3) +
    geom_pointrange(mapping = aes(ymin = q25, ymax = q75),
       position = position_dodge(width = 0.4)) +
    # geom_pointrange(mapping = aes(y = emmean, ymin = lci, ymax = uci),
    #    position = position_dodge(width = 0.4)) +
    # scale_colour_manual(values = c("SMMS" = "#1b9e77", "SYN WCVI" = "#4f4a8c"))
    scale_colour_manual(values = survey_cols) +
    scale_fill_manual(values = c('#c3f5e6', "#8581bc" ), guide = "legend") +
    theme(
      strip.clip = "off",
      axis.text.x = element_text(angle = 60, hjust = 1),
      axis.title.x = element_blank()) +
    #scale_fill_identity() +
    guides(fill = 'none') +
    facet_grid(~ bigger, drop = TRUE, scales = 'free_x', space = 'free') +
    labs(colour = "Survey", y = 'Length (cm)') +
    theme(legend.position = c(0.5, -0.4),
      legend.direction = "horizontal",
      plot.margin = margin(1, 5, 35, 1))
size_comp

ggsave(file.path(mssm_figs, 'size-comp.png'), plot = size_comp, width = 10.5, height = 6.6)

age_summary <- size_dat |>
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
  mutate(syn_minus_mssm_q50 = `SYN WCVI` - `SMMS`,
         abs_diff = abs(syn_minus_mssm_q50)) |>
  select(species_common_name, syn_minus_mssm_q50, abs_diff) |>
  mutate(bigger = case_when(syn_minus_mssm_q50 > 0 ~ 'SYN WCVI older',
                          syn_minus_mssm_q50 < 0 ~ 'SMMS older',
                          TRUE ~ "Equal")) |>
  arrange(bigger, abs_diff) |>
  mutate(bg = ifelse(row_number() %% 2 == 1, 'grey95', NA),
         species = stringr::str_to_title(species_common_name)) |>
  mutate(species = factor(species, levels = unique(species))) |>
  mutate(species = forcats::fct_recode(species, "Rougheye/Blackspotted\nRockfish Complex" = "Rougheye/Blackspotted Rockfish Complex")) |>
  drop_na(abs_diff)  # ignore species that are not measured in MSSM


raw_age_dat <- size_dat |>
  left_join(age_diff_lu) |>
  drop_na(abs_diff) |>
  distinct(species, survey_abbrev, year, age, .keep_all = TRUE)

age_comp <-
  left_join(age_summary, age_diff_lu) |>
    group_by(species_common_name) |>
    filter(n() > 1) |>
    ungroup() |>
  ggplot(aes(x = species, y = q50, colour = survey_abbrev)) +
    geom_point(data = raw_age_dat,
      aes(x = species, y = age, fill = survey_abbrev),
    colour = 'grey80', alpha = 0.3, shape = 21,
    position = position_jitterdodge(dodge.width = 0.5,
    jitter.height = 0.4, jitter.width = 0.2)
    ) +
    #geom_tile(aes(height = Inf, width = 1, fill = bg), colour = NA, alpha = 0.3) +
    geom_pointrange(mapping = aes(ymin = q25, ymax = q75),
       position = position_dodge(width = 0.4)) +
    scale_colour_manual(values = survey_cols) +
    scale_fill_manual(values = c('#c3f5e6', "#8581bc"), guide = "legend") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank()) +
    #scale_fill_identity() +
    guides(fill = 'none') +
    facet_grid(~ bigger, drop = TRUE, scales = 'free_x', space = 'free') +
    labs(colour = 'Survey', y = 'Age') +
    theme(legend.position = c(0.5, -0.7),
      legend.direction = "horizontal",
      plot.margin = margin(1, 1, 20, 1))
    #theme(legend.position = "top")
age_comp

ggsave(file.path(mssm_figs, 'age-comp.png'), plot = age_comp, width = 5.8, height = 3.8)

# ------------------------------------------------------------------------------
# ---- Look at size distributions at pulses --------
#- scaled to geomean (2003 - 2022)
plot_size_time <- function(sp, raw_data = FALSE, no_guides = TRUE,
  y_title = TRUE, x_title = TRUE, ylab = 'Relative biomass\nindex') {
  buffer <- 0.4

  p1 <- scale_geo_design(raw_inds |> filter(species %in% spp_in_mssm, year > 2003),
    'SMMS Model', 'SYN WCVI on SMMS Grid') |>
    mutate(survey_abbrev = ifelse(survey_abbrev == "SMMS Model", "SMMS", "SYN WCVI")) |>
    filter(tolower(species) == sp) |>
    ggplot(aes(x = year, y = biomass_scaled, colour = survey_abbrev, fill = survey_abbrev)) +
      geom_line() +
      geom_ribbon(aes(ymin = lowerci_scaled, ymax = upperci_scaled), alpha = 0.2, colour = NA) +
      scale_colour_manual(values = survey_cols, guide = "legend") +
      scale_fill_manual(values = survey_cols, guide = "legend") +
      labs(x = 'Year', y = ylab, colour = "Survey", fill = "Survey") +
      ggtitle(stringr::str_to_title(sp)) +
      guides(colour = 'none', fill = 'none') +
      scale_y_continuous(breaks = c(0, 0.5, 1)) +
      coord_cartesian(xlim = c(2004 - buffer, 2022 + buffer),
        ylim = (c(-0.004, 1.03)),
        expand = FALSE) +
      gfplot::theme_pbs(base_size = 11) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank())

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

  p2 <- ggplot(data = size_summ_df, aes(x = year, y = q50, colour = survey_abbrev)) +
    gfplot::theme_pbs(base_size = 11)

  if (raw_data) {
    p2 <- p2 + geom_point(data = size_df, aes(x = year, y = length, fill = survey_abbrev),
      colour = 'grey80', alpha = 0.6, shape = 21,
      position = position_jitterdodge(dodge.width = 0.5,
        jitter.height = 0.4, jitter.width = 0.2)
    )
      #position = position_dodge(width = 0.4))
  }

  if (no_guides) {
    p2 <- p2 + guides(colour = 'none', fill = 'none')
  }

  if (!y_title) {
    p1 <- p1 + theme(axis.title.y = element_blank())
    p2 <- p2 + theme(axis.title.y = element_blank())
  }

  if (!x_title) {
    p2 <- p2 + theme(axis.title.x = element_blank(),
                     axis.text.x = element_blank())
  }

  p2 <- p2 +
    geom_pointrange(aes(ymin = q25, ymax = q75), position = position_dodge(width = 0.4)) +
    scale_colour_manual(values = survey_cols, guide = "legend") +
    scale_fill_manual(values = c('#c3f5e6', "#8581bc"), guide = "legend") +
    labs(x = "Year", y = 'Length (cm)', colour = "Survey") +
    guides(fill = 'none') +
    coord_cartesian(xlim = c(2004 - buffer, 2022 + buffer), expand = FALSE)

  (p1 / p2) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

}

sp <- 'eulachon'
sp <- 'petrale sole'
sp <- 'flathead sole'
sp <- 'bocaccio'
sp <- 'arrowtooth flounder'
plot_size_time(sp, raw_data = TRUE)
plot_size_time(sp, raw_data = FALSE)




test <- size_dat |>
    filter(year > 2003) |>
    group_by(species_common_name, year, survey_abbrev) |>
    summarise(q50 = quantile(length, 0.5, na.rm = TRUE),
              q25 = quantile(length, 0.25, na.rm = TRUE),
              q75 = quantile(length, 0.75, na.rm = TRUE),
              n = sum(!is.na(length))) |>
    ungroup()

test |> filter(species_common_name == "bocaccio") |>
  arrange(q50)

# size_summary_spp <- distinct(size_comp_df, species_common_name) |>
#   pull(species_common_name)

size_summary_spp <- max_ci_scaled |>
  filter(survey_abbrev %in% c('SMMS', 'SYN WCVI')) |>
  group_by(species) |>
  filter(n() > 1) |>
  mutate(species = factor(species, levels = pull(size_diff_lu, species))) |>
  distinct(species) |>
  arrange(desc(species)) |>
  pull(species) |>
  tolower()

mk_p_list <- function(spp, y = c(TRUE, FALSE), x = c(F, F, F, F, T, T), extra_x = T, ...) {
  tibble(species = spp,
    y_title = rep(y, length.out = length(spp)),
    x_title = c(rep(x, length.out = length(spp) - length(extra_x)), extra_x)) |>
  purrr::pmap(\(species, y_title, x_title)
    plot_size_time(species, y_title = y_title, x_title = x_title, ...)) |>
  set_names(spp)
}

highlight_spp <- c('rougheye/blackspotted rockfish complex', 'darkblotched rockfish', 'bocaccio')

# Option to be able to order by taxonomic grouping
# ordered_spp <- readRDS(file.path(mssm_data_out, "mssm-species-by-taxonomic-grouping.rds")) |>
#   mutate(species_common_name = tolower(species_common_name)) |>
#   pull(species_common_name)

# Currently ordered from largest median size difference to minimum difference
size_p_list <- mk_p_list(spp = size_summary_spp[!(size_summary_spp %in% highlight_spp)],
  extra_x = c(T, T), raw_data = TRUE)

plots_per_page <- 6
guide_grob <- plot_size_time(sp, raw_data = TRUE, no_guides = FALSE) |>
  cowplot::get_legend()

# arranged_plots <- size_p_list %>%
#   split((seq_along(.) - 1) %/% plots_per_page) %>%
#   #map(~c(.x, rep_len(list(plot_spacer()), plots_per_page - length(.x)))) %>%
#   map(~wrap_plots(.x, ncol = 2) / guide_grob + plot_layout(height = c(1, 0.03)))
# save_plots_to_pdf(arranged_plots, filename = file.path(mssm_figs, 'size_time_plots.pdf'),
#   width = 7.8, height = 9)

# Breaking it up this way means the title page of the appendix has the smaller panel of plots
arranged_plots1 <- size_p_list[1:4] %>%
  split((seq_along(.) - 1) %/% plots_per_page) %>%
  map(~wrap_plots(.x, ncol = 2) / guide_grob + plot_layout(height = c(1, 0.03)))
arranged_plots1
ggsave(filename = file.path(mssm_figs, paste0('size-time-', 0, '.png')),
  width = 7.8, height = 7)

arranged_plots2 <- size_p_list[-(1:4)] %>%
  split((seq_along(.) - 1) %/% plots_per_page) %>%
  #map(~c(.x, rep_len(list(plot_spacer()), plots_per_page - length(.x)))) %>%
  map(~wrap_plots(.x, ncol = 2) / guide_grob + plot_layout(height = c(1, 0.03)))
purrr::map2(arranged_plots2, seq_along(arranged_plots2), ~ggsave(file.path(mssm_figs, paste0('size-time-', .y, '.png')), .x,
  width = 7.8, height = 10))


# Species that might be capturing some extra recruitment in the MSSM compared to SYN WCVI
highlight_list <- mk_p_list(
  spp = highlight_spp,
  y = c(T, F, F), x = c(T, T, T),
  ylab = 'Relative biomass index',
  raw_data = TRUE)

# wrap_plots(highlight_list, ncol = 3) /
# guide_grob  & plot_layout(height = c(1, 1, 1))

(highlight_list[[1]] | highlight_list[[2]] | highlight_list[[3]]) /
  guide_grob + plot_layout(height = c(1, 0.1))

ggsave(filename = file.path(mssm_figs, 'size-time-highlights.png'),
  width = 10.5, height = 5.6)
