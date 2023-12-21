if (!('indices_loaded' %in% ls())) {
  source(here::here('report', 'mssm-tech-report', 'R', '02-load-indices.R'))
}

tidy_stats_df <- function(df) {
  select(df, species, survey_abbrev, mean_cv, num_sets, num_pos_sets) %>%
    group_by(species, survey_abbrev) %>%
    summarise(
      mean_cv = sprintf("%.2f", round(mean(mean_cv, na.rm = TRUE), 2)),
      n_pos_sets = sprintf("%.0f", round(mean(num_pos_sets, na.rm = TRUE), 0)),
      n_sets = sprintf("%.0f", round(mean(num_sets, na.rm = TRUE), 0))) %>%
    mutate(sets = paste0("Mean +ve sets", ": ", n_pos_sets, "/", n_sets)) %>%
    mutate(cv = paste0("Mean", " CV: ", mean_cv)) %>%
    mutate(cv = ifelse(mean_cv == "NaN", "", cv)) %>%
    mutate(sets = ifelse(n_pos_sets == "NaN", "", sets)) #%>%
    # mutate(mean_cv = as.numeric(mean_cv)) %>%
    # mutate(n_pos_sets = as.numeric(n_pos_sets),
    #   n_sets = as.numeric(n_sets))
}

ind_layers <- function(colours = survey_cols, ncol = 4,
  xlim = c(1975 - 0.2, 2022 + 0.2), breaks = 10) {
  layers <- list(
      show_grid_lines(xlim, breaks),
      scale_colour_manual(values = colours),
      scale_fill_manual(values = colours),
      facet_wrap(~ species, scale = 'free_y', ncol = ncol),
  theme(legend.position = 'top',
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.spacing = unit(-0.1, "lines")
        ),
  guides(color = guide_legend(direction = "horizontal"),
    fill = 'none'),
  labs(x = 'Year', y = 'Relative biomass index'),
  coord_cartesian(xlim = c(1975, 2022) + c(-0.75, 0.75),
      ylim = c()
    expand = FALSE)
  )
}

show_grid_lines <- function(xlim = c(1975 - 0.2, 2022 + 0.2), breaks = 10)
  list(geom_vline(xintercept = seq(xlim[1], xlim[2]), col = "grey96", lwd = 0.3),
       geom_vline(xintercept = seq(gfplot:::mround(xlim[1], breaks), xlim[2], breaks),
        col = "grey93")
     )

show_text <- function(stat_df) {
  list(
    geom_text(data = stat_df, aes(label = cv),
      x = 1975 + 0.5, y = 0.87,
      colour = "grey35", size = 2.65, hjust = 0),
    geom_text(data = stat_df, aes(label = sets),
        x = 1975 + 0.5, y = 0.69,
        colour = "grey35", size = 2.65, hjust = 0)
  )
}

# Compare 2km and 3km grid indices
# grid_bin_ind_plot <- bind_rows(mssm_2km_inds, mssm_3km_inds) |>
#   order_spp() |>
#   ggplot(data = _, aes(x = year, y = biomass)) +
#     geom_line(aes(colour = grid)) +
#     geom_point(aes(colour = grid)) +
#     geom_ribbon(aes(ymin = lowerci, ymax = upperci, fill = grid), alpha = 0.3) +
#     geom_rect(data = . %>% filter(extreme_uci == TRUE | mean_cv > 2) %>%
#       distinct(species, grid, .keep_all = TRUE),
#       mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
#       fill = "gray85", alpha = 0.3) +
#     scale_color_brewer(palette = "Dark2") +
#     scale_fill_brewer(palette = "Dark2" ) +
#     facet_wrap(~ species, scales = "free_y", nrow = 6) +
#     #ggtitle("Comparing modeled index from 2x2 km and 3x3 km grids") +
#     theme(legend.position = c(0.5, 1.03),
#           axis.text.y = element_blank()) +
#     guides(color = guide_legend(direction = "horizontal"), fill = guide_legend(direction = "horizontal")) +
#     labs(colour = "Model", fill = "Model") +
#     labs(x = 'Year', y = 'Relative biomass index')
# grid_bin_ind_plot

# ggsave(file.path(mssm_figs, '2km-3km-grid-model-comp.png'), plot = grid_bin_ind_plot,
#   width = 10.5, height = 8)

# MSSM Design only ------------------------
mssm_design_only_inds <- scaled_inds |>
  filter(species %in% spp_in_mssm_design_only,
         survey_abbrev %in% c("MSSM Design")) |>
ggplot(data = _, aes(x = year, y = mssm_scaled_biomass, colour = survey_abbrev, fill = survey_abbrev)) +
  show_grid_lines() +
  geom_rect(data = . %>% distinct(species, .keep_all = TRUE),
      mapping = aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf),
      fill = "gray85", alpha = 0.3) +
  geom_pointrange(aes(ymin = mssm_scaled_lowerci, ymax = mssm_scaled_upperci),
    size = 0.2, alpha = 0.7, shape = 21, fill = 'grey95') +
  geom_ribbon(aes(ymin = mssm_scaled_lowerci, ymax = mssm_scaled_upperci), size = 0.2, alpha = 0.1, colour = NA) +
  scale_colour_manual(values = survey_cols) +
  scale_fill_manual(values = survey_cols) +
  facet_wrap(~ species, scale = 'free_y', ncol = 4) +
  theme(legend.position = c(0.5, 1.055),
        axis.text.y = element_blank(),
        legend.title = element_blank()
        ) +
  guides(colour = 'none', fill = 'none') +
  labs(x = 'Year', y = 'Relative biomass index')
mssm_design_only_inds

ggsave(file.path(mssm_figs, 'index-mssm-design.png'), plot = mssm_design_only_inds,
  width = 10.5, height = 8)

# MSSM Model - Design ------------------------
# mssm_model_design_inds <- scaled_inds |>
#   filter(species %in% spp_in_mssm,
#          survey_abbrev %in% c("MSSM Model")) |>
# ggplot(data = _, aes(x = year, y = mssm_scaled_biomass, colour = survey_abbrev, fill = survey_abbrev)) +
#   ind_layers(ncol = 4) +
#   geom_line(aes(colour = 'MSSM Model')) +
#   geom_ribbon(aes(ymin = mssm_scaled_lowerci, ymax = mssm_scaled_upperci), colour = NA, alpha = 0.3) +
#   geom_pointrange(data = scaled_inds |>
#     filter(species %in% spp_in_mssm, survey_abbrev == "MSSM Design") |>
#     filter(year != 2022),
#     aes(ymin = mssm_scaled_lowerci, ymax = mssm_scaled_upperci),
#     size = 0.2, alpha = 0.7, shape = 21, fill = 'grey95') +
#   guides(shape = 'none')
# mssm_model_design_inds

# ggsave(file.path(mssm_figs, 'index-model-design.png'), plot = mssm_model_design_inds,
#   width = 10.5, height = 10)

# ---- Use what was done with plot-indices -----
# Scale to max-CI of the modelled index and allow the design CIs to go off the plot
scaled_mssm <- raw_inds |>
  filter(species %in% spp_in_mssm,
    survey_abbrev %in% c('MSSM Model', 'MSSM Design')) |>
  scale_geo_design(survey1  = 'MSSM Model', survey2 = 'MSSM Design')

mssm_stats <- tidy_stats_df(scaled_mssm) |>
  pivot_wider(id_cols = species,
    names_from = 'survey_abbrev',
    values_from = c('mean_cv', 'n_pos_sets', 'n_sets')) |>
    mutate(sets = paste0("Mean +ve sets", ": ", `n_pos_sets_MSSM Model`, "/", `n_sets_MSSM Model`)) %>%
    mutate(cv = paste0("Mean", " CV: ", `mean_cv_MSSM Model`, '(M), ', `mean_cv_MSSM Design`, '(D)'))

mssm_model_design_inds <- ggplot() +
  show_grid_lines() +
  geom_line(data = scaled_mssm |>
    filter(species %in% spp_in_mssm, survey_abbrev == "MSSM Model"),
    aes(x = year, y = biomass_scaled, colour = 'MSSM Model')) +
  geom_ribbon(data = scaled_mssm |>
    filter(species %in% spp_in_mssm, survey_abbrev == "MSSM Model"),
    aes(fill = 'MSSM Model', x = year, ymin = lowerci_scaled, ymax = upperci_scaled), colour = NA, alpha = 0.3) +
  geom_pointrange(data = scaled_mssm |>
    filter(species %in% spp_in_mssm, survey_abbrev == "MSSM Design"),
    aes(x = year, y = biomass_scaled, ymin = lowerci_scaled, ymax = upperci_scaled, colour = 'MSSM Design'),
    size = 0.2, alpha = 0.7, shape = 21, fill = 'grey95') +
  scale_colour_manual(values = survey_cols) +
  scale_fill_manual(values = survey_cols) +
  facet_wrap(~ species, scale = 'free_y', ncol = 4) +
  theme(legend.position = 'top',
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.spacing = unit(-0.1, "lines")
        ) +
  guides(color = guide_legend(direction = "horizontal"),
    fill = 'none') +
  labs(x = 'Year', y = 'Relative biomass index') +
  coord_cartesian(ylim = c(-0.004, 1.05), xlim = c(1975, 2022) + c(-0.75, 0.75),
    expand = FALSE
  ) +
  show_text(mssm_stats)
mssm_model_design_inds

ggsave(file.path(mssm_figs, 'index-model-design_pjs-mode.png'),
  width = 8, height = 11)

# ----
# SYN WCVI vs MSSM
stats_df <- tidy_stats_df(scaled_inds |>
  filter(species %in% spp_in_mssm,
         survey_abbrev %in% c("MSSM Model", "SYN WCVI"))) |>
  mutate(prop_pos = round(as.numeric(n_pos_sets) / as.numeric(n_sets), digits = 2)) |>
  pivot_wider(id_cols = species,
    names_from = 'survey_abbrev',
    values_from = c('mean_cv', 'n_pos_sets', 'n_sets', 'prop_pos')) |>
    mutate(sets = paste0("Mean +ve sets", ": ", `prop_pos_MSSM Model`, '(M), ', `prop_pos_SYN WCVI`, '(S)')) %>%
    mutate(cv = paste0("Mean", " CV: ", `mean_cv_MSSM Model`, '(M), ', `mean_cv_SYN WCVI`, '(S)'))

test <- raw_inds |>
  filter(species %in% spp_in_mssm,
    survey_abbrev %in% c('MSSM Model', 'SYN WCVI')) |>
  scale_geo_design(survey1  = 'MSSM Model', survey2 = 'SYN WCVI')


mssm_syn_inds <- max_ci_scaled |>
  filter(comp == c("MSSM Model-SYN WCVI")) |>
ggplot(data = _) +
  ind_layers() +
  geom_line(aes(x = year, y = biomass_scaled, colour = survey_abbrev)) +
  geom_ribbon(aes(x = year, ymin = lowerci_scaled, ymax = upperci_scaled, fill = survey_abbrev),
    colour = NA, alpha = 0.3) +
  show_text(stats_df) +
  geom_rect(data = . %>% filter(!(tolower(species) %in% unique(syn_wcvi_inds$species))) %>%
    distinct(species, .keep_all = TRUE),
    mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill = "white", colour = NA) +
  ylim(c(-0.004, 1.03))
mssm_syn_inds


mssm_syn_inds <-

scaled_inds |>
  filter(species %in% spp_in_mssm,
         survey_abbrev %in% c("MSSM Model", "SYN WCVI")) |>
ggplot(data = _, aes(x = year, y = syn_scaled_biomass)) +
  ind_layers() +
  geom_line(aes(colour = survey_abbrev)) +
  geom_ribbon(aes(ymin = syn_scaled_lowerci, ymax = syn_scaled_upperci, fill = survey_abbrev), colour = NA, alpha = 0.3) +
  geom_rect(data = . %>% filter(!(tolower(species) %in% unique(syn_wcvi_inds$species))) %>%
    distinct(species, .keep_all = TRUE),
    mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill = "white", colour = NA) +
      geom_text(data = stats_df, aes(label = cv),
      x = 1975 + 0.5, y = 0.67,
      colour = "grey35", size = 2.65, hjust = 0)+
    geom_text(data = stats_df, aes(label = sets),
        x = 1975 + 0.5, y = 0.49,
        colour = "grey35", size = 2.65, hjust = 0)


mssm_syn_inds

ggsave(file.path(mssm_figs, 'index-mssm-model-syn-wcvi-model.png'), plot = mssm_syn_inds,
  width = 10.5, height = 10)

mssm_syn_inds_mssm_grid <- scaled_inds |>
  filter(species %in% spp_in_mssm,
         survey_abbrev %in% c("MSSM Model", "SYN WCVI on MSSM Grid")) |>
ggplot(data = _, aes(x = year, y = syn_scaled_biomass, colour = survey_abbrev, fill = survey_abbrev)) +
  ind_layers(ncol = 4) +
  geom_line() +
  geom_ribbon(aes(ymin = syn_scaled_lowerci, ymax = syn_scaled_upperci), colour = NA, alpha = 0.3) +
  geom_rect(data = . %>% filter(!(tolower(species) %in% unique(syn_wcvi_inds$species))) %>%
    distinct(species, .keep_all = TRUE),
    mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill = "white", colour = NA) +
  labs(x = 'Year', y = 'Relative biomass index')

mssm_syn_inds_mssm_grid

ggsave(file.path(mssm_figs, 'index-mssm-model-syn-wcvi-model-mssm-grid.png'), plot = mssm_syn_inds_mssm_grid,
  width = 10.5, height = 10)


mssm_syn_grid_zoom_in <- scaled_inds |>
  filter(species %in% spp_in_mssm,
         survey_abbrev %in% c("MSSM Model", "SYN WCVI", "SYN WCVI on MSSM Grid")) |>
  filter(year > 2003) |>
  filter(species != 'shiner perch') |>
ggplot(data = _, aes(x = year, y = syn_scaled_biomass, colour = survey_abbrev, fill = survey_abbrev)) +
  geom_ribbon(aes(ymin = syn_scaled_lowerci, ymax = syn_scaled_upperci), colour = NA, alpha = 0.15) +
  ind_layers(colours = c("SYN WCVI" = "#7570b3", "SYN WCVI on MSSM Grid" = "#a6761d",
    "MSSM Model" = "#1b9e77"),
    ncol = 4)
mssm_syn_grid_zoom_in
ggsave(file.path(mssm_figs, 'index-mssm-syn-wcvi-mssm-grid-zoom-in.png'), plot = mssm_syn_grid_zoom_in,
  width = 9.5, height = 11)

mssm_cpue_inds <- scaled_inds |>
  filter(species %in% spp_in_mssm,
         survey_abbrev %in% c("MSSM Model", "CPUE 3CD")) |>
ggplot(data = _, aes(x = year, y = cpue_scaled_biomass, colour = survey_abbrev, fill = survey_abbrev)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = cpue_scaled_lowerci, ymax = cpue_scaled_upperci), colour = NA, alpha = 0.3) +
  geom_vline(xintercept = c(2003), colour = 'grey80') +
  scale_colour_manual(values = survey_cols) +
  scale_fill_manual(values = survey_cols) +
  facet_wrap(~ species, scale = 'free_y') +
  #ggtitle("Comparison of Modelled MSSM and CPUE 3CD") +
  ind_layers() +
  geom_rect(data = . %>% filter(!(species %in% unique(cpue_ind$species))) %>%
    distinct(species, .keep_all = TRUE),
    mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill = "white", colour = NA)
mssm_cpue_inds

ggsave(file.path(mssm_figs, 'index-mssm-model-cpue3CD.png'), plot = mssm_cpue_inds,
  width = 10.5, height = 8)
