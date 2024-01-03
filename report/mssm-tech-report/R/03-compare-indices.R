if (!('indices_loaded' %in% ls())) {
  source(here::here('report', 'mssm-tech-report', 'R', '02-load-indices.R'))
}

ind_layers <- function(colours = survey_cols, ncol = 3,
  xlim = c(1975 - 0.75, 2022 + 0.75), breaks = 10, hide_y = TRUE,
  add_lines = TRUE) {
  layers <- list(
      show_grid_lines(xlim, breaks),
      scale_colour_manual(values = colours),
      scale_fill_manual(values = colours),
      facet_wrap(~ species, scale = 'free_y', ncol = ncol),
  theme(#legend.position = 'top',
        legend.position = c(0.82, 0.05),
        legend.title = element_blank(),
        axis.text.y = if (hide_y) element_blank() else element_text(),
        axis.ticks.y = element_blank(),
        panel.spacing = unit(-0.1, "lines")
        ),
  guides(colour = guide_legend(direction = "vertical"),
    fill = 'none'),
  labs(x = 'Year', y = 'Relative biomass index'),
  coord_cartesian(xlim = xlim,
      ylim = (c(-0.004, 1.03)),
    expand = FALSE)
  )

  if (add_lines) {
    layers <- c(layers,
      geom_line(aes(x = year, y = biomass_scaled, colour = survey_abbrev)),
      geom_ribbon(aes(x = year, ymin = lowerci_scaled, ymax = upperci_scaled,
        fill = survey_abbrev), colour = NA, alpha = 0.3)
    )
  }

  return(layers)
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
d_only_stats <- stats_df |>
  filter(comp == c("MSSM Design-MSSM Design")) |>
  mutate(survey_abbrev = 'MSSM Design',
         sets = gsub('( \\(M\\), .*)', '', sets),
         cv = gsub('( \\(M\\), .*)', '', cv)) |>
  select(species, survey_abbrev, sets, cv)

mssm_model_design_inds <- max_ci_scaled |>
  filter(comp == c("MSSM Design-MSSM Design")) |>
  ggplot() +
  geom_rect(data = . %>% distinct(species, .keep_all = TRUE),
      mapping = aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf),
      fill = "gray85", alpha = 0.3) +
  show_grid_lines() +
  ind_layers(add_lines = FALSE) +
  geom_ribbon(aes(fill = 'MSSM Design', x = year,
      ymin = lowerci_scaled, ymax = upperci_scaled),
    colour = NA, alpha = 0.3) +
  geom_pointrange(aes(x = year, y = biomass_scaled,
      ymin = lowerci_scaled, ymax = upperci_scaled, colour = 'MSSM Design'),
    size = 0.2, alpha = 0.7, shape = 21, fill = 'grey95') +
  show_text(d_only_stats) +
  guides(colour = 'none')
mssm_model_design_inds

ggsave(file.path(mssm_figs, 'index-mssm-design.png'), width = 7.5, height = 7)

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
mssm_stats <- stats_df |>
  filter(comp == c("MSSM Model-MSSM Design")) |>
  mutate(sets = gsub('( \\(M\\), .*)', '', sets))

mssm_model_design_inds <-  max_ci_scaled |>
  filter(comp == c("MSSM Model-MSSM Design")) |>
  ggplot() +
  geom_rect(data = . %>% distinct(species, .keep_all = TRUE),
      mapping = aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf),
      fill = "gray85", alpha = 0.3) +
  show_grid_lines() +
  ind_layers(add_lines = FALSE) +
  geom_line(data = max_ci_scaled |>
    filter(comp == c("MSSM Model-MSSM Design"), survey_abbrev == "MSSM Model"),
    aes(x = year, y = biomass_scaled, colour = 'MSSM Model')) +
  geom_ribbon(data = max_ci_scaled |>
    filter(comp == c("MSSM Model-MSSM Design"), survey_abbrev == "MSSM Model"),
    aes(fill = 'MSSM Model', x = year, ymin = lowerci_scaled, ymax = upperci_scaled), colour = NA, alpha = 0.3) +
  geom_pointrange(data = max_ci_scaled |>
    filter(comp == c("MSSM Model-MSSM Design"), survey_abbrev == "MSSM Design"),
    aes(x = year, y = biomass_scaled, ymin = lowerci_scaled, ymax = upperci_scaled, colour = 'MSSM Design'),
    size = 0.2, alpha = 0.7, shape = 21, fill = 'grey95') +
  show_text(mssm_stats)
mssm_model_design_inds

ggsave(file.path(mssm_figs, 'index-model-design_pjs-mode.png'),
  width = 7.5, height = 9.5)

# ----
# SYN WCVI vs MSSM
mssm_syn_inds <- max_ci_scaled |>
  filter(comp == c("MSSM Model-SYN WCVI")) |>
ggplot(data = _) +
  geom_rect(data = . %>% distinct(species, .keep_all = TRUE),
      mapping = aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf),
      fill = "gray85", alpha = 0.3) +
  ind_layers() +
  show_text(stats_df |> filter(comp == c("MSSM Model-SYN WCVI"))) +
  geom_rect(data = . %>% filter(!(tolower(species) %in% unique(syn_wcvi_inds$species))) %>%
    distinct(species, .keep_all = TRUE),
    mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill = "white", colour = NA) +
  ylim(c(-0.004, 1.03))
mssm_syn_inds

ggsave(file.path(mssm_figs, 'index-mssm-model-syn-wcvi-model.png'),
  width = 7.5, height = 9.5)

mssm_syn_inds_mssm_grid <- max_ci_scaled |>
  filter(comp == c("MSSM Model-SYN WCVI on MSSM Grid")) |>
ggplot(data = _) +
  geom_rect(data = . %>% distinct(species, .keep_all = TRUE),
      mapping = aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf),
      fill = "gray85", alpha = 0.3) +
  ind_layers() +
  show_text(stats_df |> filter(comp == c("MSSM Model-SYN WCVI on MSSM Grid"))) +
  geom_rect(data = . %>% filter(!(tolower(species) %in% unique(syn_wcvi_inds$species))) %>%
    distinct(species, .keep_all = TRUE),
    mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill = "white", colour = NA) +
  ylim(c(-0.004, 1.03))
mssm_syn_inds_mssm_grid

ggsave(file.path(mssm_figs, 'index-mssm-model-syn-wcvi-model-mssm-grid.png'),
  width = 7.5, height = 9.5)

mssm_syn_grid_zoom_in <- tibble(
  'survey1' = rep('MSSM Model', 2),
  'survey2' = c('SYN WCVI', 'SYN WCVI on MSSM Grid')
) |>
purrr::pmap(\(survey1, survey2)
  scale_geo_design(raw_inds |> filter(species %in% spp_in_mssm, year > 2003),
    survey1, survey2)
) |>
  bind_rows() |>
  filter(comp %in% c("MSSM Model-SYN WCVI on MSSM Grid", 'MSSM Model-SYN WCVI')) |>
  distinct() |>
  filter(species != 'shiner perch') |>
ggplot(data = _) +
  ind_layers(colours = c("SYN WCVI" = "#7570b3", "SYN WCVI on MSSM Grid" = "#a6761d",
    "MSSM Model" = "#1b9e77"),
    xlim = c(2003 - 0.2, 2022 + 0.2),
    ncol = 3) +
  guides(colour = guide_legend(direction = "vertical"),
    fill = 'none') +
  geom_rect(data = . %>% filter(species == 'Shiner Perch') %>%
    distinct(species, .keep_all = TRUE),
    mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill = "white", colour = NA)
mssm_syn_grid_zoom_in

ggsave(file.path(mssm_figs, 'index-mssm-syn-wcvi-mssm-grid-zoom-in.png'),
  width = 7.5, height = 9.5)

# MSSM ~ CPUE 3CD
mssm_cpue_inds <- max_ci_scaled |>
  filter(comp == c("MSSM Model-CPUE 3CD")) |>
ggplot(data = _) +
  geom_rect(data = . %>% distinct(species, .keep_all = TRUE),
      mapping = aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf),
      fill = "gray85", alpha = 0.3) +
  ind_layers() +
  show_text(stats_df |> filter(comp == c("MSSM Model-CPUE 3CD"))) +
  geom_rect(data = . %>% filter(!(tolower(species) %in% unique(cpue_ind$species))) %>%
    distinct(species, .keep_all = TRUE),
    mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill = "white", colour = NA) +
  ylim(c(-0.004, 1.03))
  mssm_cpue_inds

ggsave(file.path(mssm_figs, 'index-mssm-model-cpue3CD.png'), plot = mssm_cpue_inds,
  width = 7.5, height = 9.5)
