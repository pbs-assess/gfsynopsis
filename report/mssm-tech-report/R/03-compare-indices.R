if (!indices_loaded) {
  source(here::here('report', 'mssm-tech-report', '02-load-indices.R'))
}

# Compare 2km and 3km grid indices
grid_bin_ind_plot <- bind_rows(mssm_2km_inds, mssm_3km_inds) |>
  order_spp() |>
  ggplot(data = _, aes(x = year, y = biomass)) +
    geom_line(aes(colour = grid)) +
    geom_point(aes(colour = grid)) +
    geom_ribbon(aes(ymin = lowerci, ymax = upperci, fill = grid), alpha = 0.3) +
    geom_rect(data = . %>% filter(extreme_uci == TRUE | mean_cv > 2) %>%
      distinct(species, grid, .keep_all = TRUE),
      mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
      fill = "gray85", alpha = 0.3) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2" ) +
    facet_wrap(~ species, scales = "free_y", nrow = 6) +
    #ggtitle("Comparing modeled index from 2x2 km and 3x3 km grids") +
    theme(legend.position = c(0.5, 1.03),
          axis.text.y = element_blank()) +
    guides(color = guide_legend(direction = "horizontal"), fill = guide_legend(direction = "horizontal")) +
    labs(colour = "Model", fill = "Model") +
    labs(x = 'Year', y = 'Relative biomass index')
grid_bin_ind_plot

ggsave(file.path(mssm_figs, '2km-3km-grid-model-comp.png'), plot = grid_bin_ind_plot,
  width = 10.5, height = 8)

# Compare MSSM and SYN WCVI and 3CD CPUE --------------------------------------

ind_layers <- function(colours = survey_cols, ncol = 5) {
  layers <- list(
      geom_line(),
      geom_point(),
      scale_colour_manual(values = colours),
      scale_fill_manual(values = colours),
      facet_wrap(~ species, scale = 'free_y', ncol = ncol),
      theme(legend.position = 'top',
            legend.title = element_blank(),
            axis.text.y = element_blank()),
      guides(color = guide_legend(direction = "horizontal"), fill = guide_legend(direction = "horizontal")),
      labs(x = 'Year', y = 'Relative biomass index')
    )
}

mssm_design_only_inds <-
  scaled_inds |>
  filter(species %in% spp_in_mssm_design_only,
         survey_abbrev %in% c("MSSM Design")) |>
ggplot(data = _, aes(x = year, y = mssm_scaled_biomass, colour = survey_abbrev, fill = survey_abbrev)) +
  geom_pointrange(aes(ymin = mssm_scaled_lowerci, ymax = mssm_scaled_upperci), size = 0.2) +
  geom_ribbon(aes(ymin = mssm_scaled_lowerci, ymax = mssm_scaled_upperci), size = 0.2, alpha = 0.1, colour = NA) +
  scale_colour_manual(values = survey_cols) +
  scale_fill_manual(values = survey_cols) +
  facet_wrap(~ species, scale = 'free_y', ncol = 4) +
  theme(legend.position = c(0.5, 1.055),
        legend.title = element_blank(),
        axis.text.y = element_blank()) +
  # geom_rect(data = . %>% distinct(species, .keep_all = TRUE),
  #     mapping = aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf),
  #     fill = "gray85", alpha = 0.3) +
  guides(colour = 'none', fill = 'none')
  #ggtitle("Design based index only") +
mssm_design_only_inds

ggsave(file.path(mssm_figs, 'index-mssm-design.png'), plot = mssm_design_only_inds,
  width = 10.5, height = 8)

mssm_model_design_inds <- scaled_inds |>
  filter(species %in% spp_in_mssm,
         survey_abbrev %in% c("MSSM Model")) |>
ggplot(data = _, aes(x = year, y = mssm_scaled_biomass, colour = survey_abbrev, fill = survey_abbrev)) +
  geom_ribbon(aes(ymin = mssm_scaled_lowerci, ymax = mssm_scaled_upperci), colour = NA, alpha = 0.3) +
  ind_layers() +
  geom_pointrange(data = scaled_inds |> filter(species %in% spp_in_mssm, survey_abbrev == "MSSM Design"),
    aes(ymin = mssm_scaled_lowerci, ymax = mssm_scaled_upperci), size = 0.2, alpha = 0.7)
  #ggtitle("Comparison of Modelled and Design based index")
mssm_model_design_inds

ggsave(file.path(mssm_figs, 'index-model-design.png'), plot = mssm_model_design_inds,
  width = 10.5, height = 10)

mssm_syn_inds <- scaled_inds |>
  filter(species %in% spp_in_mssm,
         survey_abbrev %in% c("MSSM Model", "SYN WCVI")) |>
ggplot(data = _, aes(x = year, y = syn_scaled_biomass, colour = survey_abbrev, fill = survey_abbrev)) +
  geom_ribbon(aes(ymin = syn_scaled_lowerci, ymax = syn_scaled_upperci), colour = NA, alpha = 0.3) +
  ind_layers() +
geom_rect(data = . %>% filter(!(species %in% unique(syn_inds$species))) %>%
    distinct(species, .keep_all = TRUE),
    mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill = "white", colour = NA)
mssm_syn_inds

ggsave(file.path(mssm_figs, 'index-mssm-model-syn-wcvi-model.png'), plot = mssm_syn_inds,
  width = 10.5, height = 10)

mssm_syn_inds_mssm_grid <- scaled_inds |>
  filter(species %in% spp_in_mssm,
         survey_abbrev %in% c("MSSM Model", "SYN WCVI on MSSM Grid")) |>
ggplot(data = _, aes(x = year, y = syn_scaled_biomass, colour = survey_abbrev, fill = survey_abbrev)) +
  geom_ribbon(aes(ymin = syn_scaled_lowerci, ymax = syn_scaled_upperci), colour = NA, alpha = 0.3) +
  ind_layers() +
  geom_rect(data = . %>% filter(!(species %in% unique(syn_inds$species))) %>%
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
