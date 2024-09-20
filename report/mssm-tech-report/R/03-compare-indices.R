if (!('indices_loaded' %in% ls())) {
  source(here::here('report', 'mssm-tech-report', 'R', '02-load-indices.R'))
}

cpue_hex_dat <- function(cpue_dat, bin_width, start_year, n_minimum_vessels = 3) {
  dx <- bin_width / 2
  dy <- bin_width / 2

  d <- cpue_dat |>
    gfplot::plot_catch_spatial(start_year, bin_width = bin_width, n_minimum_vessels = n_minimum_vessels, return_data = T) |>
    mutate()

  dat <- lapply(
    seq_len(nrow(d)), function(i) {
    data.frame(
      hex_id = i, catch = d[i, "value"],
      gfplot:::hex_coords(d[i, "x"], d[i, "y"], dx, dy))
  })
  dat <- do.call(rbind, dat)

  dat |>
    mutate(x = 1000 * x, y = 1000 * y) |>
    st_as_sf(coords = c('x', 'y')) |>
    dplyr::group_by(hex_id, catch) |>
    dplyr::summarise() |>
    st_cast("POLYGON") |>
    st_convex_hull() |>
    mutate(bin_width_km = bin_width) |>
    st_set_crs(32609)
}

ind_layers <- function(colours = survey_cols, ncol = 3,
  xlim = c(1975 - 0.75, 2022 + 0.75), breaks = 10, hide_y = TRUE,
  add_lines = TRUE, ribbon_alpha = 0.3) {
  layers <- list(
      show_grid_lines(xlim, breaks),
      scale_colour_manual(values = colours),
      scale_fill_manual(values = colours),
      facet_wrap(~ species, scale = 'free_y', ncol = ncol),
  theme(legend.position = c(0.82, 0.05),
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
        fill = survey_abbrev), colour = NA, alpha = ribbon_alpha)
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

# ----------------------------------------------------------------
# Maps to compare spatial coverage/extent of SMMS, WCVI SYN, CPUE
# ----------------------------------------------------------------
map_survey_levels <- c("SMMS", "SYN WCVI", "CPUE 3CD")

syn_wcvi_grid <- gfdata::survey_blocks |>
  filter(survey_abbrev == "SYN WCVI", active_block == TRUE) |>
  st_union() |>
  st_sf() |>
  mutate(survey = "SYN WCVI") |>
  mutate(survey = factor(survey, levels = map_survey_levels))
  #mutate(colour = survey_cols["SYN WCVI"][[1]])

mssm_grid_poly <- mssm_grid_sf |>
  st_union() |>
  st_sf() |>
  mutate(survey = "SMMS") |>
  mutate(survey = factor(survey, levels = map_survey_levels))

if (!file.exists(file.path(mssm_data_out, 'cpue-raw.rds'))) {
  d_cpue <- readRDS(file.path(here::here("report", "data-cache-2024-05"), "cpue-index-dat.rds"))
  # data(pmfc, package = "PBSdata") # so that I know what the major regions correspond to
  cpue_raw <- d_cpue |>
    distinct(fishing_event_id, .keep_all = TRUE) |>
    mutate(catch = catch_kg, year = lubridate::year(best_date),
          vessel_registration_number = as.numeric(vessel_registration_number),
          fishing_event_id = as.numeric(fishing_event_id)) |>
    rename(lon = "longitude", lat = "latitude") |>
    drop_na(lon, lat) |>
    filter(major_stat_area_code %in% c("03", "04")) |>
    mutate(survey = "CPUE 3CD") |>
    mutate(survey = factor(survey, levels = map_survey_levels))
  saveRDS(cpue_raw, file.path(mssm_data_out, 'cpue-raw.rds'))
}
cpue_raw <- readRDS(file.path(mssm_data_out, 'cpue-raw.rds'))

cpue_hex <- cpue_hex_dat(cpue_dat = cpue_raw, start_year = 1996, bin_width = 7,
  n_minimum_vessels = 3) |>
  mutate(survey = "CPUE 3CD") |>
  mutate(survey = factor(survey, levels = map_survey_levels))

cpue_hex_post_2013 <- cpue_hex_dat(cpue_dat = cpue_raw, start_year = 2013, bin_width = 7,
  n_minimum_vessels = 3) |>
  st_union() |>
  st_sf() |>
  mutate(survey = "CPUE 3CD") |>
  mutate(survey = factor(survey, levels = map_survey_levels))

plot_survey_regions <- function() { ggplot() +
  geom_sf(data = pacea::bc_coast) +
  # scale_fill_identity(guide = "legend", labels = c("SMMS", "WCVI SYN", "CPUE 3CD"), name = "Survey region") +
  ggspatial::annotation_scale(location = "bl", style = "ticks") +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
    style = ggspatial::north_arrow_orienteering(fill = c("black", "black"), line_width = 0.5, text_size = -1),
    # pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
    height = unit(0.4, "cm"), width = unit(0.4, "cm")) +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme(axis.title = element_blank(),
    # legend.position = c(0.17, 0.35),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.title = element_blank(),
    plot.margin = margin(c(0, 0, 0, 1))
  ) +
  scale_fill_manual(values = survey_cols) +
  scale_y_continuous(breaks = c(48, 49, 50)) +
  scale_x_continuous(breaks = c(-129, -127, -125, -123))
}

p1 <- plot_survey_regions() +
  geom_sf(data = syn_wcvi_grid, aes(fill = survey), colour = NA, alpha = 0.75) +
  geom_sf(data = mssm_grid_poly, aes(fill = survey), colour =  "#105c46", alpha = 0.5, linewidth = 0.7) +
  coord_sf(y = c(48, 50.8), x = c(-129.5, -123.5), crs = "WGS84", expand = FALSE)

p2 <- plot_survey_regions() +
  geom_sf(data = cpue_hex |> st_difference(st_union(cpue_hex_post_2013)), fill = NA, colour = "grey50", linewidth = 0.5) +
  geom_sf(data = cpue_hex_post_2013, aes(fill = survey), colour = NA, alpha = 0.75) +
  geom_sf(data = mssm_grid_poly, aes(fill =  survey), colour =  "#105c46", alpha = 0.5, linewidth = 0.7) +
  coord_sf(y = c(48, 50.8), x = c(-129.5, -123.5), crs = "WGS84", expand = FALSE) +
  theme(axis.text.y = element_blank()) +
  guides(colour = "none")

(p1 + p2) +
  plot_layout(tag_level = "keep") +
  plot_annotation(tag_levels = "a", tag_suffix = ")") &
  theme(plot.tag.location = "plot", plot.tag = element_text(vjust = -1.5))

ggsave(width = 7.7, height = 3.8, filename = file.path(mssm_figs, 'map-mssm-syn-wcvi-cpue-3cd.png'))
# ----


# Check doorspread
# In 2016 and 2017, doorspreads were assigned 29.6 m,

mean_doorspread <- spp_dat |>
  filter(survey_abbrev == "SMMS WCVI", year > 2013, doorspread_m != 0) |>
  filter(!(year %in% 2016:2017)) |>
  group_by(year) |>
  summarise(mean_door = round(mean(doorspread_m, na.rm = TRUE), digits = 1),
    max_count = max(hist(doorspread_m, plot = FALSE)$counts))

spp_dat |>
  filter(survey_abbrev == "SMMS WCVI") |>
  filter(!(year %in% 2016:2017)) |>
  filter(year > 2013, doorspread_m != 0) |> # years where electronic monitoring of doorspread was collectedd
ggplot() +
  geom_histogram(aes(x = doorspread_m), fill = "grey50", binwidth = 0.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 4), minor_breaks = NULL) +
  facet_wrap(~ year, scales = "free_y") +
  geom_vline(data = mean_doorspread, aes(xintercept = mean_door)) +
  geom_text(data = mean_doorspread, aes(x = Inf, y = Inf,
    label = paste0("mean = ", mean_door), vjust = 2, hjust = 1.05),
    size = 3.5) +
  labs(x = "Doorspread (m)", y = "Count")
ggsave(width = 7.7, height = 3, filename = file.path(mssm_figs, 'doorspread-hist.png'))
# ----
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
  filter(comp == c("SMMS Design-SMMS Design")) |>
  mutate(survey_abbrev = 'SMMS Design',
         sets = gsub('( \\(M\\), .*)', '', sets),
         cv = gsub('( \\(M\\), .*)', '', cv)) |>
  select(species, survey_abbrev, sets, cv)

mssm_model_design_inds <- max_ci_scaled |>
  filter(comp == c("SMMS Design-SMMS Design")) |>
  ggplot() +
  geom_rect(data = . %>% distinct(species, .keep_all = TRUE),
      mapping = aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf),
      fill = "gray85", alpha = 0.3) +
  show_grid_lines() +
  ind_layers(add_lines = FALSE) +
  geom_ribbon(aes(fill = 'SMMS Design', x = year,
      ymin = lowerci_scaled, ymax = upperci_scaled),
    colour = NA, alpha = 0.3) +
  geom_pointrange(aes(x = year, y = biomass_scaled,
      ymin = lowerci_scaled, ymax = upperci_scaled, colour = 'SMMS Design'),
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
  filter(comp == c("SMMS Model-SMMS Design")) |>
  mutate(sets = gsub('( \\(M\\), .*)', '', sets))

mssm_model_design_inds <-  max_ci_scaled |>
  filter(comp == c("SMMS Model-SMMS Design")) |>
  ggplot() +
  geom_rect(data = . %>% distinct(species, .keep_all = TRUE),
      mapping = aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf),
      fill = "gray85", alpha = 0.4) +
  show_grid_lines() +
  ind_layers(add_lines = FALSE) +
  geom_line(data = max_ci_scaled |>
    filter(comp == c("SMMS Model-SMMS Design"), survey_abbrev == "SMMS Model"),
    aes(x = year, y = biomass_scaled, colour = 'SMMS Model')) +
  geom_ribbon(data = max_ci_scaled |>
    filter(comp == c("SMMS Model-SMMS Design"), survey_abbrev == "SMMS Model"),
    aes(fill = 'SMMS Model', x = year, ymin = lowerci_scaled, ymax = upperci_scaled), colour = NA, alpha = 0.3) +
  geom_pointrange(data = max_ci_scaled |>
    filter(comp == c("SMMS Model-SMMS Design"), survey_abbrev == "SMMS Design"),
    aes(x = year, y = biomass_scaled, ymin = lowerci_scaled, ymax = upperci_scaled, colour = 'SMMS Design'),
    size = 0.2, alpha = 0.7, shape = 21, fill = 'grey95') +
  show_text(mssm_stats)
mssm_model_design_inds +
  theme(plot.margin = margin(t = 0, b = 0),
        axis.title.y = element_text(vjust = 0.1))

ggsave(file.path(mssm_figs, 'index-model-design_pjs-mode.png'),
  width = 7.8, height = 8.75)

# ----
# SYN WCVI vs MSSM
mssm_syn_inds <- max_ci_scaled |>
  filter(comp == c("SMMS Model-SYN WCVI")) |>
ggplot(data = _) +
  geom_rect(data = . %>% distinct(species, .keep_all = TRUE),
      mapping = aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf),
      fill = "gray85", alpha = 0.3) +
  ind_layers() +
  show_text(stats_df |> filter(comp == c("SMMS Model-SYN WCVI"))) +
  geom_rect(data = . %>% filter(!(tolower(species) %in% unique(syn_wcvi_inds$species))) %>%
    distinct(species, .keep_all = TRUE),
    mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill = "white", colour = NA) +
  ylim(c(-0.004, 1.03))
mssm_syn_inds

ggsave(file.path(mssm_figs, 'index-mssm-model-syn-wcvi-model.png'),
  width = 7.5, height = 9.5)

mssm_syn_inds_mssm_grid <- max_ci_scaled |>
  filter(comp == c("SMMS Model-SYN WCVI on SMMS Grid")) |>
ggplot(data = _) +
  geom_rect(data = . %>% distinct(species, .keep_all = TRUE),
      mapping = aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf),
      fill = "gray85", alpha = 0.3) +
  ind_layers() +
  show_text(stats_df |> filter(comp == c("SMMS Model-SYN WCVI on SMMS Grid"))) +
  geom_rect(data = . %>% filter(!(tolower(species) %in% unique(syn_wcvi_inds$species))) %>%
    distinct(species, .keep_all = TRUE),
    mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill = "white", colour = NA) +
  ylim(c(-0.004, 1.03))
mssm_syn_inds_mssm_grid

ggsave(file.path(mssm_figs, 'index-mssm-model-syn-wcvi-model-mssm-grid.png'),
  width = 7.5, height = 9.5)

# mssm_syn_grid_zoom_in <- tibble(
#   'survey1' = rep('SMMS Model', 2),
#   'survey2' = c('SYN WCVI', 'SYN WCVI on SMMS Grid')
# ) |>
# purrr::pmap(\(survey1, survey2)
#   scale_geo_design(raw_inds |> filter(species %in% spp_in_mssm, year > 2003),
#     survey1, survey2)
# ) |>
#   bind_rows() |>
#   filter(comp %in% c("SMMS Model-SYN WCVI on SMMS Grid", 'SMMS Model-SYN WCVI')) |>
#   distinct() |>
#   filter(species != 'shiner perch') |>
# ggplot(data = _) +
#   ind_layers(colours = c("SYN WCVI" = "#7570b3", "SYN WCVI on SMMS Grid" = "#a6761d",
#     "SMMS Model" = "#1b9e77"),
#     xlim = c(2003 - 0.2, 2022 + 0.2),
#     ribbon_alpha = 0.15,
#     ncol = 3) +
#   guides(colour = guide_legend(direction = "vertical"),
#     fill = 'none') +
#   geom_rect(data = . %>% filter(species == 'Shiner Perch') %>%
#     distinct(species, .keep_all = TRUE),
#     mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
#     fill = "white", colour = NA)
# mssm_syn_grid_zoom_in

# ggsave(file.path(mssm_figs, 'index-mssm-syn-wcvi-mssm-grid-zoom-in.png'),
#   width = 7.5, height = 9.5)

# MSSM ~ CPUE 3CD
cpue_stats_df <- stats_df |>
  filter(comp == c("SMMS Model-CPUE 3CD")) |>
  select(species, sets, cv) |>
  mutate(sets = gsub('(, .*)', '', sets))

mssm_cpue_inds <- max_ci_scaled |>
  filter(comp == c("SMMS Model-CPUE 3CD")) |>
ggplot(data = _) +
  geom_rect(data = . %>% distinct(species, .keep_all = TRUE),
      mapping = aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf),
      fill = "gray85", alpha = 0.3) +
  ind_layers() +
  show_text(cpue_stats_df) +
  geom_rect(data = . %>% filter(!(tolower(species) %in% unique(cpue_ind$species))) %>%
    distinct(species, .keep_all = TRUE),
    mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill = "white", colour = NA) +
  ylim(c(-0.004, 1.03))
mssm_cpue_inds

ggsave(file.path(mssm_figs, 'index-mssm-model-cpue3CD.png'), plot = mssm_cpue_inds,
  width = 7.5, height = 9.5)
