if (!('mssm_loaded' %in% ls())) {
  source(here::here('report', 'mssm-tech-report', 'R', '00-load.R'))
}

if (!('indices_loaded' %in% ls())) {
  source(here::here('report', 'mssm-tech-report', 'R', '02-load-indices.R'))
}

if (!'size_dat') {
  source(here::here('report', 'mssm-tech-report', 'R', '03-age-size-frequencies.R'))
}

# --- Rolling window correlation ---
mssm_index_type <- "MSSM Design"
mssm_index_type <- "MSSM Model"
comp_index_type <- "CPUE 3CD"
comp_index_type <- "SYN WCVI"
comp_index_type <- "SYN WCVI on MSSM Grid"

get_mssm_cor <- function(mssm_index_type, comp_index_type, cor_thresh = 0.5, window = 10) {
  year_cutoff <- ifelse(grepl("SYN WCVI", comp_index_type), 2004, 1996)

  if ((comp_index_type == "CPUE 3CD")) {
    spp_intersect <- intersect(unique(mssm_3km_inds$species), unique(cpue_ind$species))
  } else {
    spp_intersect <- intersect(unique(mssm_3km_inds$species), unique(syn_inds$species))

  }

  mssm_cor_df <- scaled_inds |>
    filter(species %in% spp_intersect,
           survey_abbrev %in% c(mssm_index_type, comp_index_type),
           year >= year_cutoff) |>
    select(species, year, biomass, survey_abbrev) |>
    tidyr::pivot_wider(names_from = survey_abbrev, values_from = biomass)
  mssm_cor_list <- split(x = mssm_cor_df, mssm_cor_df$species) |>
    map(\(x) select(x, -species) |> arrange(year)) %>%
    keep(~ nrow(.x) > 0)

  # Correlation for full time series
  if (window == 0) {
    cor_log_biomass <- mssm_cor_list |>
      imap_dfr(\(dat, i) {
        cor_vals <- cor(log(dat[, mssm_index_type]), log(dat[, comp_index_type]))
        cor_df <- tibble(species = i, cor_val = cor_vals[[1]], input = 'log(biomass)')
      })
    cor_df <- cor_log_biomass |>
     mutate(comp = paste0(mssm_index_type, ' ~ ', comp_index_type))
  } else {
  # Rolling window correlation
    cor_biomass <- mssm_cor_list |>
      imap_dfr(\(dat, i) {
        cor_vals <- rollapply(dat, width = window, FUN = function(x) cor(x[, mssm_index_type], x[, comp_index_type]),
          by.column = FALSE)
        cor_df <- tibble(species = i, cor_vals = cor_vals, input = 'biomass', start_year = dat$year[1:length(cor_vals)])
      }
    )

    cor_log_biomass <- mssm_cor_list |>
      imap_dfr(\(dat, i) {
        cor_vals <- rollapply(dat, width = window, FUN = function(x) cor(log(x[, mssm_index_type]), log(x[, comp_index_type])),
          by.column = FALSE)
        cor_df <- tibble(species = i, cor_vals = cor_vals, input = 'log(biomass)', start_year = dat$year[1:length(cor_vals)])
      }
    )

    cor_df <- bind_rows(cor_biomass, cor_log_biomass) |>
      group_by(species, input) |>
      order_spp() |>
      drop_na()

    good_spp <- unique(cor_df$species)

    if (!is.null(cor_thresh)) {
      good_spp <- cor_df |>
        mutate(max_year = max(start_year)) |>
        filter(start_year > max_year - 5) |>
        group_by(species) |>
        summarise(mean_cor = mean(cor_vals), .groups = 'drop') |>
        filter(mean_cor >= cor_thresh) |>
        pluck('species')
    }

    cor_df |>
     filter(input == 'log(biomass)') |>
     filter(species %in% good_spp) |>
     mutate(comp = paste0(mssm_index_type, ' ~ ', comp_index_type)) |>
     ungroup()
   }
}

# cor_thresh = 0.25
#cor_thresh = 0.50
cor_thresh = NULL
cor1 <- get_mssm_cor(mssm_index_type = "MSSM Model", comp_index_type = "CPUE 3CD", cor_thresh = cor_thresh)
cor2 <- get_mssm_cor(mssm_index_type = "MSSM Design", comp_index_type = "CPUE 3CD", cor_thresh = cor_thresh)
cor3 <- get_mssm_cor(mssm_index_type = "MSSM Model", comp_index_type = "SYN WCVI", cor_thresh = cor_thresh)
cor4 <- get_mssm_cor(mssm_index_type = "MSSM Design", comp_index_type = "SYN WCVI", cor_thresh = cor_thresh)
cor5 <- get_mssm_cor(mssm_index_type = "MSSM Model", comp_index_type = "SYN WCVI on MSSM Grid", cor_thresh = cor_thresh)

full_cor1 <- get_mssm_cor(mssm_index_type = "MSSM Model", comp_index_type = "CPUE 3CD", cor_thresh = NULL, window = 0)
full_cor2 <- get_mssm_cor(mssm_index_type = "MSSM Model", comp_index_type = "SYN WCVI", cor_thresh = NULL, window = 0)
full_cor3 <- get_mssm_cor(mssm_index_type = "MSSM Model", comp_index_type = "SYN WCVI on MSSM Grid", cor_thresh = NULL, window = 0)

full_cor <- bind_rows(full_cor1, full_cor2, full_cor3)

cor_df <- bind_rows(cor1, cor2, cor3, cor4, cor5) |>
  mutate(comp = factor(comp, levels = c(
    "MSSM Model ~ CPUE 3CD",
    'MSSM Model ~ SYN WCVI',
    "MSSM Design ~ CPUE 3CD",
    'MSSM Design ~ SYN WCVI',
    "MSSM Model ~ SYN WCVI on MSSM Grid"))) |>
  filter(comp %in% c("MSSM Model ~ CPUE 3CD", "MSSM Model ~ SYN WCVI", "MSSM Model ~ SYN WCVI on MSSM Grid")) |>
  left_join(full_cor |>
      rename(mean_cor = 'cor_val') |>
      group_by(comp) |>
      ungroup()) |>
  filter(mean_cor >= 0.5)

cor_plot <-
  ggplot(cor_df, aes(x = start_year, y = cor_vals)) +
      geom_point(data = cor_df |> group_by(comp) |> slice(which.max(start_year)) |>
        mutate(start_year = start_year + 6), alpha = 0) +  # variable x limit increaser
      geom_hline(yintercept = 0, colour = 'grey50') +
      geom_line(aes(colour = species)) +
      geom_smooth(se = FALSE) +
      guides(colour = "none") +
      facet_wrap(~ comp, scale = 'free_x') +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      coord_cartesian(clip = "off") +
        ggrepel::geom_text_repel(
        data = cor_df %>% group_by(comp, species) %>% slice(which.max(start_year)),
        aes(label = species, x = start_year, colour = species),
        size = 3.2, hjust = 'left', segment.color = 'grey85',
        nudge_x = 0.3, box.padding = 0.1, point.padding = 0.5,
        direction = "y"
      ) +
      labs(x = "Start year of 10-year rolling window",
           y = "Correlation")
cor_plot

ggsave(file.path(mssm_figs, 'index-correlation.png'), plot = cor_plot,
  width = 10.5, height = 4.3)

# ------ Mean correlation ~ size -------
full_cor |>
  left_join(size_diff_lu, by = c('species' = 'species_common_name')) |>
ggplot(data = _, aes(x = abs_diff, y = cor_val)) +
  geom_point() +
  geom_hline(yintercept = 0, colour = 'grey70') +
  facet_wrap(~ comp) +
  labs(x = "Absolute size difference (cm)", y = "Correlation")
ggsave(file.path(mssm_figs, 'size-survey-correlation.png'), width = 7.6, height = 2.7)

# Depth overlap
# No real difference in depth ranges surveyed over time
depth_comp <- bind_rows(sw_dat, mssm_dat) |>
 distinct(survey_series_id, fishing_event_id, .keep_all =TRUE)

depth_comp |> filter(year > 2003) |>
  ggplot(aes(x = depth_m, fill = year)) +
  #geom_histogram(alpha = 0.8) +
  geom_density(aes(group = year), alpha = 0.5) +
  #scale_fill_manual(values = survey_cols) +
  facet_wrap(~ survey_abbrev, scales = 'free_y', ncol = 1) +
  # geom_vline(xintercept = c(min(mssm_dat$depth_m, na.rm = TRUE), max(mssm_dat$depth_m, na.rm = TRUE)))
  # geom_rect(data = tibble(survey_abbrev = 'SYN WCVI', depth_m = min(mssm_dat$depth_m, na.rm = TRUE)), aes(xmin = -Inf, xmax = depth_m, ymin = -Inf, ymax = Inf), fill = 'grey50', alpha = 0.1) +
  # geom_rect(data = tibble(survey_abbrev = 'SYN WCVI', depth_m = max(mssm_dat$depth_m, na.rm = TRUE)), aes(xmax = Inf, xmin = depth_m, ymin = -Inf, ymax = Inf), fill = 'grey50', alpha = 0.1) +
  theme(axis.text.y = element_blank(),
        legend.position = c(0.7, 0.9), legend.direction = 'horizontal',
        legend.key.width=unit(0.05,"npc")) +
  labs(x = "Depth (m)", y = "Sampling frequency", fill = "Year")

ggsave(file.path(mssm_figs, 'depth-ranges-mssm-syn-wcvi.png'), width = 6.5, height = 4)

