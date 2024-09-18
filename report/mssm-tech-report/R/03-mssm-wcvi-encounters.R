if (!('mssm_loaded' %in% ls())) {
  source(here::here('report', 'mssm-tech-report', 'R', '00-load.R'))
}

syn_yrs <- spp_dat |>
  filter(survey_abbrev %in% "SYN WCVI") |> pull(year) |> unique() |> sort()

mssm_depths <- spp_dat |>
  filter(survey_abbrev == 'SMMS WCVI') |>
  pull(depth_m) |>
  range(na.rm = TRUE)

syn_wcvi_mssm_depth_dat <- spp_dat |>
  filter(survey_abbrev == 'SYN WCVI') |>
  filter(year %in% syn_yrs) |>
  filter(depth_m >= mssm_depths[[1]] & depth_m <= mssm_depths[[2]]) |>
  mutate(survey_abbrev = 'SYN WCVI\n(75 - 219 m)')

s <- spp_dat |>
  filter(survey_abbrev %in% c('SMMS WCVI', 'SYN WCVI')) |>
  filter(year %in% syn_yrs) |>
  bind_rows(syn_wcvi_mssm_depth_dat) |>
  filter(is.finite(density_kgpm2), !is.na(density_kgpm2)) |>
  group_by(species_common_name, survey_abbrev) |>
  summarise(density = mean(density_kgpm2), encounter = mean(density_kgpm2 > 0)) |>
  filter(max(encounter) > 0.1) |>
  ungroup()

# den <- tidyr::pivot_wider(s, id_cols = species_common_name, values_from = density, names_from = survey_abbrev)
# enc <- tidyr::pivot_wider(s, id_cols = species_common_name, values_from = encounter, names_from = survey_abbrev)

# spp_keep <- s |>
#   group_by(species_common_name) |>
#   mutate(n_above = sum(encounter) >= 0.1) |>
#   filter(n_above >= 1) |>
#   pull(species_common_name)
# length(spp_keep)

make_mssm_tigure <- function(df, fill_limits = c(0, 1), padding = 0, digits = 2L) {
  df$species_common_name <- stringr::str_to_title(df$species_common_name)
  df$species_common_name <- gsub(" Complex$", "", df$species_common_name)
  sp <- df |> filter(survey_abbrev == "SMMS WCVI") |>
    arrange(val) |>
    pull(species_common_name) %>%
    factor(., levels = .)
  df$species_common_name <- factor(df$species_common_name, levels = sp)
  df$txt <- sdmTMB:::mround(df$val, digits)
  g <- df |>
    mutate(survey_abbrev = ifelse(survey_abbrev == 'SMMS WCVI', 'SMMS', survey_abbrev)) |>
    mutate(survey_abbrev = factor(survey_abbrev, levels = c("SMMS", "SYN WCVI\n(75 - 219 m)", "SYN WCVI"))) |>
    mutate(survey_abbrev = forcats::fct_recode(survey_abbrev, "SYN WCVI\n(all depths)" = "SYN WCVI")) |>
    ggplot(aes(x = survey_abbrev, y = species_common_name)) +
    geom_tile(aes(fill = val), color = "white") +
    scale_fill_viridis_c(limits = fill_limits, begin = 0.15, end = 1, alpha = .9, option = "D", direction = 1) +
    xlab("") +
    ylab("") +
    geom_text(aes(x = survey_abbrev, label = txt), size = ggplot2::rel(3)) +
    coord_cartesian(
      expand = FALSE,
      xlim = range(as.numeric(df$survey_abbrev)) + c(-padding, padding),
      ylim = range(as.numeric(df$species_common_name)) + c(-padding - 0.5, padding + 0.5)
    ) +
    gfplot::theme_pbs() +
    guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, barwidth = unit(5, "cm"))) +
    theme(
      plot.background = element_rect(fill = NA),
      plot.margin = margin(t = 0, r = -2, b = 0, l = -2),
      panel.border = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(color = "grey10"),
      axis.text.y = element_text(hjust = 1.03),
      legend.margin = margin(t = 0, r = 0.1, b = -15, l = 0),
      legend.position = "top",
      legend.box = "horizontal"
    ) +
    scale_x_discrete(position = "top")
  g
}

dat <- s
dat$val <- dat$encounter
g1 <- make_mssm_tigure(dat) + labs(fill = "Encounter probability")

dat$val <- dat$density * 1000000
g2 <- make_mssm_tigure(dat, digits = 1L) +
  scale_fill_viridis_c(limits = c(NA, NA), begin = 0.15, end = 1,
    alpha = 0.9, option = "D", direction = 1, trans = "log10",
    breaks = c(0.01, 0.1, 1, 10, 100),
    labels = c("0.01", "0.1", "1", "10", "100")) +
  labs(fill = expression("Mean density (kg/km"^2*")"))

g1 + g2 & theme(plot.margin = margin(0, 0, 0, -15))

ggsave(filename = file.path(mssm_figs, "mssm-wcvi-tigure_same-depth.png"), width = 8, height = 10)
