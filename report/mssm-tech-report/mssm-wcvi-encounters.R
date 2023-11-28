library(dplyr)
library(ggplot2)

f <- list.files("report/data-cache-nov-2023/", pattern = ".rds", full.names = TRUE)
f <- f[!grepl("cpue", f)]
f <- f[!grepl("bait-counts", f)]
f <- f[!grepl("hbll-inside-grid_water-area", f)]

d <- purrr::map_dfr(f, function(x) {
  cat(x, "\n")
  a <- readRDS(x)
  dplyr::filter(a$survey_sets, survey_abbrev %in% c("MSSM WCVI", "SYN WCVI"))
})
saveRDS(d, "report/mssm-sets-temp.rds")
d <- readRDS("report/mssm-sets-temp.rds")

syn_yrs <- filter(d, survey_abbrev %in% "SYN WCVI") |> pull(year) |> unique() |> sort()

s <- d |>
  filter(year %in% syn_yrs) |>
  filter(is.finite(density_kgpm2), !is.na(density_kgpm2)) |>
  group_by(species_common_name, survey_abbrev) |>
  summarise(density = mean(density_kgpm2), encounter = mean(density_kgpm2 > 0)) |>
  group_by(species_common_name) |>
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

make_mssm_tigure <- function(df, fill_limits = c(0, 1), padding = 0, fill_lab = "Encounter\nprobability", digits = 2L) {
  df$species_common_name <- stringr::str_to_title(df$species_common_name)
  df$species_common_name <- gsub(" Complex$", "", df$species_common_name)
  sp <- df |> filter(survey_abbrev == "MSSM WCVI") |>
    arrange(val) |>
    pull(species_common_name) %>%
    factor(., levels = .)
  df$species_common_name <- factor(df$species_common_name, levels = sp)
  df$txt <- sdmTMB:::mround(df$val, digits)
  g <- df |>
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
    theme(
      panel.border = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(color = "grey10")
    ) +
    labs(fill = fill_lab) +
    scale_x_discrete(position = "top")
  g
}

dat <- s
dat$val <- dat$encounter
g1 <- make_mssm_tigure(dat)

dat$val <- dat$density * 1000000
g2 <- make_mssm_tigure(dat, fill_lab = "Mean\\\ndensity\\\n(kg/km^2^)", digits = 1L) +
  scale_fill_viridis_c(limits = c(NA, NA), begin = 0.15, end = 1, alpha = .9, option = "D", direction = 1, trans = "log10") +
    theme(legend.title = ggtext::element_markdown())

g <- cowplot::plot_grid(g1, g2, ncol = 2L, rel_widths = c(1, 1))
ggsave("figs/mssm-wcvi-tigure.png", width = 9.05, height = 7.5)
