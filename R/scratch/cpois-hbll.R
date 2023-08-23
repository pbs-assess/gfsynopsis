library(tidyverse)
library(mgcv)
library(patchwork)
library(sdmTMB)
devtools::load_all()

save_plots_to_pdf <- function(ggplot_list, filename, width = 7, height = 3.7) {
  pdf(filename, width = width, height = height)
  map(ggplot_list, print)
  dev.off()
}

# Load functions used to get pstar
source(here::here('R', 'scratch', 'cpois-gam-functions.R'))

# Set survey type
#survey_type <- 'hbll_outside'
survey_type <- 'hbll_inside'

# Setup caches ---------
# Data cache
dc <- file.path("report", "data-cache-aug-2023")

# Stitch output cache
stitch_cache <- file.path('report', 'stitch-cache', survey_type)
dir.create(file.path(stitch_cache, 'cpois'), showWarnings = FALSE)

# Pstar cache
pstar_cache <- file.path('report', 'pstar-cache')
pstar_hbll_cache <- file.path(pstar_cache, survey_type)
dir.create(pstar_hbll_cache, showWarnings = FALSE)

# Prepare data --------------------
spp_list <- gfsynopsis::get_spp_names()$species_common_name |> sort()
bait_counts <- readRDS(file.path(dc, "bait-counts.rds"))
grid_dir <- file.path(dc, 'grids')

# Clean data (a bit slow)
spp_dat  <-
  spp_list |>
  map(\(sp) readRDS(file.path(dc, paste0(clean_name(sp), ".rds")))$survey_sets) |>
  map(\(dat) prep_stitch_dat(spp_dat = dat, bait_counts = bait_counts)) |>
  map(\(dat) filter(dat, survey_type == {{survey_type}})) |>
  setNames(spp_list)
#beepr::beep()

# Save time because I keep screwing up the models
#saveRDS(spp_dat, 'scratch-out/spp_dat.rds')
spp_dat <- readRDS('scratch-out/spp_dat.rds')

# Stitch only species with proportion of positive sets >= 5% (in both regions)
hbll_stitch_lu <-
  get_stitch_lu(bind_rows(spp_dat), species = spp_list, survey_type = survey_type) |>
  filter(include_in_stitch == 1) |>
  group_by(species_common_name) |>
  filter(n() == 2) |>
  distinct(species_common_name) |>
  pull('species_common_name')

# Get pstar for hbll  ----------------
# How similar is pstar across survey types?
gam_formula <- formula(catch ~ -1 + s(prop_removed) + fyear + offset(log(hook_count)))

pstar_list <-
  spp_dat[hbll_stitch_lu] |>
  map(\(dat) get_pstar(dat, gam_formula, survey_type = survey_type, pstar_cache = pstar_cache,
    save_out = FALSE))

pstar_plots <- names(pstar_list) |>
  map(\(name) plot_pstar(pstar_list[[name]], sp_dat = spp_dat[[name]]))

# @QUESTION: should we add that the value at 1 is greater than at
# prop_removed < 1 (see rougheye/blackspotted outside gam fit)
#save_plots_to_pdf(pstar_plots, file.path(pstar_hbll_cache, 'pstar-plots_offset.pdf'))
pstar_df <-
  pstar_list |>
    map(\(i) i$pstar_df) |>
    bind_rows(.id = 'species_common_name')

# saveRDS(pstar_df, file.path(pstar_hbll_cache, 'pstar-df.rds'))

# -------
# Stitch HBLL index using censored poisson where applicable
model_type <- "st-rw"
pstar_df <- readRDS(file.path(pstar_cache, survey_type, 'pstar-df.rds')) |>
  right_join(tibble(species_common_name = spp_list))

# add_upr <- function(dat, prop_removed_col, n_catch_col, n_hooks_col,
#   pstar_col = 'pstar', pstar = NULL) {
#   if (is.null(pstar)) {
#     pstar <- dat[[pstar_col]][1]
#   }

#   if (is.na(pstar) | pstar == 1) {
#     dat$upr <- as.numeric(NA)
#   } else {
#     dat$upr <- sdmTMB:::get_censored_upper(dat[[prop_removed_col]], dat[[n_catch_col]],
#         dat[[n_hooks_col]], pstar)
#   }

#   if (all(is.na(dat$upr))) {
#     control_upr <- NULL
#   }  else {
#     control_upr <- dat$upr
#   }

#   list(survey_dat = dat, control_upr = control_upr)
# }

add_upr <- function(dat, prop_removed_col, n_catch_col, n_hooks_col,
  pstar_col = 'pstar', pstar = NULL) {

  if (is.null(pstar)) {
    pstar <- dat[[pstar_col]][1]
  }

  dat$upr <- sdmTMB:::get_censored_upper(dat[[prop_removed_col]], dat[[n_catch_col]],
        dat[[n_hooks_col]], pstar)
  dat
}

# Use poisson as default to be consistent with using censored poisson
# get_pois_flavour <- function(upr) {
#   if (is.null(upr)) {
#     family <- poisson(link = "log")
#   } else {
#     family <- sdmTMB::censored_poisson(link = "log")
#   }
# }

hbll_stitch_dat <- spp_dat |>
  map(\(dat) mutate(dat, obs_id = factor(row_number()))) |>
  map(\(dat) left_join(dat, pstar_df, by = 'species_common_name')) |>
  map(\(dat) mutate(dat, pstar = ifelse(is.na(pstar), 1, pstar))) |>
  map(\(dat) add_upr(dat, 'prop_removed', 'catch', 'hook_count', 'pstar')) |>
  setNames(spp_list) # makes easier to interactively check species/outputs

# GET INDEX
# ------------------------------------------------------------------------------
nbin_index <-
  names(hbll_stitch_dat) |>
  map(\(sp) get_stitched_index(survey_dat = hbll_stitch_dat[[sp]], species = sp,
    survey_type = survey_type, model_type = model_type,
    offset = 'offset', # original offset with hook competition
    family = sdmTMB::nbinom1(link = "log"),
    grid_dir = grid_dir, silent = FALSE,
    cache = file.path(stitch_cache_hbll))
  )

index_list <-
  names(hbll_stitch_dat) |>
  map(\(sp) get_stitched_index(survey_dat = hbll_stitch_dat[[sp]], species = sp,
    survey_type = survey_type, model_type = model_type,
    offset = 'log_hook_count',
    family = sdmTMB::censored_poisson(link = "log"), ctrl = sdmTMB::sdmTMBcontrol(
      censored_upper = hbll_stitch_dat[[sp]]$upr),
    grid_dir = grid_dir, silent = FALSE,
    cache = file.path(stitch_cache, 'cpois'))
  )
  )
beepr::beep()

# ------------------------------------------------------------------------------

# stats_family_lu <- hbll_stitch_dat |>
#   imap(\(x, idx) tibble(species = idx, family = x$family$family[1])) |>
#   bind_rows()

stats_family_lu <- hbll_stitch_dat |>
  map(\(dat) mutate(dat, family = ifelse(pstar == 1, 'poisson', 'censored poisson'))) |>
  bind_rows() |>
  distinct(species_common_name, pstar, family)

index_list <-
  spp_list |>
  map(\(sp) readRDS(
    file.path(stitch_cache, 'cpois', paste0(clean_name(sp), '_', model_type, '.rds')))) |>
  setNames(spp_list) |>
  keep(is.data.frame) |>
  imap(\(x, idx) mutate(x, survey_abbrev = survey_type, species_common_name = idx) |>
    left_join(stats_family_lu)
  )
  imap(\(x, idx) mutate(x, survey_abbrev = "HBLL OUT N/S", species_common_name = idx) |>
    left_join(stats_family_lu)
  )

survey_cols <- c(
  RColorBrewer::brewer.pal(5L, "Set1"),
  RColorBrewer::brewer.pal(8L, "Set1")[7:8],
  "#303030", "#a8a8a8", "#a8a8a8", "#a8a8a8",
  "#a8a8a8", "#a8a8a8", "#a8a8a8", "#a8a8a8"
)
survey_col_names <- c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI",
    "HBLL OUT N", "HBLL OUT S", "IPHC FISS", "Commercial",
    "HBLL INS N", "HBLL INS S", "MSA HS",
    "SYN HS/QCS/WCHG/WCVI", "SYN HS/QCS/WCVI",
    "HBLL OUT N/S", "HBLL INS N/S"
    )

cpois_plots <-
  index_list |>
  map(\(index)
    gfplot::plot_survey_index(index,
          col = c("grey60", "grey20"), survey_cols = survey_cols,
          xlim = c(1984 - 0.2, 2022 + 0.2), french = FALSE) +
          scale_x_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE))
  ) |>
  imap(\(x, idx) x + ggtitle(paste0(index_list[[idx]]$species_common_name, ': ', index_list[[idx]]$family)))

#save_plots_to_pdf(cpois_plots, 'scratch-out/hbll_out_cpois_plots.pdf', width = 7, height = 3.7)

negbin_index <-
  spp_list |>
  map(\(sp) readRDS(
    file.path(stitch_cache_hbll,
      paste0(gfsynopsis:::clean_name(sp), '_', model_type, '.rds')))) |>
  setNames(spp_list)

negbin_plots <-
  negbin_index |>
  keep(is.data.frame) |>
  imap(\(x, idx) mutate(x, survey_abbrev = "HBLL OUT N/S", species = idx)) |>
  map(\(index)
    gfplot::plot_survey_index(index,
          col = c("grey60", "grey20"), survey_cols = survey_cols,
          xlim = c(1984 - 0.2, 2022 + 0.2), french = FALSE) +
          scale_x_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE))
  ) |>
  imap(\(x, idx) x + ggtitle(paste0(idx, ': nbinom1, hook comp')))

#save_plots_to_pdf(negbin_plots, 'scratch-out/hbll_out_negbin_plots.pdf', width = 7, height = 3.7)

empty_plots <- map(names(cpois_plots), ~ ggplot() + ggtitle(.x))

cpois_plots <- names(negbin_plots) |>
  map(~ if (.x %in% names(cpois_plots)) {
      cpois_plots[[.x]]
    } else {
      ggplot() + theme_pbs() + ggtitle(paste0(.x, ": poisson/cpois"))
    }
  ) |>
  map(\(p) p + theme(axis.title.y = element_blank(), axis.text.y = element_blank()))

# Combine the plots using patchwork
combined_plots <- plot_layout(
  map2(negbin_plots, cpois_plots, ~ .x + .y),
  nrow = 2
)

# save_plots_to_pdf(combined_plots, 'scratch-out/hbll_out_comparison.pdf',
#   height = 3.4)

save_plots_to_pdf(combined_plots, 'scratch-out/hbll_ins_comparison.pdf',
  height = 3.4)
