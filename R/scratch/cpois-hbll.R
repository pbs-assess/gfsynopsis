library(tidyverse)
library(mgcv)
devtools::load_all()

save_plots_to_pdf <- function(ggplot_list, filename, width = 7, height = 3.7) {
  pdf(filename, width = width, height = height)
  map(ggplot_list, print)
  dev.off()
}

source(here::here('R', 'scratch', 'cpois-gam-functions.R'))
#source(here::here('R', 'scratch', 'stitch-iphc-functions.R'))

survey_type <- 'hbll_outside'
#survey_type <- 'hbll_inside'

dc <- file.path("report", "data-cache-aug-2023")

# @ FIXME The cpois caches are really goofy right now....
stitch_cache <- file.path("report", "stitch-cache") # Stitched outputs
stitch_cache_hbll_out <- file.path(stitch_cache, "hbll_outside")
stitch_cache_hbll_ins <- file.path(stitch_cache, "hbll_inside")

dir.create(file.path(stitch_cache_hbll_out, 'cpois'), showWarnings = FALSE)
dir.create(file.path(stitch_cache_hbll_ins, 'cpois'), showWarnings = FALSE)

pstar_cache <- file.path('report', 'pstar-cache')
pstar_hbll_cache <- file.path(pstar_cache, survey_type)
dir.create(pstar_hbll_cache, showWarnings = FALSE)

#spp_list <- gfsynopsis::get_spp_names()$species_common_name |> sort()
spp_list <- "quillback rockfish"
bait_counts <- readRDS(file.path(dc, "bait-counts.rds"))
grid_dir <- file.path(dc, 'grids')

spp_dat  <- 
  spp_list |>
  map(\(sp) readRDS(file.path(dc, paste0(gfsynopsis:::clean_name(sp), ".rds")))$survey_sets) |>
  map(\(dat) prep_stitch_dat(spp_dat = dat, bait_counts = bait_counts)) |>
  map(\(dat) filter(dat, survey_type == {{survey_type}})) |>
  setNames(spp_list)
beepr::beep()

# Save time because I keep screwing up the models
#saveRDS(spp_dat, 'scratch-out/spp_dat.rds')
# spp_dat <- readRDS('scratch-out/spp_dat.rds')

# test <-
#   bind_rows(spp_dat)

# filter(test, prop_removed > 0.95) |>
#   arrange(desc(catch)) |>
#   select(species_common_name, catch, hook_count, fishing_event_id, year) |>
#   slice(1:100) |>
#   View()

# Get pstar for hbll  ----------------
# How similar is pstar across survey types?
gam_formula <- formula(catch ~ -1 + s(prop_removed) + fyear + offset(log(hook_count)))

hbll_stitch_lu <-
  get_stitch_lu(bind_rows(spp_dat), species = spp_list, survey_type = survey_type) |>
  filter(include_in_stitch == 1) |>
  distinct(species_common_name) |>
  pull('species_common_name')

pstar_list <-
  spp_dat[hbll_stitch_lu] |>
  map(\(dat) get_pstar(dat, gam_formula, survey_type = survey_type, pstar_cache = pstar_cache,
    save_out = FALSE))

# pstar_plots <- names(pstar_list) |>
#   map(\(name) plot_pstar(pstar_list[[name]], sp_dat = spp_dat[[name]]))


# @QUESTION: should we add that the value at 1 is greater than at
# prop_removed < 1 (see rougheye/blackspotted outside gam fit)
#save_plots_to_pdf(pstar_plots, file.path(pstar_hbll_cache, 'pstar-plots_offset.pdf'))
pstar_df <-
  pstar_list |>
    map(\(i) i$pstar_df) |>
    bind_rows(.id = 'species_common_name')

#saveRDS(pstar_df, file.path(pstar_hbll_cache, 'pstar-df.rds'))

# -------
# Stitch HBLL index using censored poisson where applicable
model_type <- "st-rw"
# family <- sdmTMB::censored_poisson()
# family <- sdmTMB::nbinom1(link = "log")
# family <- poisson(link = "log")
pstar_df <- readRDS(file.path(pstar_cache, survey_type, 'pstar-df.rds')) |>
  mutate(pstar = "prop_removed") |> # This still needs to be updated for hbll out and ins
  right_join(tibble(species_common_name = spp_list))

add_upr <- function(dat, prop_removed_col, n_catch_col, n_hooks_col,
  pstar_col = 'pstar', cap_upr = FALSE, pstar = NULL) {
  if (is.null(pstar)) {
    pstar <- dat[[pstar_col]][1]
  }

  if (is.na(pstar) | pstar == 1) {
    dat$upr <- as.numeric(NA)
  } else {
    dat$upr <- sdmTMB:::get_censored_upper(dat[[prop_removed_col]], dat[[n_catch_col]],
        dat[[n_hooks_col]], pstar)
  }

  if (cap_upr) {
    dat <- dat |> mutate(upr = ifelse((upr < hook_count) | is.na(upr),  upr, dat[[n_hooks_col]]))
  }

  if (all(is.na(dat$upr))) {
    control_upr <- NULL
  }  else {
    control_upr <- dat$upr
  }

  list(survey_dat = dat, control_upr = control_upr)
}

# @QUESTION should the default be negbin1 or poisson?
get_pois_flavour <- function(upr) {
  if (is.null(upr)) {
    family <- sdmTMB::nbinom1(link = "log")
  } else {
    family <- sdmTMB::censored_poisson(link = "log")
  }
}


hbll_stitch_dat <- spp_dat |>
  map(\(dat) left_join(dat, pstar_df, by = 'species_common_name')) |>
  map(\(dat) add_upr(dat, 'prop_removed', 'catch', 'hook_count', 'pstar',
    cap_upr = FALSE)) |>
  map(\(dat) list(survey_dat = dat$survey_dat, control_upr = dat$control_upr,
    family = get_pois_flavour(dat$control_upr))) |>
  setNames(spp_list) # makes easier to interactively check species/outputs

# GET INDEX
# ------------------------------------------------------------------------------
nbin_index <-
  names(hbll_stitch_dat) |>
  map(\(sp) get_stitched_index(survey_dat = hbll_stitch_dat[[sp]]$survey_dat, species = sp,
    survey_type = survey_type, model_type = model_type,
    offset = 'offset', # original offset with hook competition
    family = sdmTMB::nbinom1(link = "log"), ctrl = sdmTMB::sdmTMBcontrol(
      censored_upper = NULL),
    grid_dir = grid_dir, silent = FALSE,
    cache = file.path(stitch_cache_hbll_out))
  )

# @QUESTION: should the offset be plain log(hook_count)?
index_list <-
  names(hbll_stitch_dat) |>
  map(\(sp) get_stitched_index(survey_dat = hbll_stitch_dat[[sp]]$survey_dat, species = sp,
    survey_type = survey_type, model_type = model_type,
    offset = 'log_hook_count',
    family = hbll_stitch_dat[[sp]]$family, ctrl = sdmTMB::sdmTMBcontrol(
      censored_upper = hbll_stitch_dat[[sp]]$control_upr),
    grid_dir = grid_dir, silent = FALSE,
    cache = file.path(stitch_cache_hbll_out, 'cpois'))  # mb... lives in a weird dir structure
  )
#beepr::beep()

# ------------------------------------------------------------------------------

stats_family_lu <- hbll_stitch_dat |>
  imap(\(x, idx) tibble(species = idx, family = x$family$family[1])) |>
  bind_rows()

# @FIXME need to deal with file paths at some point
index_list <-
  spp_list |>
  map(\(sp) readRDS(
    file.path(stitch_cache_hbll_out, 'cpois', survey_type,
      paste0(gfsynopsis:::clean_name(sp), '_', model_type, '.rds')))) |>
  setNames(spp_list) |>
  keep(is.data.frame) |>
  imap(\(x, idx) mutate(x, survey_abbrev = "HBLL OUT N/S", species = idx) |>
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
  imap(\(x, idx) x + ggtitle(paste0(index_list[[idx]]$species, ': ', index_list[[idx]]$family)))

save_plots_to_pdf(cpois_plots, 'scratch-out/hbll_out_cpois_plots.pdf', width = 7, height = 3.7)

negbin_index <-
  spp_list |>
  map(\(sp) readRDS(
    file.path(stitch_cache_hbll_out,
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
  imap(\(x, idx) x + ggtitle(paste0(idx, ': nbinom1, hook comp offset')))

save_plots_to_pdf(negbin_plots, 'scratch-out/hbll_out_negbin_plots.pdf', width = 7, height = 3.7)


# Use inside hbll for qb:
pstar_df <- readRDS(file.path(pstar_cache, 'hbll_inside', 'pstar-df.rds')) |>
  rename(pstar = 'prop_removed') |> # @FIXME remove after rerunning get_pstar() for hbll out/ins
  right_join(tibble(species_common_name = spp_list))

qb_stitch_dat <- spp_dat['quillback rockfish'] |>
  map(\(dat) left_join(dat, pstar_df, by = 'species_common_name')) |>
  map(\(dat) add_upr(dat, 'prop_removed', 'catch', 'hook_count', 'pstar',
    cap_upr = TRUE))|>
  map(\(dat) list(survey_dat = dat$survey_dat, control_upr = dat$control_upr,
    family = get_pois_flavour(dat$control_upr))) |>
  setNames('quillback rockfish') # makes easier to interactively check species/outputs


# @QUESTION: should the offset be plain log(hook_count)?
qb_index <-
  names(qb_stitch_dat) |>
  map(\(sp) get_stitched_index(survey_dat = qb_stitch_dat[[sp]]$survey_dat, species = sp,
    survey_type = survey_type, model_type = model_type,
    offset = 'log_hook_count',
    family = qb_stitch_dat[[sp]]$family, ctrl = sdmTMB::sdmTMBcontrol(
      censored_upper = qb_stitch_dat[[sp]]$control_upr),
    grid_dir = grid_dir, silent = FALSE,
    cache = file.path(stitch_cache_hbll_out, 'cpois'))
  )
beepr::beep()


qb_plot <-
qb_index[[1]] |>
  mutate(survey_abbrev = "HBLL OUT N/S") |>
gfplot::plot_survey_index(col = c("grey60", "grey20"), survey_cols = survey_cols,
          xlim = c(1984 - 0.2, 2022 + 0.2), french = FALSE) +
          scale_x_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE))

qb_plot + ggtitle('QB using hbll inside pstar for cpois')