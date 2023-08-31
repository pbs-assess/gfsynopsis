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
dir.create(file.path(stitch_cache, 'cpois'), showWarnings = FALSE, recursive = TRUE)

# Pstar cache
pstar_cache <- file.path('report', 'pstar-cache', survey_type)
dir.create(pstar_cache, showWarnings = FALSE, recursive = TRUE)

# Prepare data --------------------
spp_list <- gfsynopsis::get_spp_names()$species_common_name |> sort()
spp_list <- 'lingcod'
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
#save_plots_to_pdf(pstar_plots, file.path(pstar_cache, 'pstar-plots_offset.pdf'))
pstar_df <-
  pstar_list |>
    map(\(i) i$pstar_df) |>
    bind_rows(.id = 'species_common_name')

# saveRDS(pstar_df, file.path(pstar_cache, 'pstar-df.rds'))

# -------
# Stitch HBLL index using censored poisson where applicable
model_type <- "st-rw"
pstar_df <- readRDS(file.path(pstar_cache, survey_type, 'pstar-df.rds')) |>
  right_join(tibble(species_common_name = spp_list))

hbll_stitch_dat <- spp_dat |>
  map(\(dat) mutate(dat, obs_id = factor(row_number()))) |>
  map(\(dat) left_join(dat, pstar_df, by = 'species_common_name')) |>
  map(\(dat) mutate(dat, pstar = ifelse(is.na(pstar), 1, pstar))) |>
  map(\(dat) add_upr(dat, 'prop_removed', 'catch', 'hook_count', 'pstar')) |>
  setNames(spp_list) # makes easier to interactively check species/outputs

# GET INDEX
# ------------------------------------------------------------------------------
nbin2_index <-
  names(hbll_stitch_dat) |>
  map(\(sp) get_stitched_index(survey_dat = hbll_stitch_dat[[sp]], species = sp,
    survey_type = survey_type, model_type = model_type,
    offset = 'offset', # original offset with hook competition
    #offset = 'log_hook_count',
    family = sdmTMB::nbinom2(link = "log"),
    grid_dir = grid_dir, silent = FALSE,
    cache = file.path(stitch_cache, 'nbin2'))
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

#assert_that(mean(upr-y_i, na.rm = TRUE)>=0)

# ------------------------------------------------------------------------------
nbin2_fits <- hbll_stitch_lu |>
  map(\(sp) readRDS(
    file.path(stitch_cache, 'nbin2', 'fits', paste0(clean_name(sp), '_', model_type, '.rds')))) |>
  setNames(hbll_stitch_lu)

fit_list <-
  hbll_stitch_lu |>
  map(\(sp) readRDS(
    file.path(stitch_cache, 'cpois', 'fits', paste0(clean_name(sp), '_', model_type, '.rds')))) |>
  setNames(hbll_stitch_lu)

sanity_list <- fit_list |> map(\(x) sdmTMB::sanity(x, gradient_thresh = 0.01) |> as_tibble()) |>
  bind_rows(.id = 'species')

bad_fits <- sanity_list |> filter(!all_ok | is.na(all_ok))

test_inds <-
#bad_fits$species[c(2, 3, 5)] |>
c('china rockfish', 'copper rockfish', 'petrale sole') |>
map(\(sp) get_stitched_index(survey_dat = hbll_stitch_dat[[sp]], species = sp,
    survey_type = survey_type, model_type = 'custom',
    form = 'catch ~ 1 + (1 | obs_id)',
    family = sdmTMB::censored_poisson(link = "log"),
    time = 'year',
    spatial = "off",
    spatiotemporal = "ar1",
    time_varying = ~1,
    time_varying_type = 'ar1',
    offset = 'log_hook_count',
    priors = sdmTMB::sdmTMBpriors(
      sigma_G = sdmTMB::halfnormal(0, 1),
      matern_st = sdmTMB::pc_matern(range_gt = 20, sigma_lt = 1),
      #matern_s = sdmTMB::pc_matern(range_gt = 10, sigma_lt = 5)
    ),
    ctrl = sdmTMB::sdmTMBcontrol(censored_upper = hbll_stitch_dat[[sp]]$upr
    ),
    grid_dir = grid_dir, silent = FALSE,
    gradient_thresh = 0.001,
    cache = file.path(stitch_cache, 'badfits-drop-sp-off_st-ar1_tv-ar1'))
  )
beepr::beep()

test <- as.list(fit_list[[2]]$sd_report, "Estimate")



sds <- fit_list |>
  map(\(x) if (inherits(x, 'sdmTMB')) as.list(x$sd_report, "Estimate")) |>
  map(\(x) if (is.list(x)) tibble(ln_tau_O = x$ln_tau_O, ln_tau_E = x$ln_tau_E,
      ln_tau_V = x$ln_tau_V, ln_tau_G = x$ln_tau_G,
      omega_s = mean(x$omega_s), epsilon_st = mean(x$epsilon_st)))

sigmas <- fit_list |> map(\(x) try(sdmTMB::tidy(x, "ran_pars")))
test2 <- sdmTMB::tidy(fit_list[['petrale sole']], "ran_pars")


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


test_fits <- bad_fits$species |>
  map(\(sp) readRDS(file.path(stitch_cache, 'badfits-drop-sp-st', paste0(clean_name(sp), '_custom.rds')))) |>
  setNames(bad_fits$species)

test_sanity <- sanity_list |> filter(!all_ok | is.na(all_ok))

sdmTMB::sanity(test_fits[['yellowtail rockfish']])

test_index <- bad_fits$species |>
  map(\(sp) readRDS(file.path(stitch_cache, 'badfits-drop-sp-st', paste0(clean_name(sp), '_custom.rds')))) |>
  setNames(bad_fits$species) |>
  keep(is.data.frame) |>
  imap(\(x, idx) mutate(x, survey_abbrev = "HBLL OUT N/S", species_common_name = idx) |>
    left_join(stats_family_lu)
  )

test_index |>
map(\(index)
    gfplot::plot_survey_index(index,
          col = c("grey60", "grey20"), survey_cols = survey_cols,
          xlim = c(1984 - 0.2, 2022 + 0.2), french = FALSE) +
          scale_x_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE))
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

index_list <- c(index_list |> map(\(x) mutate(x, st = "on")),
  test_index |> map(\(x) mutate(x, st = "off, rw0 int, matern prior"))) %>% `[`(sort(names(.)))

cpois_plots <- index_list |>
  map(\(index)
    gfplot::plot_survey_index(index,
          col = c("grey60", "grey20"), survey_cols = survey_cols,
          xlim = c(1984 - 0.2, 2022 + 0.2), french = FALSE) +
          scale_x_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE))
  ) |>
  imap(\(x, idx) x + ggtitle(paste0(index_list[[idx]]$species_common_name, '\n',
    index_list[[idx]]$family,
   "(st: ", index_list[[idx]]$st, ")")))

#save_plots_to_pdf(cpois_plots, 'scratch-out/hbll_out_cpois_plots.pdf', width = 7, height = 3.7)

negbin_index <-
  spp_list |>
  map(\(sp) readRDS(
    file.path(stitch_cache, 'nbin2',
      paste0(clean_name(sp), '_', model_type, '.rds')))) |>
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
  imap(\(x, idx) x + ggtitle(paste0(idx, '\nnbinom2, hook comp')))

#save_plots_to_pdf(negbin_plots, 'scratch-out/hbll_out_negbin_plots.pdf', width = 7, height = 3.7)

empty_plots <- map(names(cpois_plots), ~ ggplot() + ggtitle(.x))

cpois_plots <- names(negbin_plots) |>
  map(~ if (.x %in% names(cpois_plots)) {
      cpois_plots[[.x]]
    } else {
      family <- stats_family_lu[stats_family_lu$species_common_name == .x, 'family'][[1]]
      ggplot() + theme_pbs() + ggtitle(paste0(.x, "\n", family))
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
