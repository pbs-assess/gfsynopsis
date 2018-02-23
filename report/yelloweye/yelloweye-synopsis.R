library(gfplot)

figs <- file.path("report", "yelloweye", "yelloweye-figs")
dir.create(figs, showWarnings = FALSE)

cache_pbs_data("yelloweye rockfish", path = "data-cache")

cache <- file.path("report", "yelloweye", "data-cache")

d_survey_tows <- readRDS(file.path(cache, "pbs-surv-tows.rds"))
d_survey_samples <- readRDS(file.path(cache, "pbs-surv-samples.rds"))
d_comm_samples <- readRDS(file.path(cache, "pbs-comm-samples.rds"))
d_catch <- readRDS(file.path(cache, "pbs-catch.rds"))
d_cpue_spatial <- readRDS(file.path(cache, "pbs-cpue-spatial.rds"))
d_cpue_spatial_ll <- readRDS(file.path(cache, "pbs-cpue-spatial-ll.rds"))
d_survey_index <- readRDS(file.path(cache, "pbs-surv-index.rds"))
d_age_precision <- readRDS(file.path(cache, "pbs-age-precision.rds"))

library(ggplot2)
library(dplyr)

g <- tidy_ages_raw(d_survey_samples) %>%
  plot_ages()
ggsave(file.path(figs, "ages-raw.pdf"), width = 13, height = 6)

g <- tidy_lengths_raw(d_survey_samples, bin_size = 2,
  year_lim = c(2002, Inf)) %>%
  plot_lengths()
ggsave(file.path(figs, "lengths-raws.pdf"), width = 9, height = 9)

g <- tidy_age_precision(d_age_precision) %>%
  plot_age_precision()
ggsave(file.path(figs, "age-precision.pdf"), width = 5, height = 5)

g <- tidy_catch(d_catch) %>%
  plot_catch()
ggsave(file.path(figs, "catch.pdf"), width = 6, height = 3)

g <- tidy_survey_index(d_survey_index) %>%
  plot_survey_index()
ggsave(file.path(figs, "surv-index.pdf"), width = 6, height = 5)

g <- tidy_sample_avail(d_comm_samples) %>%
  plot_sample_avail(title = "Commercial samples", year_range = c(1994, 2017))
ggsave(file.path(figs, "comm-samp-avail.pdf"), width = 6, height = 1.75)

g <- tidy_sample_avail(d_survey_samples) %>%
  plot_sample_avail(title = "Survey samples", year_range = c(1994, 2017),
    palette = "Blues")
ggsave(file.path(figs, "surv-samp-avail.pdf"), width = 6, height = 1.75)

vb_m <- fit_vb(d_survey_samples, sex = "male", method = "mpd")
vb_f <- fit_vb(d_survey_samples, sex = "female", method = "mpd")
g <- plot_vb(object_female = vb_m, object_male = vb_f)
ggsave(file.path(figs, "vb.pdf"), width = 5, height = 4)

lw_m <- fit_length_weight(d_survey_samples, sex = "male", method = "rlm")
lw_f <- fit_length_weight(d_survey_samples, sex = "female", method = "rlm")
g <- plot_length_weight(object_female = lw_m, object_male = lw_f)
ggsave(file.path(figs, "length-wt.pdf"), width = 6, height = 3.5)

mat_age <- d_survey_samples %>%
  fit_mat_ogive(
    type = "age",
    months = seq(4, 6),
    ageing_method = c(3, 17))
g <- plot_mat_ogive(mat_age)
ggsave(file.path(figs, "age-mat-ogive.pdf"), width = 6, height = 3.5)

mat_length <- d_survey_samples %>%
  fit_mat_ogive(
    type = "length",
    months = seq(4, 6),
    ageing_method = c(3, 17))
g <- plot_mat_ogive(mat_length)
ggsave(file.path(figs, "length-mat-ogive.pdf"), width = 6, height = 3.5)

g <- dplyr::filter(d_cpue_spatial, year >= 2012) %>%
  plot_cpue_spatial(bin_width = 7, n_minimum_vessels = 3) +
  ggtitle("Trawl CPUE") +
  labs(subtitle = "Since 2012; including discards; 3-vessel minimum")
ggsave(file.path(figs, "cpue-spatial.pdf"), width = 6, height = 4.75)

g <- filter(d_cpue_spatial_ll, year >= 2008) %>%
  plot_cpue_spatial(bin_width = 7, n_minimum_vessels = 3,
    fill_lab = "CPUE (kg/fe)") +
  ggtitle("Hook and line CPUE") +
  labs(subtitle = "Since 2008; excluding discards; 3-vessel minimum")
ggsave(file.path(figs, "cpue-spatial-ll.pdf"), width = 6, height = 4.75)

# ## Will come back to:
# m_wcvi <- fit_survey_tows(d_survey_tows,
#   survey = "West Coast Vancouver Island Synoptic Survey",
#   years = 2016)
# plot_survey_tows(m1$predictions, m1$data, fill_column = "combined")
#
# ## etc.
# m_wchg <- fit_survey_tows(d_survey_tows,
#   survey = "West Coast Vancouver Island Synoptic Survey",
#   years = 2016)
# plot_survey_tows(m1$predictions, m1$data, fill_column = "combined")

# ## Not using:
# survey_series_desc <- c(
#   "PHMA Rockfish Longline Survey - Outside North",
#   "PHMA Rockfish Longline Survey - Outside South",
#   "IPHC Longline Survey")
# survey <- c(
#   "PHMA LL (N)",
#   "PHMA LL (S)",
#   "IPHC")
