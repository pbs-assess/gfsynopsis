library("gfplot")
library("tidyverse")

d_index <- readRDS("data-cache/all-boot-biomass-indices.rds")
d_surv_samp <- readRDS("data-cache/all-survey-bio.rds")
d_comm_samp <- readRDS("data-cache/all-commercial-bio.rds")
d_catch <- readRDS("data-cache/all-catches.rds")

# filter(d_surv_samp, survey_series_desc=="PHMA Rockfish Longline Survey - Outside South", !is.na(length)) %>% pull(species_common_name) %>% table %>% sort() %>% rev()
#
# filter(d_surv_samp, survey_series_desc=="IRF Longline Survey (North)", !is.na(length)) %>% pull(species_common_name) %>% table %>% sort() %>% rev()
#
# filter(d_surv_samp, survey_series_desc=="IPHC Longline Survey", !is.na(length)) %>% pull(species_common_name) %>% table %>% sort() %>% rev()

spp <- "yelloweye rockfish"

sn <- get_spp_names() %>%
  filter(type == "A") %>%
  filter(species_common_name %in% c("pacific ocean perch", "yelloweye rockfish", "pacific cod"))

filter(d_comm_samp, species_common_name == spp) %>%
  tidy_sample_avail()

for (i in seq_along(sn$species_common_name)) {

  spp <- sn$species_common_name[i]
  spp_f <- sn$spp_w_hyphens[i]
  message(spp)

  g <- filter(d_index, species_common_name == spp) %>%
    tidy_survey_index() %>%
    plot_survey_index()
  ggsave(paste0("report/figs/", spp_f, "-survey_index.pdf"), width = 5, height = 5)

  g <- filter(d_surv_samp, species_common_name == spp) %>%
    tidy_ages_raw() %>%
    plot_ages(max_size = 3.7, sex_gap = 0.25, year_range = c(2003, 2016))
  ggsave(paste0("report/figs/", spp_f, "-ages.pdf"), width = 13, height = 5)

  # # TODO:
  g <- filter(d_surv_samp, species_common_name == spp) %>%
    plot_lengths(n_bins = 25)
  ggsave(paste0("report/figs/", spp_f, "-lengths.pdf"), width = 8, height = 6)

  g <- filter(d_catch, species_common_name == spp) %>%
    tidy_catch() %>%
    plot_catch()
  ggsave(paste0("report/figs/", spp_f, "-catch.pdf"), width = 6, height = 2)

  g <- filter(d_surv_samp, species_common_name == spp) %>%
    tidy_sample_avail(year_range = c(1996, 2016)) %>%
    plot_sample_avail(year_range = c(1996, 2016), title = "Survey samples")
  ggsave(paste0("report/figs/", spp_f, "-surv-samples.pdf"), width = 6, height = 1.5)

  g <- filter(d_comm_samp, species_common_name == spp) %>%
    tidy_sample_avail(year_range = c(1996, 2016)) %>%
    plot_sample_avail(year_range = c(1996, 2016), title = "Commercial samples")
  ggsave(paste0("report/figs/", spp_f, "-comm-samples.pdf"), width = 6, height = 1.5)


}

# weighting:
# # bio_specs <- readRDS("data-cache/all-commercial-bio.rds") %>%
# #   filter(species_common_name %in% "pacific ocean perch")
# # catch <- readRDS("data-cache/all-catches.rds") %>%
# #   filter(species_common_name %in% "pacific ocean perch")
#survey_specimens <- readRDS("data-cache/all-survey-bio.rds") %>%
#  filter(species_common_name %in% "pacific ocean perch")
#survey_tows <- readRDS("data-cache/all-survey-spatial-tows.rds") %>%
#  filter(species_common_name %in% "pacific ocean perch")
#
# weighted_lengths <- tidy_comps_survey(survey_specimens, survey_tows,
#   value = age, bin_size = 2)
#
#
# out <- purrr::map_df(survs, function(x) {
#   surv_spec <- dplyr::filter(survey_specimens, survey_series_desc == x)
#   survey_tows <- dplyr::filter(survey_tows, survey_series_desc == x)
#   o <- surv_spec %>% tidy_comps_survey(survey_tows, length, bin_size = 2) %>%
#     weight_comps()
#   o$survey_series_desc <- x
#   o
# })
#
# ggplot(out, aes_string("value", "weighted_prop")) +
#   ggplot2::geom_col(width = 2) +
#   facet_grid(forcats::fct_rev(as.character(year))~forcats::fct_relevel(survey_series_desc,
#     survs)) +
#   theme_pbs() +
#   theme(panel.spacing = unit(-0.5, "lines")) +
#   ylim(0, NA) +
#   coord_cartesian(expand = FALSE)
