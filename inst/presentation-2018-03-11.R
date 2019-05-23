library("gfplot")
library("dplyr")
library("ggplot2")
library("ggthemes")

# d_cpue_index <- get_cpue_index(gear = "bottom trawl")
d_cpue_index <- readRDS(file.path("report", "data-cache", "pbs-cpue-index.rds"))

fleet <- tidy_cpue_index(d_cpue_index,
  species_common = "walleye pollock",
  area_grep_pattern = "5[CDE]+",
  min_positive_fe = 100,
  min_positive_trips = 4,
  min_yrs_with_trips = 4,
  year_range = c(1997, 2017))

names(fleet)
length(unique(fleet$vessel_name))

m_cpue <- fit_cpue_index(fleet,
  formula_binomial =
    pos_catch ~ year_factor + f(month) + f(vessel) +
    f(locality) + f(depth) + f(latitude),
  formula_lognormal =
    log(spp_catch/hours_fished) ~ year_factor + f(month) +
    f(vessel) + f(locality) + f(depth) + f(latitude))

plot_cpue_index_coefs(m_cpue)
ggsave("~/Desktop/cpue-coefs.pdf", width = 10, height = 10)

predict_cpue_index(m_cpue, center = TRUE) %>%
  plot_cpue_index()
ggsave("~/Desktop/cpue-index.pdf", width = 10, height = 3)

predict_cpue_index(m_cpue, center = TRUE) %>%
  plot_cpue_index(all_models = FALSE)

g <- plot_cpue_index_jk(m_cpue)
ggsave("~/Desktop/cpue-index-jk.pdf", width = 10, height = 6)

g
g + theme_grey()
ggsave("~/Desktop/cpue-index-jk-grey.pdf", width = 10, height = 6)
g + theme_excel() +
  scale_colour_excel()
ggsave("~/Desktop/cpue-index-jk-excel.pdf", width = 10, height = 6)

## # ------------------------------

survey_samples  <- readRDS(file.path("..", "gfsynopsis", "report",
  "data-cache", "pbs-survey-samples.rds")) %>%
  filter(species_common_name == "yelloweye rockfish")

g <- tidy_ages_raw(survey_samples) %>%
  plot_ages()

g <- tidy_ages_raw(survey_samples,
  survey_series_desc = c(
    "PHMA Rockfish Longline Survey - Outside North",
    "PHMA Rockfish Longline Survey - Outside South",
    "IPHC Longline Survey"),
  survey = c("PHMA LL (N)", "PHMA LL (S)", "IPHC")) %>%
  plot_ages()
ggsave("inst/yelloweye-ages.pdf", width = 6, height = 6)

g + theme_grey()
g + theme_excel() +
  scale_color_excel(breaks = c("M", "F"))


##
## d_survey_samples  <- readRDS(file.path("..", "gfsynopsis", "report",
##   "data-cache", "pbs-survey-samples.rds"))
## d <- filter(d_survey_samples, species_common_name == "pacific ocean perch")
##
## tidy_lengths_raw(d, year_lim = c(2002, 2017)) %>%
##   plot_lengths()
##
## g <- tidy_lengths_raw(d, year_lim = c(2002, 2017)) %>%
##   plot_lengths()
##
## g
##
## g + theme_grey()
## g + theme_excel()
##
