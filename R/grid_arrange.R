make_pages <- function(debug = FALSE) {
  figs <- file.path("report", "yelloweye", "yelloweye-figs")
  dir.create(figs, showWarnings = FALSE)
  if (!file.exists(file.path("report", "yelloweye", "data-cache", "pbs-surv-tows.rds"))) { # random test
    cache_pbs_data("yelloweye rockfish", path = "data-cache")
  }
  cache <- file.path("report", "yelloweye", "data-cache")

  d_survey_tows     <- readRDS(file.path(cache, "pbs-surv-tows.rds"))
  d_survey_samples  <- readRDS(file.path(cache, "pbs-surv-samples.rds"))
  d_comm_samples    <- readRDS(file.path(cache, "pbs-comm-samples.rds"))
  d_catch           <- readRDS(file.path(cache, "pbs-catch.rds"))
  d_cpue_spatial    <- readRDS(file.path(cache, "pbs-cpue-spatial.rds"))
  d_cpue_spatial_ll <- readRDS(file.path(cache, "pbs-cpue-spatial-ll.rds"))
  d_survey_index    <- readRDS(file.path(cache, "pbs-surv-index.rds"))
  d_age_precision   <- readRDS(file.path(cache, "pbs-age-precision.rds"))

  survey_pal <- function(x) rev(gg_color_hue(x))
  # survey_pal <- function(x) RColorBrewer::brewer.pal(n = x, "Set2")

  load_all("../gfplot/")
  g_ages <- tidy_ages_raw(d_survey_samples) %>%
    plot_ages() + guides(colour = FALSE, fill = FALSE)

  g_lengths <- tidy_lengths_raw(d_survey_samples, bin_size = 2.5,
    year_lim = c(2002, Inf)) %>%
    plot_lengths(
      survey_col_function = survey_pal)

  g_age_precision <- tidy_age_precision(d_age_precision) %>%
    plot_age_precision()

  g_catch <- tidy_catch(d_catch) %>%
    plot_catch()

  g_survey_index <- tidy_survey_index(d_survey_index) %>%
    plot_survey_index()

  g_comm_samples <- tidy_sample_avail(d_comm_samples) %>%
    plot_sample_avail(title = "Commercial samples", year_range = c(1994, 2017))

  g_survey_samples <- tidy_sample_avail(d_survey_samples) %>%
    plot_sample_avail(title = "Survey samples", year_range = c(1994, 2017))

  vb_m <- fit_vb(d_survey_samples, sex = "male", method = "mpd")
  vb_f <- fit_vb(d_survey_samples, sex = "female", method = "mpd")
  g_vb <- plot_vb(object_female = vb_m, object_male = vb_f) +
    guides(colour = FALSE, fill = FALSE)

  lw_m <- fit_length_weight(d_survey_samples, sex = "male", method = "rlm")
  lw_f <- fit_length_weight(d_survey_samples, sex = "female", method = "rlm")
  g_length_weight <- plot_length_weight(object_female = lw_m, object_male = lw_f) +
    guides(colour = FALSE, fill = FALSE)

  mat_age <- d_survey_samples %>%
    fit_mat_ogive(
      type = "age",
      months = seq(4, 6),
      ageing_method = c(3, 17))
  g_mat_age <- plot_mat_ogive(mat_age) +
    guides(colour = FALSE, fill = FALSE)

  mat_length <- d_survey_samples %>%
    fit_mat_ogive(
      type = "length",
      months = seq(4, 6),
      ageing_method = c(3, 17))
  g_mat_length <- plot_mat_ogive(mat_length) +
    guides(colour = FALSE, fill = FALSE)

  g_cpue_spatial <- dplyr::filter(d_cpue_spatial, year >= 2012) %>%
    plot_cpue_spatial(bin_width = 7, n_minimum_vessels = 3) +
    ggtitle("Trawl CPUE") +
    labs(subtitle = "Since 2012; including discards; 3-vessel minimum")

  g_cpue_spatial_ll <- filter(d_cpue_spatial_ll, year >= 2008) %>%
    plot_cpue_spatial(bin_width = 7, n_minimum_vessels = 3,
      fill_lab = "CPUE (kg/fe)") +
    ggtitle("Hook and line CPUE") +
    labs(subtitle = "Since 2008; excluding discards; 3-vessel minimum")

  ## layout figure:

  gg_mat_age        <- ggplotGrob(g_mat_age)
  gg_mat_length     <- ggplotGrob(g_mat_length)
  gg_lengths        <- ggplotGrob(g_lengths)
  gg_ages           <- ggplotGrob(g_ages)
  gg_length_weight  <- ggplotGrob(g_length_weight)
  gg_vb             <- ggplotGrob(g_vb)
  gg_comm_samples   <- ggplotGrob(g_comm_samples)
  gg_survey_samples <- ggplotGrob(g_survey_samples)

  fg_mat_age        <- egg::gtable_frame(gg_mat_age, debug = debug)
  fg_mat_length     <- egg::gtable_frame(gg_mat_length, debug = debug)
  fg_lengths        <- egg::gtable_frame(gg_lengths, debug = debug)
  fg_ages           <- egg::gtable_frame(gg_ages, debug = debug)
  fg_length_weight  <- egg::gtable_frame(gg_length_weight, debug = debug)
  fg_vb             <- egg::gtable_frame(gg_vb, debug = debug)
  fg_comm_samples   <- egg::gtable_frame(gg_comm_samples, debug = debug)
  fg_survey_samples <- egg::gtable_frame(gg_survey_samples, debug = debug)

  f_topright <- egg::gtable_frame(
    gridExtra::gtable_rbind(fg_vb, fg_length_weight),
    width = grid::unit(1, "null"),
    height = grid::unit(1, "null"),
    debug = debug)

  f_topleft <- egg::gtable_frame(
    fg_lengths,
    width = grid::unit(2.2, "null"),
    height = grid::unit(1, "null"),
    debug = debug)

  f_top <- egg::gtable_frame(
    gridExtra::gtable_cbind(f_topleft, f_topright),
    width = grid::unit(1, "null"),
    height = grid::unit(1, "null"),
    debug = debug)

  f_middle <- egg::gtable_frame(
    fg_ages,
    width = grid::unit(1, "null"),
    height = grid::unit(0.75, "null"),
    debug = debug)

  f_bottom <- egg::gtable_frame(
    gridExtra::gtable_cbind(fg_mat_age, fg_mat_length, fg_mat_length),
    width = grid::unit(1, "null"),
    height = grid::unit(0.3, "null"),
    debug = debug)

  f_very_top <- egg::gtable_frame(
    gridExtra::gtable_cbind(fg_comm_samples, fg_survey_samples),
    width = grid::unit(1, "null"),
    height = grid::unit(0.17, "null"),
    debug = debug)

  f_all <- gridExtra::gtable_rbind(f_very_top, f_top, f_middle, f_bottom)
  assertthat::assert_that(identical(ncol(f_top), ncol(f_middle)))
  assertthat::assert_that(identical(ncol(f_top), ncol(f_bottom)))
  assertthat::assert_that(identical(ncol(f_top), ncol(f_very_top)))

  aspect <- 1.35 # aspect ratio of full page figure in Science Response
  width <- 10.5 # arbitrary scalar to get size looking right
  height <- width * aspect

  pdf("report/test.pdf", width = width, height = height)
  grid.newpage()
  grid.draw(f_all)
  dev.off()

}
