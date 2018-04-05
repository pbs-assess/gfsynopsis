#' Make pages
#'
#' @param dat TODO
#' @param spp TODO
#' @param output_path TODO
#' @param aspect TODO
#' @param width TODO
#' @param survey_cols TODO
#' @param debug TODO
#'
#' importFrom grDevices dev.off pdf
#' @export
#' @examples
#' \dontrun{
#' library("gfplot")
#' spp <- "arrowtooth flounder"
#' dc <- "data-cache"
#' dir.create(dc, showWarnings = FALSE)
#' dir.create("figs", showWarnings = FALSE)
#' cache_pbs_data(spp, path = dc)
#'
#' dat <- list()
#' dat$survey_tows     <- readRDS(file.path(dc, "pbs-surv-tows.rds"))
#' dat$survey_samples  <- readRDS(file.path(dc, "pbs-surv-samples.rds"))
#' dat$comm_samples    <- readRDS(file.path(dc, "pbs-comm-samples.rds"))
#' dat$catch           <- readRDS(file.path(dc, "pbs-catch.rds"))
#' dat$cpue_spatial    <- readRDS(file.path(dc, "pbs-cpue-spatial.rds"))
#' dat$cpue_spatial_ll <- readRDS(file.path(dc, "pbs-cpue-spatial-ll.rds"))
#' dat$survey_index    <- readRDS(file.path(dc, "pbs-surv-index.rds"))
#' dat$age_precision   <- readRDS(file.path(dc, "pbs-age-precision.rds"))
#'
#' make_pages(dat, spp, output_path = "figs")
#' }

make_pages <- function(dat,
                       spp,
                       output_path,
                       aspect = 1.35,
                       width = 11.5,
                       survey_cols = gg_color_hue(7L),
                       debug = FALSE,
                       ageing_method_codes = c(3, 17),
                       survey_age_year_range =
                         c(2002, as.numeric(format(Sys.Date(), "%Y")))) {

  height <- width * aspect

  survey_cols <- stats::setNames(survey_cols,
    c("WCHG", "HS", "QCS", "WCVI", "PHMA LL (N)", "PHMA LL (S)", "IPHC"))

  survey_index_cols <-
    c(survey_cols[seq_len(6)], rep("#666666", 3), survey_cols[7])

  g_ages <- tidy_ages_raw(dat$survey_samples, year_range = survey_age_year_range,
  ageing_method_codes = ageing_method_codes) %>%
    plot_ages(survey_cols = survey_cols, year_range = survey_age_year_range)

  g_lengths <- tidy_lengths_raw(dat$survey_samples, bin_size = 2.5,
  year_lim = c(2002, Inf)) %>%
    plot_lengths(survey_cols = survey_cols)

  g_age_precision <- tidy_age_precision(dat$age_precision,
    ageing_method_codes = ageing_method_codes) %>%
    plot_age_precision()

  g_catch <- tidy_catch(dat$catch) %>%
    plot_catch()

  g_survey_index <- tidy_survey_index(dat$survey_index) %>%
    plot_survey_index(survey_cols = survey_index_cols)

  g_comm_samples <- tidy_sample_avail(dat$comm_samples) %>%
    plot_sample_avail(title = "Commercial samples", year_range = c(1994, 2017))

  g_survey_samples <- tidy_sample_avail(dat$survey_samples) %>%
    plot_sample_avail(title = "Survey samples", year_range = c(1994, 2017))

  g_blank <- ggplot() + gfplot::theme_pbs()

  g_mat_month <- tidy_maturity_months(dat$survey_samples) %>%
    plot_maturity_months() +
    ggplot2::guides(fill = FALSE, colour = FALSE)

  vb_m <- fit_vb(dat$survey_samples, sex = "male", method = "mpd")
  vb_f <- fit_vb(dat$survey_samples, sex = "female", method = "mpd")
  g_vb <- plot_vb(object_female = vb_m, object_male = vb_f) +
    guides(colour = FALSE, fill = FALSE)

  lw_m <- fit_length_weight(dat$survey_samples, sex = "male", method = "rlm")
  lw_f <- fit_length_weight(dat$survey_samples, sex = "female", method = "rlm")
  g_length_weight <- plot_length_weight(object_female = lw_m, object_male = lw_f) +
    guides(colour = FALSE, fill = FALSE)

  mat_age <- dat$survey_samples %>%
    fit_mat_ogive(
      type = "age",
      months = seq(4, 6),
      ageing_method = c(3, 17))
  g_mat_age <- plot_mat_ogive(mat_age) +
    guides(colour = FALSE, fill = FALSE)

  mat_length <- dat$survey_samples %>%
    fit_mat_ogive(
      type = "length",
      months = seq(4, 6),
      ageing_method = c(3, 17))
  g_mat_length <- plot_mat_ogive(mat_length) +
    guides(colour = FALSE, fill = FALSE)

  g_cpue_spatial <- dplyr::filter(dat$cpue_spatial, year >= 2012) %>%
    plot_cpue_spatial(bin_width = 7, n_minimum_vessels = 3) +
    ggplot2::ggtitle("Trawl CPUE") #+
  # labs(subtitle = "Since 2012; including discards; 3-vessel minimum")

  g_cpue_spatial_ll <- dplyr::filter(dat$cpue_spatial_ll, year >= 2008) %>%
    plot_cpue_spatial(bin_width = 7, n_minimum_vessels = 3,
    fill_lab = "CPUE (kg/fe)") +
    ggplot2::ggtitle("Hook and line CPUE") #+
  # labs(subtitle = "Since 2008; excluding discards; 3-vessel minimum")

  ## Page 2
  gg_mat_age        <- ggplot2::ggplotGrob(g_mat_age)
  gg_mat_length     <- ggplot2::ggplotGrob(g_mat_length)
  gg_mat_month      <- ggplot2::ggplotGrob(g_mat_month)
  gg_lengths        <- ggplot2::ggplotGrob(g_lengths)
  gg_ages           <- ggplot2::ggplotGrob(g_ages)
  gg_length_weight  <- ggplot2::ggplotGrob(g_length_weight)
  gg_vb             <- ggplot2::ggplotGrob(g_vb)
  gg_comm_samples   <- ggplot2::ggplotGrob(g_comm_samples)
  gg_survey_samples <- ggplot2::ggplotGrob(g_survey_samples)

  fg_mat_age        <- egg::gtable_frame(gg_mat_age, debug = debug)
  fg_mat_length     <- egg::gtable_frame(gg_mat_length, debug = debug)
  fg_mat_month      <- egg::gtable_frame(gg_mat_month, debug = debug)
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
    gridExtra::gtable_cbind(fg_mat_age, fg_mat_length, fg_mat_month),
    width = grid::unit(1, "null"),
    height = grid::unit(0.3, "null"),
    debug = debug)

  f_very_top <- egg::gtable_frame(
    gridExtra::gtable_cbind(fg_comm_samples, fg_survey_samples),
    width = grid::unit(1, "null"),
    height = grid::unit(0.17, "null"),
    debug = debug)

  f_all <- gridExtra::gtable_rbind(f_very_top, f_top, f_middle, f_bottom)

  pdf(file.path(output_path, paste0(clean_name(spp), "-2.pdf")),
    width = width, height = height)
  grid::grid.newpage()
  grid::grid.draw(f_all)
  dev.off()

  # ## Page 1
  # gg_catch           <- ggplot2::ggplotGrob(g_catch)
  # gg_survey_index    <- ggplot2::ggplotGrob(g_survey_index)
  # gg_cpue_spatial    <- ggplot2::ggplotGrob(g_cpue_spatial + xlim(290, 800))
  # gg_cpue_spatial_ll <- ggplot2::ggplotGrob(g_cpue_spatial_ll)
  # gg_cpue_index      <- ggplot2::ggplotGrob(g_blank + ggplot2::ggtitle("Commercial CPUE"))
  # gg_survey_spatial  <- ggplot2::ggplotGrob(g_blank + ggplot2::ggtitle("Surveys"))
  #
  # fg_catch           <- egg::gtable_frame(gg_catch, debug = debug)
  # fg_survey_index    <- egg::gtable_frame(gg_survey_index, debug = debug)
  # fg_cpue_spatial    <- egg::gtable_frame(gg_cpue_spatial, debug = debug)
  # fg_cpue_spatial_ll <- egg::gtable_frame(gg_cpue_spatial_ll, debug = debug)
  # fg_cpue_index      <- egg::gtable_frame(gg_cpue_index, debug = debug)
  # fg_survey_spatial  <- egg::gtable_frame(gg_survey_spatial, debug = debug)
  #
  # f_topleft <- egg::gtable_frame(
  #   fg_survey_index,
  #   width = grid::unit(1, "null"),
  #   height = grid::unit(1, "null"),
  #   debug = debug)
  #
  # f_topright <- egg::gtable_frame(
  #   gridExtra::gtable_rbind(fg_catch, fg_cpue_index, fg_cpue_index),
  #   width = grid::unit(1, "null"),
  #   height = grid::unit(1, "null"),
  #   debug = debug)
  #
  # f_fake_text <- egg::gtable_frame(ggplot2::ggplotGrob(
  #   g_blank + ggplot2::ggtitle("Description here")),
  #   width = grid::unit(1, "null"),
  #   height = grid::unit(0.3, "null"),
  #   debug = debug)
  #
  # f_top <- egg::gtable_frame(
  #   gridExtra::gtable_cbind(f_topleft, f_topright),
  #   width = grid::unit(1, "null"),
  #   height = grid::unit(0.7, "null"),
  #   debug = debug)
  #
  # f_bottomleft <- egg::gtable_frame(
  #   fg_survey_spatial,
  #   width = grid::unit(1, "null"),
  #   height = grid::unit(1, "null"),
  #   debug = debug)
  #
  # f_bottomright <- egg::gtable_frame(
  #   # gridExtra::gtable_rbind(fg_cpue_spatial, fg_cpue_spatial_ll),
  #   gridExtra::gtable_rbind(fg_mat_month, fg_mat_month),
  #   width = grid::unit(1, "null"),
  #   height = grid::unit(1, "null"),
  #   debug = debug)
  #
  # f_bottom <- egg::gtable_frame(
  #   gridExtra::gtable_cbind(f_bottomleft, f_bottomright),
  #   width = grid::unit(1, "null"),
  #   height = grid::unit(1, "null"),
  #   debug = debug)
  #
  # f_all <- gridExtra::gtable_rbind(f_fake_text, f_top, f_bottom)
  #
  # pdf(file.path(output_path, paste0(clean_name(spp), "-1.pdf")),
  #   width = width, height = height)
  # grid::grid.newpage()
  # grid::grid.draw(f_all)
  # dev.off()

}
