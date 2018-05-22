#' Make synopsis pages
#'
#' This is the main workhorse function that creates the synopsis pages.
#'
#' @param dat A data list object from [gfplot::cache_pbs_data()].
#' @param spp A species common name.
#' @param aspect The aspect ratio of the 2nd page.
#' @param short_page_height_ratio The aspect ratio of the shorter first page.
#' @param width The width of a page.
#' @param debug If `TRUE` then a debugging grid will be added to the plot
#'   layout.
#' @param resolution The resolution for a png page.
#' @param png_format If `TRUE` then the pages will be png instead of PDF.
#' @param spp_file A version of the species name to be used when generating file
#'   names.
#' @param report_folder The folder in which to create the report.
#' @param include_map_square If `TRUE` then a perfect square will be added to
#'   the maps to help adjust the figure layout to get the aspect ratio of these
#'   maps correct.
#' @param map_xlim The X axis limits in UTM coordinates.
#' @param map_ylim The Y axis limits in UTM coordinates.
#' @param save_gg_objects If `TRUE` then the ggplot2 objects will be saved as a
#'   list object in `file.path(report_folder, 'ggplot-objects')`. This will also
#'   cause the CPUE model to be cached.
#' @param survey_cols A vector of colors for the surveys.
#' @param survey_col_names A vector of names to associate with the colours.
#'   Should match the survey abbreviations.
#' @param mat_min_n The minimum number of samples before the maturity ogives are
#'   plotted.
#' @param survey_map_outlier A quantile above which of the colors in the map
#'   should be squashed the same color. Useful to avoid a small number of
#'   outlying cells from distorting the color scale.
#'
#' @return
#' This function generates 2 png files with all of the plots for a given species.
#'
#' @export
#' @importFrom grDevices dev.off pdf png
#' @importFrom ggplot2 labeller theme element_blank ggtitle
#' @importFrom grid unit
#' @importFrom INLA inla.models inla.reorderings

make_pages <- function(
  dat,
  spp,
  aspect = 1.35,
  short_page_height_ratio = 0.78,
  width = 11.5,
  debug = FALSE,
  resolution = 180,
  png_format = TRUE,
  spp_file = gfsynopsis:::clean_name(spp),
  report_folder = "report",
  include_map_square = FALSE,
  map_xlim = c(360, 653),
  map_ylim = c(5275, 6155),
  save_gg_objects = FALSE,
  survey_cols = c(RColorBrewer::brewer.pal(7L, "Set2"),
    "#303030", "#a8a8a8", "#a8a8a8", "#a8a8a8"),
  survey_col_names = c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI",
      "HBLL OUT N", "HBLL OUT S", "IPHC FISS", "Commercial",
      "HBLL INS N", "HBLL INS S", "MSA HS"),
  mat_min_n = 20,
  survey_map_outlier = 0.999
  ) {

  survey_cols <- stats::setNames(survey_cols, survey_col_names)

  # Internal setup calculations: -------------
  height <- width * aspect

  dat$survey_sets <- dplyr::filter(dat$survey_sets, species_common_name == spp)
  dat$survey_samples <- dplyr::filter(dat$survey_samples, species_common_name == spp)
  dat$comm_samples <- dplyr::filter(dat$comm_samples, species_common_name == spp)
  dat$catch <- dplyr::filter(dat$catch, species_common_name == spp)
  dat$cpue_spatial <- dplyr::filter(dat$cpue_spatial, species_common_name == spp)
  dat$cpue_spatial_ll <- dplyr::filter(dat$cpue_spatial_ll, species_common_name == spp)
  dat$survey_index <- dplyr::filter(dat$survey_index, species_common_name == spp)
  # TODO fix:
  dat$age_precision <- dplyr::filter(dat$age_precision,
    species_code == unique(dat$survey_sets$species_code))

  # TODO:
  dat$survey_samples$maturity_convention_maxvalue <- 1e6
  dat$comm_samples$maturity_convention_maxvalue <- 1e6

  dat$comm_samples_no_keepers <- dplyr::filter(dat$comm_samples,
    sampling_desc %in% "UNSORTED")
  dat$combined_samples <- bind_samples(dat$comm_samples, dat$survey_samples)

  # TODO: temp:
  dat$survey_index$survey_abbrev <- gsub("_", " ", dat$survey_index$survey_abbrev)
  dat$survey_index$survey_abbrev <-
    ifelse(dat$survey_index$survey_series_desc ==
        "Hecate Strait Multispecies Assemblage Bottom Trawl", "MSA HS",
      dat$survey_index$survey_abbrev)

  # File and folder setup: --------------------------

  fig_folder <- file.path(report_folder, "figure-pages")
  ggplot_folder <- file.path(report_folder, "ggplot-objects")
  cpue_cache <- file.path(report_folder, "cpue-cache")
  survey_map_cache <- file.path(report_folder, "map-cache")
  vb_cache <- file.path(report_folder, "vb-cache")

  dir.create(fig_folder, showWarnings = FALSE, recursive = TRUE)
  dir.create(ggplot_folder, showWarnings = FALSE, recursive = TRUE)
  dir.create(cpue_cache, showWarnings = FALSE, recursive = TRUE)
  dir.create(survey_map_cache, showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(survey_map_cache, "synoptic"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(survey_map_cache, "iphc"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(survey_map_cache, "hbll"), showWarnings = FALSE, recursive = TRUE)
  dir.create(vb_cache, showWarnings = FALSE, recursive = TRUE)

  gg_folder_spp <- paste0(file.path(ggplot_folder, spp_file), ".rds")
  fig_folder_spp1 <- paste0(file.path(fig_folder, spp_file), if (png_format) "-1.png" else "-1.pdf")
  fig_folder_spp2 <- paste0(file.path(fig_folder, spp_file), if (png_format) "-2.png" else "-2.pdf")
  cpue_cache_spp <- paste0(file.path(cpue_cache, spp_file), ".rds")
  map_cache_spp_synoptic <- paste0(file.path(survey_map_cache, "synoptic", spp_file), ".rds")
  map_cache_spp_iphc <- paste0(file.path(survey_map_cache, "iphc", spp_file), ".rds")
  map_cache_spp_hbll <- paste0(file.path(survey_map_cache, "hbll", spp_file), ".rds")
  vb_cache_spp <- paste0(file.path(vb_cache, spp_file), ".rds")

  samp_panels <- c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI", "HBLL OUT N",
    "HBLL OUT S", "IPHC FISS", "Commercial")

  # Age compositions: -------------------------------

  ss <- tidy_ages_raw(dat$survey_samples,
    sample_type = "survey")
  sc <- tidy_ages_raw(dat$comm_samples_no_keepers,
    sample_type = "commercial")

  if (!is.na(sc[[1]])) sc <- sc %>% filter(year >= 2003)
  if (is.data.frame(sc))
    if (nrow(sc) == 0)
      sc <- NA

  if (!is.na(ss[[1]]) && !is.na(sc[[1]])) {
    sb <- suppressWarnings(bind_rows(ss, sc))
  }
  if (!is.na(ss[[1]]) && is.na(sc[[1]])) {
    sb <- ss
  }
  if (is.na(ss[[1]]) && !is.na(sc[[1]])) {
    sb <- sc
  }
  if (is.na(ss[[1]]) && is.na(sc[[1]])) {
    sb <- NA
  }
  if (!is.na(sb)) {
    sb$survey_abbrev <- factor(sb$survey_abbrev,
      levels = samp_panels)
    g_ages <- plot_ages(sb, survey_cols = survey_cols, year_range = c(2003, 2017)) +
      guides(fill = FALSE, colour = FALSE)
  } else {
    g_ages <- plot_ages(expand.grid(
      survey_abbrev = factor(x = samp_panels, levels = samp_panels),
      year = seq(2004, 2016, 2),
      sex = NA, age = 0, proportion = 0, total = 1, stringsAsFactors = FALSE),
      year_range = c(2003, 2017)) +
      guides(fill = FALSE, colour = FALSE) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }

  # Length compositions: -------------------------------

  bin_width1 <- diff(range(dat$survey_samples$length, na.rm = TRUE)) / 30
  bin_width2 <- diff(range(dat$comm_samples_no_keepers$length, na.rm = TRUE)) / 30
  bin_width <- mean(bin_width1, bin_width2, na.rm = TRUE)

  ss <- tidy_lengths_raw(dat$survey_samples, bin_size = bin_width,
    sample_type = "survey")
  sc <- tidy_lengths_raw(dat$comm_samples_no_keepers, bin_size = bin_width,
    sample_type = "commercial")

  if (!is.na(sc[[1]])) sc <- sc %>% filter(year >= 2003)
  if (is.data.frame(sc))
    if (nrow(sc) == 0)
      sc <- NA

  if (!is.na(ss[[1]]) && !is.na(sc[[1]])) {
    sb <- suppressWarnings(bind_rows(ss, sc))
  }
  if (!is.na(ss[[1]]) && is.na(sc[[1]])) {
    sb <- ss
  }
  if (is.na(ss[[1]]) && !is.na(sc[[1]])) {
    sb <- sc
  }
  if (is.na(ss[[1]]) && is.na(sc[[1]])) {
    sb <- NA
  }

  if (!is.na(sb)) {
    sb$survey_abbrev <- factor(sb$survey_abbrev,
      levels = c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI", "HBLL OUT N",
        "HBLL OUT S", "IPHC FISS", "Commercial"))
    sb$year <- factor(sb$year, levels = seq(2003, 2017))
    g_lengths <- plot_lengths(sb, survey_cols = survey_cols, bin_size = bin_width) +
      guides(colour = FALSE, fill = FALSE)
  } else {
    g_lengths <- ggplot() + theme_pbs() + ggtitle("Length frequencies")
  }

  # Aging precision: -------------------------------

  if (nrow(dat$age_precision) > 0) {
  g_age_precision <- tidy_age_precision(dat$age_precision) %>%
    plot_age_precision()
  } else {
    g_age_precision <- ggplot() + theme_pbs()
  }

  # Commercial CPUE indices: -------------------------------

  if (nrow(dat$catch) > 0) {
    if (!file.exists(cpue_cache_spp)) {
      cpue_index <- gfsynopsis::fit_cpue_indices(dat$cpue_index,
        species = unique(dat$catch$species_common_name),
        save_model = save_gg_objects)
      saveRDS(cpue_index, file = cpue_cache_spp, compress = FALSE)
    } else {
      cpue_index <- readRDS(cpue_cache_spp)
    }

    if (!is.na(cpue_index[[1]])) { # enough vessels?

      g_cpue_index <- gfsynopsis::plot_cpue_indices(cpue_index) +
        ggplot2::ggtitle("Commercial trawl CPUE") +
        ylab("") + xlab("") +
        ggplot2::theme(
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
    }
  }
  if (nrow(dat$catch) == 0 || is.na(cpue_index[[1]])) {
    g_cpue_index <-
      gfsynopsis::plot_cpue_indices(
        expand.grid(area = factor(c("3CD|5ABCDE", "5AB", "5CDE", "3CD"),
          levels = c("3CD|5ABCDE", "5AB", "5CDE", "3CD")), year = 2000,
          est = NA, lwr = NA, upr = NA), blank_plot = TRUE) +
      ggplot2::ggtitle("Commercial trawl CPUE") +
      ylab("") + xlab("") +
      ggplot2::theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
  }

  # Commercial catch: -------------------------------

  if (nrow(dat$catch) > 0) {
    g_catch <- gfsynopsis::plot_catches(dat$catch)
  } else {
    g_catch <- ggplot() + theme_pbs()
    g_catch <- gfsynopsis::plot_catches(expand.grid(year = 999,
      area = factor(c("Coastwide", "5AB", "5CDE", "3CD"),
        levels = c("Coastwide", "5AB", "5CDE", "3CD")),
      gear = "abc", value = 1, stringsAsFactors = FALSE), blank_plot = TRUE) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }

  # Survey biomass indices: -------------------------------

  g_survey_index <- tidy_survey_index(dat$survey_index) %>%
    plot_survey_index(col = c("grey60", "grey20"), survey_cols = survey_cols,
      xlim = c(1984, 2017)) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) + ggplot2::ggtitle("Survey relative biomass indices")

  # Specimen numbers: -------------------------------

  suppressMessages({
    g_comm_samples <- tidy_sample_avail(dat$comm_samples) %>%
      plot_sample_avail(title = "Commercial samples", year_range = c(1996, 2017)) +
      # ggplot2::scale_fill_distiller(palette = "Greys", na.value = "white", direction = 1) +
      viridis::scale_fill_viridis(option = "D", end = 0.82, na.value = "grey75") +
      ggplot2::ggtitle("Commercial specimen counts")

    g_survey_samples <- tidy_sample_avail(dat$survey_samples) %>%
      plot_sample_avail(title = "Survey samples", year_range = c(1996, 2017)) +
      # ggplot2::scale_fill_distiller(palette = "Greys", na.value = "white", direction = 1) +
      viridis::scale_fill_viridis(option = "C", end = 0.82, na.value = "grey75") +
      ggplot2::ggtitle("Survey specimen counts")
  })

  # Maturity by month: -------------------------------

  # TODO: should this include commercial?
  g_maturity_month <- tidy_maturity_months(dat$survey_samples) %>%
    plot_maturity_months() +
    guides(colour = FALSE, fill = FALSE)

  # Growth fits: -------------------------------

  if (!file.exists(vb_cache_spp)) {
    vb_m <- fit_vb(dat$combined_samples, sex = "male", method = "mpd",
      too_high_quantile = 0.99)
    vb_f <- fit_vb(dat$combined_samples, sex = "female", method = "mpd",
      too_high_quantile = 0.99)
    vb <- list()
    vb$m <- vb_m
    vb$f <- vb_f
    saveRDS(vb, file = vb_cache_spp, compress = FALSE)
  } else {
    vb <- readRDS(vb_cache_spp)
  }

  g_vb <- plot_vb(object_female = vb$f, object_male = vb$m) +
    guides(colour = FALSE, fill = FALSE, lty = FALSE)

  lw_m <- fit_length_weight(dat$combined_samples, sex = "male", method = "rlm",
    too_high_quantile = 0.99)
  lw_f <- fit_length_weight(dat$combined_samples, sex = "female", method = "rlm",
    too_high_quantile = 0.99)

  g_length_weight <-
    plot_length_weight(object_female = lw_f, object_male = lw_m) +
    ggplot2::theme(legend.position = c(0.9, 0.2),
      legend.key.width = grid::unit(1.8, units = "char")) +
    ggplot2::guides(lty =
        guide_legend(override.aes = list(lty = c(1, 2), lwd = c(.7, .7))))

  # Maturity ogives: -------------------------------

  mat_age <- dat$combined_samples %>%
    fit_mat_ogive(
      type = "age",
      months = seq(1, 12))

  type <- "none"
  if (!is.na(mat_age[[1]])) {
    sample_size <- mat_age$data %>% group_by(female, mature) %>%
      summarise(N = n()) %>%
      group_by(female) %>%
      summarise(N_min = min(N))
    if (sample_size[sample_size$female == 0, "N_min", drop = TRUE] < mat_min_n &&
        sample_size[sample_size$female == 1, "N_min", drop = TRUE] < mat_min_n)
      type <- "none"
    if (sample_size[sample_size$female == 0, "N_min", drop = TRUE] >= mat_min_n &&
        sample_size[sample_size$female == 1, "N_min", drop = TRUE] >= mat_min_n)
      type <- "all"
    if (sample_size[sample_size$female == 0, "N_min", drop = TRUE] >= mat_min_n &&
        sample_size[sample_size$female == 1, "N_min", drop = TRUE] < mat_min_n)
      type <- "male"
    if (sample_size[sample_size$female == 0, "N_min", drop = TRUE] < mat_min_n &&
        sample_size[sample_size$female == 1, "N_min", drop = TRUE] >= mat_min_n)
      type <- "female"
  }

  if (!is.na(mat_age[[1]])) {
    g_mat_age <- plot_mat_ogive(mat_age, prediction_type = type) +
      guides(colour = FALSE, fill = FALSE, lty = FALSE) +
      ggplot2::guides(lty = FALSE, colour = FALSE)
  } else {
    g_mat_age <- ggplot() + theme_pbs() + ggtitle("Age at maturity") +
      ggplot2::labs(x = "Age (years)", y = "Probability mature") +
      ggplot2::guides(lty = FALSE, colour = FALSE)
  }

  mat_length <- dat$combined_samples %>%
    fit_mat_ogive(
      type = "length",
      months = 1:12)

  type <- "none"
  if (!is.na(mat_age[[1]])) {
    sample_size <- mat_length$data %>% group_by(female, mature) %>%
      summarise(N = n()) %>%
      group_by(female) %>%
      summarise(N_min = min(N))
    if (sample_size[sample_size$female == 0, "N_min", drop = TRUE] < mat_min_n &&
        sample_size[sample_size$female == 1, "N_min", drop = TRUE] < mat_min_n)
      type <- "none"
    if (sample_size[sample_size$female == 0, "N_min", drop = TRUE] >= mat_min_n &&
        sample_size[sample_size$female == 1, "N_min", drop = TRUE] >= mat_min_n)
      type <- "all"
    if (sample_size[sample_size$female == 0, "N_min", drop = TRUE] >= mat_min_n &&
        sample_size[sample_size$female == 1, "N_min", drop = TRUE] < mat_min_n)
      type <- "male"
    if (sample_size[sample_size$female == 0, "N_min", drop = TRUE] < mat_min_n &&
        sample_size[sample_size$female == 1, "N_min", drop = TRUE] >= mat_min_n)
      type <- "female"
  }

  if (!is.na(mat_length[[1]])) {
    g_mat_length <- plot_mat_ogive(mat_length, prediction_type = type) +
      ggplot2::theme(legend.position = c(0.9, 0.2),
      legend.key.width = grid::unit(1.8, units = "char")) +
      ggplot2::guides(lty =
          guide_legend(override.aes = list(lty = c(1, 2), lwd = c(.7, .7))))
  } else {
    g_mat_length <- ggplot() + theme_pbs() + ggtitle("Length at maturity") +
      ggplot2::labs(x = "Length (cm)", y = "Probability mature") +
      ggplot2::guides(lty = FALSE, colour = FALSE)
  }

  # Commercial CPUE maps -------------------------------

  coord_cart <- coord_cartesian(xlim = map_xlim, ylim = map_ylim)

  # for checking if aspect ratio of map is 1:1
  checking_square <- geom_polygon(data = data.frame(x = c(400, 600, 600, 400),
    y = c(5500, 5500, 5700, 5700)), aes_string(x = "x", y = "y"),
    inherit.aes = FALSE, fill = "grey50", lwd = 1, col = "black")

  g_cpue_spatial <- dplyr::filter(dat$cpue_spatial, year >= 2012) %>%
    plot_cpue_spatial(bin_width = 7, n_minimum_vessels = 3,
      rotation_angle = 40, xlim = map_xlim, ylim = map_ylim) +
    ggplot2::ggtitle("Commercial trawl CPUE") +
    theme(legend.position = "none") +
    coord_cart + theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )

  if (include_map_square)
    g_cpue_spatial <- g_cpue_spatial + checking_square

  g_cpue_spatial_ll <- filter(dat$cpue_spatial_ll, year >= 2008) %>%
    plot_cpue_spatial(bin_width = 7, n_minimum_vessels = 3,
      rotation_angle = 40, xlim = map_xlim, ylim = map_ylim,
      fill_lab = "CPUE (kg/fe)") +
    ggplot2::ggtitle("Commercial H & L CPUE") +
    theme(legend.position = "none") +
    coord_cart + theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )

  # Survey maps: -------------------------------
  if (!file.exists(map_cache_spp_synoptic)) {
    dat$survey_sets$density_kgpm2 <- dat$survey_sets$density_kgpm2 * 1000
    syn_fits <- gfsynopsis::fit_survey_maps(dat$survey_sets,
      species = spp, model = "inla",
      surveys = c("SYN QCS", "SYN HS", "SYN WCHG", "SYN WCVI"),
      verbose = FALSE, max_edge = c(20, 100))
    syn_fits$models <- NULL # save space
    saveRDS(syn_fits, file = map_cache_spp_synoptic, compress = FALSE)
  } else {
    syn_fits <- readRDS(map_cache_spp_synoptic)
  }

  if (!file.exists(map_cache_spp_iphc)) {
    iphc_fits <- gfsynopsis::fit_survey_maps(dat$survey_sets,
      species = spp, model = "inla",
      surveys = "IPHC FISS",
      verbose = FALSE, max_edge = c(30, 100))
    iphc_fits$models <- NULL # save space
    saveRDS(iphc_fits, file = map_cache_spp_iphc, compress = FALSE)
  } else {
    iphc_fits <- readRDS(map_cache_spp_iphc)
  }

  if (!file.exists(map_cache_spp_hbll)) {
    hbll_fits <- gfsynopsis::fit_survey_maps(dat$survey_sets,
      species = spp, model = "inla",
      surveys = "HBLL OUT",
      verbose = FALSE, max_edge = c(30, 100))
    hbll_fits$models <- NULL # save space
    saveRDS(hbll_fits, file = map_cache_spp_hbll, compress = FALSE)
  } else {
    hbll_fits <- readRDS(map_cache_spp_hbll)
  }

  # squash massive outlying cells for colour scale goodness:
  if (!is.null(syn_fits$pred_dat$combined)) {
    qs <- quantile(syn_fits$pred_dat$combined, probs = survey_map_outlier, na.rm = TRUE)[[1]]
    syn_fits$pred_dat$combined[syn_fits$pred_dat$combined > qs] <- qs
  }
  if (!is.null(hbll_fits$pred_dat$combined)) {
    qs <- quantile(hbll_fits$pred_dat$combined, probs = survey_map_outlier, na.rm = TRUE)[[1]]
    hbll_fits$pred_dat$combined[hbll_fits$pred_dat$combined > qs] <- qs
  }

  g_survey_spatial_syn <-
    gfsynopsis::plot_survey_maps(syn_fits$pred_dat, syn_fits$raw_dat,
      north_symbol = TRUE, annotations = "SYN") +
    coord_cart + ggplot2::ggtitle("Synoptic survey biomass")

  if (sum(iphc_fits$raw_dat$present) > 0.02 * nrow(iphc_fits$raw_dat))
    show_model_predictions <- TRUE
  else
    show_model_predictions <- FALSE
  g_survey_spatial_iphc <-
    gfsynopsis::plot_survey_maps(iphc_fits$pred_dat, iphc_fits$raw_dat,
      show_raw_data = FALSE, cell_size = 2.0, circles = TRUE,
      show_model_predictions = show_model_predictions, annotations = "IPHC") +
    coord_cart + ggplot2::ggtitle("IPHC survey biomass")

  if (sum(hbll_fits$raw_dat$present) > 0.02 * nrow(hbll_fits$raw_dat))
    show_model_predictions <- TRUE
  else
    show_model_predictions <- FALSE
  g_survey_spatial_hbll <-
    gfsynopsis::plot_survey_maps(hbll_fits$pred_dat, hbll_fits$raw_dat,
      pos_pt_col = "#FFFFFF35",
      bin_pt_col = "#FFFFFF12",
      pos_pt_fill = "#FFFFFF03",
      show_model_predictions = show_model_predictions, annotations = "HBLL") +
    coord_cart + ggplot2::ggtitle("HBLL OUT survey biomass")

  # Page 1 layout: -------------------------------

  gg_catch               <- ggplot2::ggplotGrob(g_catch)
  gg_survey_index        <- ggplot2::ggplotGrob(g_survey_index)
  gg_cpue_spatial        <- ggplot2::ggplotGrob(g_cpue_spatial)
  gg_cpue_spatial_ll     <- ggplot2::ggplotGrob(g_cpue_spatial_ll)
  gg_cpue_index          <- ggplot2::ggplotGrob(g_cpue_index)
  gg_survey_spatial_syn  <- ggplot2::ggplotGrob(g_survey_spatial_syn)
  gg_survey_spatial_iphc <- ggplot2::ggplotGrob(g_survey_spatial_iphc)
  gg_survey_spatial_hbll <- ggplot2::ggplotGrob(g_survey_spatial_hbll)

  fg_catch               <- egg::gtable_frame(gg_catch, debug = debug)
  fg_survey_index        <- egg::gtable_frame(gg_survey_index, debug = debug)
  fg_cpue_spatial        <- egg::gtable_frame(gg_cpue_spatial, debug = debug)
  fg_cpue_spatial_ll     <- egg::gtable_frame(gg_cpue_spatial_ll, debug = debug)
  fg_cpue_index          <- egg::gtable_frame(gg_cpue_index, debug = debug,
                                              width = grid::unit(0.7, "null"))
  fg_survey_spatial_syn  <- egg::gtable_frame(gg_survey_spatial_syn, debug = debug)
  fg_survey_spatial_iphc <- egg::gtable_frame(gg_survey_spatial_iphc, debug = debug)
  fg_survey_spatial_hbll <- egg::gtable_frame(gg_survey_spatial_hbll, debug = debug)

  f_topleft <- egg::gtable_frame(
    fg_survey_index,
    width = grid::unit(1, "null"),
    height = grid::unit(1, "null"),
    debug = debug)

  f_topright <- egg::gtable_frame(
    gridExtra::gtable_cbind(fg_catch, fg_cpue_index),
    width = grid::unit(1, "null"),
    height = grid::unit(1, "null"),
    debug = debug)

  f_fake_text <- egg::gtable_frame(
    ggplot2::ggplotGrob(ggplot2::ggplot() + ggplot2::ggtitle(" ") + theme_pbs()),
    width = grid::unit(1, "null"),
    height = grid::unit(0.3, "null"),
    debug = debug)

  f_top <- egg::gtable_frame(
    gridExtra::gtable_cbind(f_topleft, f_topright),
    width = grid::unit(1, "null"),
    height = grid::unit(1, "null"),
    debug = debug)

  f_bottom <- egg::gtable_frame(
    gridExtra::gtable_cbind(
      fg_survey_spatial_syn, fg_survey_spatial_hbll, fg_survey_spatial_iphc,
      fg_cpue_spatial, fg_cpue_spatial_ll),
    width = grid::unit(1, "null"),
    height = grid::unit(1.35, "null"),
    debug = debug)

  f_all <- gridExtra::gtable_rbind(f_top, f_bottom)

  if (png_format) {
    png(fig_folder_spp1, width = width * resolution,
      height = height * resolution * short_page_height_ratio, res = resolution)
  } else {
    pdf(fig_folder_spp1, width = width, height = height * short_page_height_ratio)
  }
  grid::grid.newpage()
  grid::grid.draw(f_all)
  dev.off()

  # Page 2 layout: -------------------------------

  gg_mat_age        <- ggplot2::ggplotGrob(g_mat_age)
  gg_mat_length     <- ggplot2::ggplotGrob(g_mat_length)
  gg_mat_month      <- ggplot2::ggplotGrob(g_maturity_month)
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

  f_very_bottom <- egg::gtable_frame(
    gridExtra::gtable_cbind(fg_survey_samples, fg_comm_samples),
    width = grid::unit(1, "null"),
    height = grid::unit(0.17, "null"),
    debug = debug)

  f_all <- gridExtra::gtable_rbind(f_top, f_middle, f_bottom, f_very_bottom)

  if (png_format) {
    png(fig_folder_spp2, width = width * resolution,
      height = height * resolution, res = resolution)
  } else {
    pdf(fig_folder_spp2, width = width, height = height)
  }
  grid::grid.newpage()
  grid::grid.draw(f_all)
  dev.off()

  if (save_gg_objects) {
    plots <- list()
    plots$age_precision <- g_age_precision
    plots$ages <- g_ages
    plots$lengths <- g_lengths
    plots$catch <- g_catch
    plots$comm_samples <- g_comm_samples
    plots$survey_samples <- g_survey_samples
    plots$mat_age <- g_mat_age
    plots$mat_length <- g_mat_length
    plots$vb <- g_vb
    plots$cpue_index <- g_cpue_index
    plots$cpue_spatial_ll <- g_cpue_spatial_ll
    plots$cpue_index <- g_cpue_index
    plots$maturity_month <- g_maturity_month
    plots$cpue_spatial <- g_cpue_spatial
    plots$cpue_spatial_ll <- g_cpue_spatial_ll
    plots$survey_spatial_hbll <- g_survey_spatial_hbll
    plots$survey_spatial_iphc <- g_survey_spatial_iphc
    plots$survey_spatial_syn <- g_survey_spatial_syn
    plots$length_weight <- g_length_weight
    plots$survey_index <- g_survey_index
    saveRDS(plots, file = gg_folder_spp, compress = FALSE)
  }
}
