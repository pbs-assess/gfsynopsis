#' Make pages
#'
#' @param dat TODO
#' @param spp TODO
#' @param aspect TODO
#' @param width TODO
#' @param debug TODO
#' @param resolution TODO
#' @param png_format TODO
#' @param spp_file TODO
#' @param report_folder TODO
#' @param survey_cols TODO
#' @param survey_col_names TODO
#'
#' @export
#' @importFrom grDevices dev.off pdf png
#' @importFrom ggplot2 labeller

make_pages <- function(
  dat,
  spp,
  aspect = 1.35,
  short_page_height_ratio = 0.78,
  width = 11.5,
  debug = FALSE,
  resolution = 220,
  png_format = TRUE,
  spp_file = gfsynopsis:::clean_name(spp),
  report_folder = "report",
  include_map_square = FALSE,
  map_xlim = c(360, 653),
  map_ylim = c(5275, 6155),
  survey_cols = c(RColorBrewer::brewer.pal(7L, "Set2"),
    "#303030", "#a8a8a8", "#a8a8a8", "#a8a8a8"),
  survey_col_names = c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI",
      "HBLL OUT N", "HBLL OUT S", "IPHC FISS", "Commercial",
      "HBLL INS N", "HBLL INS S", "MSA HS")
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

  dat$comm_samples_no_keepers <- dplyr::filter(dat$comm_samples, keeper == FALSE)
  dat$combined_samples <- bind_samples(dat$comm_samples, dat$survey_samples)

  # TODO: temp:
  dat$survey_index$survey_abbrev <- gsub("_", " ", dat$survey_index$survey_abbrev)
  dat$survey_index$survey_abbrev <-
    ifelse(dat$survey_index$survey_series_desc ==
        "Hecate Strait Multispecies Assemblage Bottom Trawl", "MSA HS",
      dat$survey_index$survey_abbrev)

  # TODO: temp:
  # lookup <- unique(select(dat$survey_samples, survey_abbrev, survey_series_desc))
  # dat$survey_sets$survey_abbrev <- NULL # in case
  # dat$survey_sets <- left_join(dat$survey_sets, lookup)

  # File and folder setup: --------------------------

  fig_folder <- file.path(report_folder, "figure-pages")
  cpue_cache <- file.path(report_folder, "cpue-cache")
  survey_map_cache <- file.path(report_folder, "map-cache")
  vb_cache <- file.path(report_folder, "vb-cache")

  dir.create(fig_folder, showWarnings = FALSE, recursive = TRUE)
  dir.create(cpue_cache, showWarnings = FALSE, recursive = TRUE)
  dir.create(survey_map_cache, showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(survey_map_cache, "synoptic"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(survey_map_cache, "iphc"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(survey_map_cache, "hbll"), showWarnings = FALSE, recursive = TRUE)
  dir.create(vb_cache, showWarnings = FALSE, recursive = TRUE)

  fig_folder_spp1 <- paste0(file.path(fig_folder, spp_file), if (png_format) "-1.png" else "-1.pdf")
  fig_folder_spp2 <- paste0(file.path(fig_folder, spp_file), if (png_format) "-2.png" else "-2.pdf")
  cpue_cache_spp <- paste0(file.path(cpue_cache, spp_file), ".rds")
  map_cache_spp_synoptic <- paste0(file.path(survey_map_cache, "synoptic", spp_file), ".rds")
  map_cache_spp_iphc <- paste0(file.path(survey_map_cache, "iphc", spp_file), ".rds")
  map_cache_spp_hbll <- paste0(file.path(survey_map_cache, "hbll", spp_file), ".rds")
  vb_cache_spp <- paste0(file.path(vb_cache, spp_file), ".rds")

  # Age compositions: -------------------------------

  ss <- tidy_ages_raw(dat$survey_samples,
    sample_type = "survey")
  sc <- tidy_ages_raw(dat$comm_samples_no_keepers,
    sample_type = "commercial") %>%
    filter(year >= 2003)
  sb <- suppressWarnings(bind_rows(ss, sc))
  sb$survey_abbrev <- factor(sb$survey_abbrev,
    levels = c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI", "HBLL OUT N",
      "HBLL OUT S", "IPHC FISS", "Commercial"))
  g_ages <- plot_ages(sb, survey_cols = survey_cols) +
    guides(fill = FALSE, colour = FALSE)

  # Length compositions: -------------------------------

  ss <- tidy_lengths_raw(dat$survey_samples, bin_size = 2.5,
    sample_type = "survey")
  sc <- tidy_lengths_raw(dat$comm_samples_no_keepers, bin_size = 2.5,
    sample_type = "commercial") %>%
    filter(year >= 2003)
  sb <- suppressWarnings(bind_rows(ss, sc))
  sb$survey_abbrev <- factor(sb$survey_abbrev,
    levels = c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI", "HBLL OUT N",
      "HBLL OUT S", "IPHC FISS", "Commercial"))
  g_lengths <- plot_lengths(sb, survey_cols = survey_cols) +
    guides(colour = FALSE, fill = FALSE)

  # Aging precision: -------------------------------

  g_age_precision <- tidy_age_precision(dat$age_precision) %>%
    plot_age_precision()

  # Commercial CPUE indices: -------------------------------

  if (!file.exists(cpue_cache_spp)) {
    cpue_index <- gfsynopsis::fit_cpue_indices(dat$cpue_index,
      species = unique(dat$catch$species_common_name))
    saveRDS(cpue_index, file = cpue_cache_spp, compress = FALSE)
  } else {
    cpue_index <- readRDS(cpue_cache_spp)
  }

  g_cpue_index <- gfsynopsis::plot_cpue_indices(cpue_index) +
    ggplot2::ggtitle("Commercial trawl CPUE") +
    ylab("") + xlab("") +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )

  # Commercial catch: -------------------------------

  g_catch <- gfsynopsis::plot_catches(dat$catch) +
    theme(legend.position = "none")

  # Survey biomass indices: -------------------------------

  g_survey_index <- tidy_survey_index(dat$survey_index) %>%
    plot_survey_index(col = c("grey60", "grey20"), survey_cols = survey_cols) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) + ggplot2::ggtitle("Survey relative biomass indices")

  g_comm_samples <- tidy_sample_avail(dat$comm_samples) %>%
    plot_sample_avail(title = "Commercial samples", year_range = c(1996, 2017)) +
    viridis::scale_fill_viridis(option = "D", end = 0.82, na.value = "grey85")

  g_survey_samples <- tidy_sample_avail(dat$survey_samples) %>%
    plot_sample_avail(title = "Survey samples", year_range = c(1996, 2017)) +
    viridis::scale_fill_viridis(option = "C", end = 0.82, na.value = "grey85")

  # Maturity by month: -------------------------------

  # TODO: should this include commercial?
  g_maturity_month <- tidy_maturity_months(dat$survey_samples) %>%
    plot_maturity_months() +
    guides(colour = FALSE, fill = FALSE)

  # Growth fits: -------------------------------

  if (!file.exists(vb_cache_spp)) {
    # TODO: memory mapping problem:
    model_file <- system.file("stan", "vb.stan", package = "gfplot")
    mod <- rstan::stan_model(model_file)
    vb_m <- fit_vb(dat$combined_samples, sex = "male", method = "mpd")
    vb_f <- fit_vb(dat$combined_samples, sex = "female", method = "mpd")
    vb <- list()
    vb$m <- vb_m
    vb$f <- vb_f
    saveRDS(vb, file = vb_cache_spp, compress = FALSE)
  } else {
    vb <- readRDS(vb_cache_spp)
  }

  g_vb <- plot_vb(object_female = vb$m, object_male = vb$f) +
    guides(colour = FALSE, fill = FALSE)

  lw_m <- fit_length_weight(dat$combined_samples, sex = "male", method = "rlm")
  lw_f <- fit_length_weight(dat$combined_samples, sex = "female", method = "rlm")
  g_length_weight <-
    plot_length_weight(object_female = lw_m, object_male = lw_f) +
    guides(colour = FALSE, fill = FALSE)

  # Maturity ogives: -------------------------------

  mat_age <- dat$combined_samples %>%
    fit_mat_ogive(
      type = "age",
      months = 1:12)
  g_mat_age <- plot_mat_ogive(mat_age) +
    guides(colour = FALSE, fill = FALSE)

  mat_length <- dat$combined_samples %>%
    fit_mat_ogive(
      type = "length",
      months = 1:12)
  g_mat_length <- plot_mat_ogive(mat_length) +
    guides(colour = FALSE, fill = FALSE)

  # Commercial CPUE maps -------------------------------

  coord_cart <- coord_cartesian(xlim = map_xlim, ylim = map_ylim)

  # for checking if aspect ratio of map is 1:1
  checking_square <- geom_polygon(data = data.frame(x = c(400, 600, 600, 400),
    y = c(5500, 5500, 5700, 5700)), aes_string(x = "x", y = "y"),
    inherit.aes = FALSE, fill = "grey50", lwd = 1, col = "black")

  g_cpue_spatial <- dplyr::filter(dat$cpue_spatial, year >= 2012) %>%
    plot_cpue_spatial(bin_width = 7, n_minimum_vessels = 3,
      rotation_angle = 40, xlim = c(375, 680), ylim = c(5200, 6150)) +
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
      rotation_angle = 40, xlim = c(375, 680), ylim = c(5200, 6150),
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
      verbose = FALSE)
    syn_fits$models <- NULL # save space
    saveRDS(syn_fits, file = map_cache_spp_synoptic, compress = FALSE)
  } else {
    syn_fits <- readRDS(map_cache_spp_synoptic)
  }

  # if (!file.exists(map_cache_spp_iphc)) {
  #   iphc_fits <- gfsynopsis::fit_survey_maps(dat$survey_sets,
  #     species = spp, model = "inla", density_column = "density_ppkm2",
  #     surveys = "IPHC FISS",
  #     verbose = TRUE)
  #   syn_fits$models <- NULL # save space
  #   saveRDS(iphc_fits, file = map_cache_spp_iphc, compress = FALSE)
  # } else {
  #   iphc_fits <- readRDS(map_cache_spp_iphc)
  # }
  #
  # if (!file.exists(map_cache_spp_hbll)) {
  #   hbll_fits <- gfsynopsis::fit_survey_maps(dat$survey_sets,
  #     species = spp, model = "inla", density_column = "density_ppkm2",
  #     surveys = c("HBLL OUT N", "HBLL OUT S"),
  #     verbose = TRUE)
  #   syn_fits$models <- NULL # save space
  #   saveRDS(hbll_fits, file = map_cache_spp_hbll, compress = FALSE)
  # } else {
  #   hbll_fits <- readRDS(map_cache_spp_hbll)
  # }

  g_survey_spatial <-
    gfsynopsis::plot_survey_maps(syn_fits$pred_dat, syn_fits$raw_dat) +
    coord_cart

  g_survey_spatial_syn <- g_survey_spatial + ggplot2::ggtitle("Synoptic survey biomass")

  # TODO: temp:
  g_survey_spatial_iphc <- g_survey_spatial + ggplot2::ggtitle("IPHC survey biomass")
  g_survey_spatial_hbll <- g_survey_spatial + ggplot2::ggtitle("HBLL OUT survey biomass")

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
}
