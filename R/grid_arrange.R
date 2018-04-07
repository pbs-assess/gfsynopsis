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
#' @export
#' @importFrom grDevices dev.off pdf
#'
#' @examples
#' \dontrun{
#' # TODO broken example!!
#' library("gfplot")
#' dc <- "data-cache"
#' spp <- "pacific cod"
#' dir.create(dc, showWarnings = FALSE)
#' dir.create("figs", showWarnings = FALSE)
#' cache_pbs_data(spp, path = dc)
#'
#' dat <- list()
#' dat$survey_sets     <- readRDS(file.path(dc, "pbs-surv-sets.rds"))
#' dat$survey_samples  <- readRDS(file.path(dc, "pbs-surv-samples.rds"))
#' dat$comm_samples    <- readRDS(file.path(dc, "pbs-comm-samples.rds"))
#' dat$catch           <- readRDS(file.path(dc, "pbs-catch.rds"))
#' dat$catch           <- readRDS(file.path(dc, "pbs-catch.rds"))
#' dat$cpue_spatial    <- readRDS(file.path(dc, "pbs-cpue-spatial.rds"))
#' dat$cpue_spatial_ll <- readRDS(file.path(dc, "pbs-cpue-spatial-ll.rds"))
#' dat$survey_index    <- readRDS(file.path(dc, "pbs-surv-index.rds"))
#' dat$age_precision   <- readRDS(file.path(dc, "pbs-age-precision.rds"))
#'
#' make_pages(dat, "arrowtooth flounder", output_path = "figs")
#' }

make_pages <- function(
  dat,
  spp,
  spp_file = spp,
  output_path,
  aspect = 1.35,
  width = 11.5,
  survey_cols = gg_color_hue(7L),
  debug = FALSE) {

  # Setup: -------------------------------------
  dc <- file.path("report", "data-cache")
  dat <- list()
  dat$survey_sets     <- readRDS(file.path(dc, "pbs-survey-sets.rds"))
  dat$survey_samples  <- readRDS(file.path(dc, "pbs-survey-samples.rds"))
  dat$comm_samples    <- readRDS(file.path(dc, "pbs-comm-samples.rds"))
  dat$catch           <- readRDS(file.path(dc, "pbs-catch.rds"))
  dat$cpue_index      <- readRDS(file.path(dc, "pbs-cpue-index.rds"))
  dat$cpue_spatial    <- readRDS(file.path(dc, "pbs-cpue-spatial.rds"))
  dat$cpue_spatial_ll <- readRDS(file.path(dc, "pbs-cpue-spatial-ll.rds"))
  dat$survey_index    <- readRDS(file.path(dc, "pbs-survey-index.rds"))
  dat$age_precision   <- readRDS(file.path(dc, "pbs-age-precision.rds"))

  spp <- "pacific ocean perch"
  # spp <- "yelloweye rockfish"
  spp_file <- file.path("report", "pop-eg")

  dat$survey_sets <- dplyr::filter(dat$survey_sets, species_common_name == spp)
  dat$survey_samples <- dplyr::filter(dat$survey_samples, species_common_name == spp)
  dat$comm_samples <- dplyr::filter(dat$comm_samples, species_common_name == spp)
  dat$catch <- dplyr::filter(dat$catch, species_common_name == spp)
  dat$cpue_spatial <- dplyr::filter(dat$cpue_spatial, species_common_name == spp)
  dat$cpue_spatial_ll <- dplyr::filter(dat$cpue_spatial_ll, species_common_name == spp)
  dat$survey_index <- dplyr::filter(dat$survey_index, species_common_name == spp)
  dat$age_precision <- dplyr::filter(dat$age_precision, species_code == 396)

  dat$comm_samples_no_keepers <- dplyr::filter(dat$comm_samples, keeper == FALSE)
  dat$combined_samples <- bind_samples(dat$comm_samples, dat$survey_samples)

  # saveRDS(dat, file = "report/dat-eg.rds", compress = FALSE)
  # dat <- readRDS("report/dat-eg.rds")

  # temp:
  dat$survey_index$survey_abbrev <- gsub("_", " ", dat$survey_index$survey_abbrev)
  dat$survey_index$survey_abbrev <-
    ifelse(dat$survey_index$survey_series_desc ==
        "Hecate Strait Multispecies Assemblage Bottom Trawl", "MSA HS",
      dat$survey_index$survey_abbrev)

  # temp:
  # lookup <- unique(select(dat$survey_samples, survey_abbrev, survey_series_desc))
  # dat$survey_sets$survey_abbrev <- NULL # in case
  # dat$survey_sets <- left_join(dat$survey_sets, lookup)

  output_path <- "."
  aspect = 1.35
  width = 11.5
  survey_cols = RColorBrewer::brewer.pal(7L, "Set2")
  debug = FALSE
  resolution = 300

  height <- width * aspect

  survey_cols <- stats::setNames(survey_cols,
    c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI", "HBLL OUT N",
      "HBLL OUT S", "IPHC FISS"))

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
  g_ages <- plot_ages(sb) +
    guides(fill = FALSE, colour = FALSE)

  # Length compositions: -------------------------------

  ss <- tidy_lengths_raw(dat$survey_samples, bin_size = 2.5,
    sample_type = "survey")
  sc <- tidy_lengths_raw(dat$comm_samples_no_keepers, bin_size = 2.5,
    sample_type = "commercial") %>%
    filter(year >= 2000)
  sb <- suppressWarnings(bind_rows(ss, sc))
  sb$survey_abbrev <- factor(sb$survey_abbrev,
    levels = c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI", "HBLL OUT N",
      "HBLL OUT S", "IPHC FISS", "Commercial"))
  g_lengths <- plot_lengths(sb) +
    guides(colour = FALSE, fill = FALSE)

  # Aging precision: -------------------------------

  g_age_precision <- tidy_age_precision(dat$age_precision) %>%
    plot_age_precision()

  # Commercial CPUE indices: -------------------------------

  # ind <- gfsynopsis::fit_cpue_indices(dat$cpue_index,
  #   species = unique(dat$catch$species_common_name))
  # saveRDS(ind, file = "report/cpue-eg-cache.rds", compress = FALSE)
  ind <- readRDS("report/cpue-eg-cache.rds")

  g_cpue_index <- gfsynopsis::plot_cpue_indices(ind) +
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

  # Survey biomas indices: -------------------------------

  g_survey_index <- tidy_survey_index(dat$survey_index) %>%
    plot_survey_index(col = c("grey60", "grey20")) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) + ggplot2::ggtitle("Survey relative biomass indices")

  g_comm_samples <- tidy_sample_avail(dat$comm_samples) %>%
    plot_sample_avail(title = "Commercial samples", year_range = c(1994, 2017))

  g_survey_samples <- tidy_sample_avail(dat$survey_samples) %>%
    plot_sample_avail(title = "Survey samples", year_range = c(1994, 2017))

  # Maturity by month: -------------------------------

  # TODO: should this include commercial?
  g_maturity_month <- tidy_maturity_months(dat$survey_samples) %>%
    plot_maturity_months() +
    guides(colour = FALSE, fill = FALSE)

  # Growth fits: -------------------------------

  vb_m <- fit_vb(dat$combined_samples, sex = "male", method = "mpd")
  vb_f <- fit_vb(dat$combined_samples, sex = "female", method = "mpd")
  g_vb <- plot_vb(object_female = vb_m, object_male = vb_f) +
    guides(colour = FALSE, fill = FALSE)

  lw_m <- fit_length_weight(dat$combined_samples, sex = "male", method = "rlm")
  lw_f <- fit_length_weight(dat$combined_samples, sex = "female", method = "rlm")
  g_length_weight <-
    plot_length_weight(object_female = lw_m, object_male = lw_f) +
    guides(colour = FALSE, fill = FALSE)

  # Maturity ogives: -------------------------------

  e <- dat$combined_samples %>%
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

  coord_cart <- coord_cartesian(xlim = c(360, 640), ylim = c(5275, 6155))

  checking_square <- geom_polygon(data = data.frame(x = c(400, 600, 600, 400),
    y = c(5500, 5500, 5700, 5700)), aes(x = x, y = y),
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
    )  #+ checking_square

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

  # syn_fits <- gfsynopsis::fit_survey_maps(dat$survey_sets,
  #   species = spp, model = "inla", plot = FALSE,
  #   surveys = c("SYN QCS", "SYN HS", "SYN WCHG", "SYN WCVI"),
  #     verbose = TRUE)
  # saveRDS(syn_fits, file = "report/syn-fits-eg-cache.rds", compress = FALSE)
  syn_fits <- readRDS("report/syn-fits-eg-cache.rds")

  # iphc_fits <- gfsynopsis::fit_survey_maps(dat$survey_sets,
  #   species = spp, density_column = "density_ppkm2",
  #   model = "inla", plot = TRUE,
  #   surveys = "IPHC FISS", verbose = TRUE)
  # saveRDS(syn_fits, file = "report/syn-fits-eg-cache.rds", compress = FALSE)

  g_survey_spatial <-
    gfsynopsis::plot_survey_maps(syn_fits$pred_dat, syn_fits$raw_dat) +
    coord_cart +
    ggplot2::ggtitle("Synoptic survey biomass")

  # Page 1 layout: -------------------------------

  gg_catch           <- ggplot2::ggplotGrob(g_catch)
  gg_survey_index    <- ggplot2::ggplotGrob(g_survey_index)
  gg_cpue_spatial    <- ggplot2::ggplotGrob(g_cpue_spatial)
  gg_cpue_spatial_ll <- ggplot2::ggplotGrob(g_cpue_spatial_ll)
  gg_cpue_index      <- ggplot2::ggplotGrob(g_cpue_index)
  gg_survey_spatial  <- ggplot2::ggplotGrob(g_survey_spatial)

  fg_catch           <- egg::gtable_frame(gg_catch, debug = debug)
  fg_survey_index    <- egg::gtable_frame(gg_survey_index, debug = debug)
  fg_cpue_spatial    <- egg::gtable_frame(gg_cpue_spatial, debug = debug)
  fg_cpue_spatial_ll <- egg::gtable_frame(gg_cpue_spatial_ll, debug = debug)
  fg_cpue_index      <- egg::gtable_frame(gg_cpue_index, debug = debug,
    width = grid::unit(0.7, "null"))
  fg_survey_spatial  <- egg::gtable_frame(gg_survey_spatial, debug = debug)

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
    ggplot2::ggplotGrob(g_blank + ggplot2::ggtitle(" ")),
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
      fg_survey_spatial, fg_survey_spatial, fg_survey_spatial,
      fg_cpue_spatial, fg_cpue_spatial_ll),
    width = grid::unit(1, "null"),
    height = grid::unit(1.102, "null"),
    debug = debug)

  f_all <- gridExtra::gtable_rbind(f_fake_text, f_top, f_bottom)

  pdf(file.path(output_path, paste0(spp_file, "-1.pdf")), width = width,
    height = height)
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

  f_very_top <- egg::gtable_frame(
    gridExtra::gtable_cbind(fg_comm_samples, fg_survey_samples),
    width = grid::unit(1, "null"),
    height = grid::unit(0.17, "null"),
    debug = debug)

  f_all <- gridExtra::gtable_rbind(f_very_top, f_top, f_middle, f_bottom)

  pdf(file.path(output_path, paste0(spp_file, "-2.pdf")), width = width,
    height = height)
  # png(file.path(output_path, paste0(spp_file, "-2.png")), width = width * resolution,
    # height = height * resolution, res = resolution)
  grid::grid.newpage()
  grid::grid.draw(f_all)
  dev.off()

}
