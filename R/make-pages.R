#' Make synopsis pages
#'
#' This is the main workhorse function that creates the synopsis pages.
#'
#' @param dat A data list object from [gfdata::cache_pbs_data()].
#' @param dat_iphc A data list object from [gfiphc::cache_pbs_data_iphc()].
#' @param spp A species common name.
#' @param d_geostat_index `d_geostat_index` data
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
#' @param synoptic_max_survey_years A vector of length 2 giving the maximum
#'   synoptic survey years to include.
#' @param age_comp_first_year Minimum year to be shown in the age frequency plots,
#'   such that all survey panels can be shown.
#' @param parallel Parallel CPUE index fitting?
#' @param length_ticks A data frame indicating optional length composition
#'   x-axis breaks and labels.
#' @param all_survey_years DF with `survey_abbrev` and all `year`
#' @param stitch_model_type A string indicating what type of stitch model to fit.
#'   Matching one of: "st-rw" (the default), "st-rw_tv-rw". See
#'   [gfsynopsis::get_stitched_index()].
#'
#' @return
#' This function generates 2 png files with all of the plots for a given species.
#'
#' @export
#' @importFrom grDevices dev.off pdf png
#' @importFrom ggplot2 labeller theme element_blank ggtitle
#' @importFrom grid unit
#' @importFrom rosettafish en2fr

make_pages <- function(
    dat,
    dat_iphc,
    spp,
    d_geostat_index,
    aspect = 1.35,
    short_page_height_ratio = 0.78,
    width = 11.5,
    debug = FALSE,
    resolution = 170,
    png_format = TRUE,
    spp_file = gfsynopsis:::clean_name(spp),
    report_folder = "report",
    report_lang_folder = "report",
    include_map_square = FALSE,
    map_xlim = c(360, 653),
    map_ylim = c(5275, 6155),
    save_gg_objects = FALSE,
    survey_cols = c("SYN WCHG" = "#E41A1C",
      "SYN HS" = "#377EB8",
      "SYN QCS" = "#4DAF4A",
      "SYN WCVI" = "#984EA3",
      "HBLL OUT N" = "#FF7F00",
      "HBLL OUT S" = "#FDBF6F",
      "HBLL INS N/S" = "#A65628",
      "IPHC FISS" = "#F781BF",
      "MSSM WCVI" = "#a8a8a8",
      "MSA HS" = "#a8a8a8",
      "SYN HS/QCS/WCHG/WCVI" = "#a8a8a8",
      "SYN HS/QCS/WCVI" = "#a8a8a8",
      "HBLL OUT N/S" = "#a8a8a8",
      "Commercial" = "#303030"
    ),
    mat_min_n = 20,
    survey_map_outlier = 1,
    synoptic_max_survey_years =
      list("SYN WCHG" = 2020, "SYN HS" = 2019, "SYN WCVI" = 2018, "SYN QCS" = 2019),
    hbll_out_max_survey_years = list("HBLL OUT N" = 2019, "HBLL OUT S" = 2020),
    iphc_max_survey_year = 2022,
    age_comp_first_year = NULL,
    parallel = FALSE,
    french = FALSE,
    final_year_comm = 2021,
    final_year_surv = 2022,
    length_ticks = NULL,
    all_survey_years = NULL,
    stitch_model_type = "st-rw",
    grid_dir,
    hbll_bait_counts,
    iphc_hook_counts) {
  message("\nMaking pages for species: ", spp)
  # Internal setup calculations: -----------------------------------------------
  height <- width * aspect

  dat$survey_sets <- dplyr::filter(dat$survey_sets, species_common_name == spp)
  dat$survey_samples <- dplyr::filter(dat$survey_samples, species_common_name == spp) |>
    mutate(survey_abbrev = ifelse(survey_abbrev %in% c("HBLL INS N", "HBLL INS S"), "HBLL INS N/S", survey_abbrev))
  dat$commercial_samples <- dplyr::filter(dat$commercial_samples, species_common_name == spp)
  dat$catch <- dplyr::filter(dat$catch, species_common_name == spp)

  # TODO: temp:
  dat$catch$major_stat_area_description <- NULL # in case; older version
  dat$catch$major_stat_area_name <- NULL # in case; older version
  dat$catch <- dplyr::inner_join(dat$catch, gfplot::pbs_areas, by = "major_stat_area_code")
  dat$catch <- rename(dat$catch, major_stat_area_name = major_stat_area_description)

  dat$cpue_spatial <- dplyr::filter(dat$cpue_spatial, species_common_name == spp)
  dat$cpue_spatial_ll <- dplyr::filter(dat$cpue_spatial_ll, species_common_name == spp)
  dat$survey_index <- dplyr::filter(dat$survey_index, species_common_name == spp)
  dat$age_precision <- dplyr::filter(
    dat$age_precision,
    species_code == unique(dat$survey_sets$species_code)
  )

  dat_iphc$set_counts <- dplyr::mutate(dat_iphc$set_counts, species_common_name = spp)

  # FIXME: (not needed anymore?)
  # if (nrow(dat$survey_samples) > 0L)
  #   dat$survey_samples$maturity_convention_maxvalue <- 1e6
  # if (nrow(dat$commercial_samples) > 0L)
  #   dat$commercial_samples$maturity_convention_maxvalue <- 1e6

  # FIXME: (get this fixed in the raw data)
  if (identical(spp, "rosethorn rockfish")) {
    # one over 1 meter!
    dat$survey_samples <- dplyr::filter(dat$survey_samples, length < 60)
    dat$commercial_samples <- dplyr::filter(dat$commercial_samples, length < 60)
  }
  if (identical(spp, "aleutian skate")) {
    dat$survey_samples <- dplyr::filter(dat$survey_samples, length < 500)
    dat$commercial_samples <- dplyr::filter(dat$commercial_samples, length < 500)
  }
  if (identical(spp, "sablefish")) {
    dat$survey_samples <- dplyr::filter(dat$survey_samples, length < 110)
    dat$commercial_samples <- dplyr::filter(dat$commercial_samples, length < 110)
  }
  # These ones look very off:
  if (identical(spp, "spotted ratfish")) {
    dat$survey_samples <- dplyr::filter(dat$survey_samples, !(survey_abbrev == "SYN HS" & year == 2005))
  }
  if (identical(spp, "shortspine thornyhead")) {
    dat$survey_samples <- dplyr::filter(dat$survey_samples, age < 80)
  }

  dat$commercial_samples_no_keepers <- dplyr::filter(
    dat$commercial_samples,
    sampling_desc %in% "UNSORTED"
  )

  # Create a data frame that combines the survey and commercial samples:
  common_cols <- intersect(
    colnames(dat$survey_samples),
    colnames(dat$commercial_samples)
  )
  common_cols <- c(common_cols, "survey_abbrev")
  dat$commercial_samples[["survey_abbrev"]] <-
    character(length = nrow(dat$commercial_samples))
  temp_commercial_samples <- dat$commercial_samples[, common_cols, drop = FALSE]
  temp_survey_samples <- dat$survey_samples[, common_cols, drop = FALSE]
  dat$combined_samples <- rbind(temp_commercial_samples, temp_survey_samples)

  dat$survey_index$survey_abbrev <- gsub("_", " ", dat$survey_index$survey_abbrev)
  dat$survey_index$survey_abbrev <-
    ifelse(dat$survey_index$survey_series_desc ==
      "Hecate Strait Multispecies Assemblage Bottom Trawl", "MSA HS",
    dat$survey_index$survey_abbrev
    )

  # File and folder setup: -----------------------------------------------------

  fig_folder <- file.path(report_lang_folder, "figure-pages")
  ggplot_folder <- file.path(report_lang_folder, "ggplot-objects")
  cpue_cache <- file.path(report_folder, "cpue-cache")
  survey_map_cache <- file.path(report_folder, "map-cache")
  vb_cache <- file.path(report_folder, "vb-cache")
  iphc_index_cache <- file.path(report_folder, "iphc-cache")
  stitch_cache <- file.path(report_folder, "stitch-cache")

  dir.create(fig_folder, showWarnings = FALSE, recursive = TRUE)
  dir.create(ggplot_folder, showWarnings = FALSE, recursive = TRUE)
  dir.create(cpue_cache, showWarnings = FALSE, recursive = TRUE)
  dir.create(survey_map_cache, showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(survey_map_cache, "synoptic"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(survey_map_cache, "iphc"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(survey_map_cache, "hbll"), showWarnings = FALSE, recursive = TRUE)
  dir.create(vb_cache, showWarnings = FALSE, recursive = TRUE)
  dir.create(iphc_index_cache, showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(stitch_cache, "synoptic"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(stitch_cache, "hbll_outside"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(stitch_cache, "hbll_inside"), showWarnings = FALSE, recursive = TRUE)

  gg_folder_spp <- paste0(file.path(ggplot_folder, spp_file), ".rds")
  fig_folder_spp1 <- paste0(file.path(fig_folder, spp_file), if (png_format) "-1.png" else "-1.pdf")
  fig_folder_spp2 <- paste0(file.path(fig_folder, spp_file), if (png_format) "-2.png" else "-2.pdf")
  cpue_cache_spp <- paste0(file.path(cpue_cache, spp_file), ".rds")
  map_cache_spp_synoptic <- paste0(file.path(survey_map_cache, "synoptic", spp_file), ".rds")
  map_cache_spp_iphc <- paste0(file.path(survey_map_cache, "iphc", spp_file), ".rds")
  map_cache_spp_hbll <- paste0(file.path(survey_map_cache, "hbll", spp_file), ".rds")
  vb_cache_spp <- paste0(file.path(vb_cache, spp_file), ".rds")
  iphc_index_cache_spp <- paste0(file.path(iphc_index_cache, spp_file), ".rds")
  sc_spp_synoptic <- paste0(file.path(stitch_cache, "synoptic", spp_file), "_", stitch_model_type, ".rds")
  sc_spp_hbll_out <- paste0(file.path(stitch_cache, "hbll_outside", spp_file), "_", stitch_model_type, ".rds")
  sc_spp_hbll_ins <- paste0(file.path(stitch_cache, "hbll_inside", spp_file), "_", stitch_model_type, ".rds")
  sc_spp_iphc <- paste0(file.path(stitch_cache, "iphc", spp_file), "_", stitch_model_type, ".rds")

  samp_panels <- c(
    "SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI", "HBLL OUT N",
    "HBLL OUT S", "HBLL INS N", "HBLL INS S", "IPHC FISS",
    en2fr("Commercial", french)
  )
  # Age compositions: ----------------------------------------------------------
  ss <- tidy_ages_raw(dat$survey_samples,
    sample_type = "survey", survey = c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI",
    "HBLL OUT N", "HBLL OUT S", "HBLL INS N", "HBLL INS S", "IPHC FISS")
  ) |>
    dplyr::filter(year >= age_comp_first_year)
  sc <- tidy_ages_raw(dat$commercial_samples_no_keepers,
    sample_type = "commercial"
  ) |>
    dplyr::filter(year >= age_comp_first_year)

  if (all(!is.na(sc[[1]]))) sc <- sc %>% dplyr::filter(year >= 2003)
  if (is.data.frame(sc)) {
    if (nrow(sc) == 0) {
      sc <- NA
    }
  }

  if (all(!is.na(ss[[1]])) && all(!is.na(sc[[1]]))) {
    sb <- suppressWarnings(bind_rows(ss, sc))
  }
  if (all(!is.na(ss[[1]])) && all(is.na(sc[[1]]))) {
    sb <- ss
  }
  if (all(is.na(ss[[1]])) && all(!is.na(sc[[1]]))) {
    sb <- sc
  }
  if (all(is.na(ss[[1]])) && all(is.na(sc[[1]]))) {
    sb <- NA
  }
  if (all(!is.na(sb))) {
    sb$survey_abbrev <- factor(sb$survey_abbrev,
      levels = samp_panels
    )
    g_ages <- plot_ages(sb, survey_cols = survey_cols, year_range = c(age_comp_first_year, final_year_surv), french = french) +
      guides(fill = "none", colour = "none") +
      ggtitle(en2fr("Age frequencies", french)) +
      labs(y = en2fr("Age (years)", french))
  } else {
    g_ages <- plot_ages(
      expand.grid(
        survey_abbrev = factor(x = samp_panels, levels = samp_panels),
        year = seq(age_comp_first_year, final_year_surv, 2),
        max_size = 8,
        sex = NA, age = 0, proportion = 0, total = 1, stringsAsFactors = FALSE
      ),
      year_range = c(age_comp_first_year, final_year_surv)
    ) +
      guides(fill = "none", colour = "none") +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
      ggtitle(en2fr("Age frequencies", french)) +
      labs(y = en2fr("Age (years)", french))
  }

  # Length compositions: -------------------------------------------------------

  length_samples_survey <- dplyr::filter(
    dat$survey_samples,
    !length %in% find_length_outliers(dat$survey_samples$length)
  )
  length_samples_commercial <- dplyr::filter(
    dat$commercial_samples_no_keepers,
    !length %in% find_length_outliers(dat$commercial_samples_no_keepers$length)
  )

  bin_width1 <- diff(quantile(length_samples_survey$length,
    na.rm = TRUE,
    probs = c(0, 1)
  )) / 20
  bin_width2 <- diff(quantile(length_samples_commercial$length,
    na.rm = TRUE, probs = c(0, 1)
  )) / 20
  bin_width <- mean(c(bin_width1, bin_width2), na.rm = TRUE)

  ss <- tidy_lengths_raw(length_samples_survey,
    bin_size = bin_width,
    sample_type = "survey"
  )
  sc <- length_samples_commercial %>%
    mutate(sex = 2) %>% # fake all sex as female for commercial samples; often not sexed
    tidy_lengths_raw(
      bin_size = bin_width,
      sample_type = "commercial", spp_cat_code = 1
    )

  if (all(!is.na(sc[[1]]))) sc <- sc %>% dplyr::filter(year >= 2003)
  if (is.data.frame(sc)) {
    if (nrow(sc) == 0) {
      sc <- NA
    }
  }

  if (all(!is.na(ss[[1]])) && all(!is.na(sc[[1]]))) {
    sb <- suppressWarnings(bind_rows(ss, sc))
  }
  if (all(!is.na(ss[[1]])) && all(is.na(sc[[1]]))) {
    sb <- ss
  }
  if (all(is.na(ss[[1]])) && all(!is.na(sc[[1]]))) {
    sb <- sc
  }
  if (all(is.na(ss[[1]])) && all(is.na(sc[[1]]))) {
    sb <- NA
  }

  min_total <- 20
  if (all(!is.na(sb)) && max(sb$total) >= min_total) {
    sb$survey_abbrev <- factor(sb$survey_abbrev,
      levels = c(
        "SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI", "HBLL OUT N",
        "HBLL OUT S", "IPHC FISS", en2fr("Commercial", french)
      )
    )
    sb$year <- factor(sb$year, levels = seq(2003, final_year_surv))

    suppressMessages({
      x_breaks <- if (!is.na(length_ticks$breaks)) {
        eval(parse(text = length_ticks$breaks))
      } else {
        ggplot2::waiver()
      }
      x_labels <- if (!is.na(length_ticks$labels)) {
        eval(parse(text = length_ticks$labels))
      } else {
        ggplot2::waiver()
      }
      g_lengths <- plot_lengths(sb,
        survey_cols = survey_cols,
        bin_size = bin_width, min_total = min_total, french = french
      ) +
        guides(colour = "none", fill = "none") +
        ggtitle(en2fr("Length frequencies", french)) +
        ggplot2::xlab(paste(en2fr("Length", french), "(cm)")) +
        ggplot2::ylab(en2fr("Relative length frequency", french)) +
        scale_x_continuous(breaks = x_breaks, labels = x_labels)
    })
  } else {
    g_lengths <- ggplot() +
      theme_pbs() +
      ggtitle(en2fr("Length frequencies", french)) +
      ggplot2::xlab(paste(en2fr("Length", french), "(cm)")) +
      ggplot2::ylab(en2fr("Relative length frequency", french))
  }

  # Aging precision: -----------------------------------------------------------

  if (nrow(dat$age_precision) > 0) {
    g_age_precision <- tidy_age_precision(dat$age_precision) %>%
      plot_age_precision()
  } else {
    g_age_precision <- ggplot() +
      theme_pbs()
  }

  # Commercial CPUE indices: ---------------------------------------------------

  all_not_NA <- function(x) {
    all(!is.na(x[[1L]]))
  }
  all_NA <- function(x) {
    all(is.na(x[[1L]]))
  }

  if ("cpue_index" %in% names(dat)) {
    if (nrow(dat$catch) > 0) {
      if (!file.exists(cpue_cache_spp)) {
        cpue_index <- gfsynopsis::fit_cpue_indices(dat$cpue_index,
          species = unique(dat$catch$species_common_name),
          save_model = save_gg_objects, parallel = parallel
        )
        saveRDS(cpue_index, file = cpue_cache_spp, compress = FALSE)
      } else {
        cpue_index <- readRDS(cpue_cache_spp)
      }

      if (all_not_NA(cpue_index)) { # enough vessels?

        g_cpue_index <- gfsynopsis::plot_cpue_indices(cpue_index, xlim = c(1996, final_year_comm)) +
          ggplot2::ggtitle(en2fr("Commercial bottom trawl CPUE", french)) +
          ylab("") + xlab("") +
          ggplot2::theme(
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
          )
      }
    }
  } else {
    cpue_index <- NA
  }
  if (nrow(dat$catch) == 0 || all_NA(cpue_index)) {
    g_cpue_index <-
      gfsynopsis::plot_cpue_indices(
        expand.grid(
          area = factor(c("3CD5ABCDE", "5CDE", "5AB", "3CD"),
            levels = c("3CD5ABCDE", "5CDE", "5AB", "3CD")
          ), year = 2000,
          est = NA, lwr = NA, upr = NA
        ),
        blank_plot = TRUE, xlim = c(1996, final_year_comm)
      ) +
      ggplot2::ggtitle(en2fr("Commercial bottom trawl CPUE", french)) +
      ylab("") + xlab("") +
      ggplot2::theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  }

  # Commercial catch: ----------------------------------------------------------
  if (nrow(dat$catch) > 0) {
    g_catch <- gfsynopsis::plot_catches(dat$catch, french = french, xlim = c(1955, final_year_comm))
  } else {
    g_catch <- ggplot() +
      theme_pbs()
    g_catch <- gfsynopsis::plot_catches(expand.grid(
      year = 999,
      area = factor(c("Coastwide", "5AB", "5CDE", "3CD"),
        levels = c("Coastwide", "5AB", "5CDE", "3CD")
      ),
      gear = "abc", value = 1, stringsAsFactors = FALSE
    ), blank_plot = TRUE) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }
  g_catch <- g_catch + ggplot2::theme(legend.key.width = grid::unit(0.7, "line")) +
    ggtitle(en2fr("Commercial catch", french))

  # Survey biomass indices: ----------------------------------------------------

  # Get new IPHC calculations
  dat_tidy_survey_index <- tidy_survey_index(dat$survey_index)

  if ("OTHER HS MSA" %in% levels(dat_tidy_survey_index$survey_abbrev)) {
    dat_tidy_survey_index$survey_abbrev <-
      forcats::fct_recode(dat_tidy_survey_index$survey_abbrev, `MSA HS` = "OTHER HS MSA")
  }

  lvls <- levels(dat_tidy_survey_index$survey_abbrev)

  # Make NAs in middle of time series into 0s:
  if (!is.null(all_survey_years)) {
    all_survey_years <- dplyr::filter(all_survey_years, survey_abbrev %in% unique(dat_tidy_survey_index$survey_abbrev))
    dat_tidy_survey_index <- dplyr::full_join(all_survey_years, dat_tidy_survey_index,
      by = c("survey_abbrev", "year")
    )
    if (!all(is.na(dat_tidy_survey_index$biomass))) {
      dat_tidy_survey_index$lowerci[is.na(dat_tidy_survey_index$biomass)] <- 0
      dat_tidy_survey_index$upperci[is.na(dat_tidy_survey_index$biomass)] <- 0
      dat_tidy_survey_index$biomass[is.na(dat_tidy_survey_index$biomass)] <- 0
      dat_tidy_survey_index$survey_abbrev <- factor(dat_tidy_survey_index$survey_abbrev, levels = lvls)
    }
  }

  if (!is.null(dat_iphc)) {
    if (!file.exists(iphc_index_cache_spp)) {
      iphc_set_counts_sp <- tryCatch(
        {
          gfiphc::calc_iphc_full_res(dat_iphc$set_counts)
        },
        error = function(e) NA
      )
      saveRDS(iphc_set_counts_sp, file = iphc_index_cache_spp, compress = FALSE)
    } else {
      iphc_set_counts_sp <- readRDS(iphc_index_cache_spp)
    }

    iphc_set_counts_sp_format <- tryCatch(
      {
        gfiphc::format_iphc_longest(iphc_set_counts_sp)
      },
      error = function(e) NA
    )

    if (!identical(iphc_set_counts_sp_format, NA)) {
      if (all(is.na(iphc_set_counts_sp_format$biomass))) {
        iphc_set_counts_sp_format <- NA
      }
    }

    if (!identical(iphc_set_counts_sp_format, NA)) {
      all_iphc_yrs <- data.frame(year = sort(unique(dat_iphc$set_counts$year)))
      iphc_set_counts_sp_format <- dplyr::left_join(
        all_iphc_yrs, iphc_set_counts_sp_format,
        by = "year"
      )
      iphc_set_counts_sp_format$lowerci[is.na(iphc_set_counts_sp_format$biomass)] <- 0
      iphc_set_counts_sp_format$upperci[is.na(iphc_set_counts_sp_format$biomass)] <- 0
      iphc_set_counts_sp_format$biomass[is.na(iphc_set_counts_sp_format$biomass)] <- 0
      iphc_set_counts_sp_format$survey_abbrev <- "IPHC FISS"
    }

    # Remove existing (GFbio) based IPHC series with longer ones from new calcs
    if (!identical(iphc_set_counts_sp_format, NA)) {
      dat_tidy_survey_index <- dat_tidy_survey_index %>%
        filter(survey_abbrev != "IPHC FISS") %>%
        rbind(iphc_set_counts_sp_format)
    }
  }

  # Add stitched index: ----------------------------------------------------------
  # Generate stitched index if not already cached
  if (any(!file.exists(c(
    sc_spp_synoptic, sc_spp_hbll_out,
    sc_spp_hbll_ins
  )))) {
    stitch_dat <- prep_stitch_dat(survey_dat = dat$survey_sets, bait_counts = hbll_bait_counts)
  }

  stitched_syn <- get_stitched_index(
    survey_dat = stitch_dat, species = spp,
    survey_type = "synoptic", model_type = stitch_model_type,
    grid_dir = grid_dir,
    cache = file.path(stitch_cache, "synoptic"),
    check_cache = TRUE
  )

  stitched_out <- get_stitched_index(
    survey_dat = stitch_dat, species = spp,
    survey_type = "hbll_outside", model_type = stitch_model_type,
    family = sdmTMB::nbinom2(link = "log"), grid_dir = grid_dir,
    cache = file.path(stitch_cache, "hbll_outside"),
    check_cache = TRUE
  )

  stitched_ins <- get_stitched_index(
    survey_dat = stitch_dat, species = spp,
    survey_type = "hbll_inside", model_type = stitch_model_type,
    family = sdmTMB::nbinom2(link = "log"),
    cache = file.path(stitch_cache, "hbll_inside"),
    check_cache = TRUE
  )

  if (length(stitched_syn) > 1) {
    stitched_syn <- stitched_syn |>
      mutate(
        survey_abbrev = gsub("SYN ", "", survey_abbrev),
        survey_abbrev = gsub(", ", "/", survey_abbrev)
      ) |>
      mutate(survey_abbrev = paste0("SYN ", survey_abbrev)) |>
      select(survey_abbrev, year, biomass, lowerci, upperci, mean_cv, num_sets, num_pos_sets)
  } else {
    stitched_syn <- data.frame(
      survey_abbrev = "SYN", year = NA, biomass = NA,
      lowerci = NA, upperci = NA, mean_cv = NA, num_sets = NA, num_pos_sets = NA
    )
  }

  if (length(stitched_out) > 1) {
    stitched_out <- stitched_out |>
      mutate(survey_abbrev = "HBLL OUT N/S") |>
      select(survey_abbrev, year, biomass, lowerci, upperci, mean_cv, num_sets, num_pos_sets)
  } else {
    stitched_out <- data.frame(
      survey_abbrev = "HBLL OUT N/S", year = NA, biomass = NA,
      lowerci = NA, upperci = NA, mean_cv = NA, num_sets = NA, num_pos_sets = NA
    )
  }

  if (length(stitched_ins) > 1) {
    stitched_ins <- stitched_ins |>
      mutate(survey_abbrev = "HBLL INS N/S") |>
      select(survey_abbrev, year, biomass, lowerci, upperci, mean_cv, num_sets, num_pos_sets)
  } else {
    stitched_ins <- data.frame(
      survey_abbrev = "HBLL INS N/S", year = NA, biomass = NA,
      lowerci = NA, upperci = NA, mean_cv = NA, num_sets = NA, num_pos_sets = NA
    )
  }

  # IPHC stitching
  # Use 2017 grid for predictions (can be changed)
  iphc_grid <- iphc_hook_counts |>
    filter(year == 2017) |>
    select(year, station, lon, lat) |>
    sdmTMB::add_utm_columns(ll_names = c("lon", "lat"))

  if (!file.exists(sc_spp_iphc)) {
    iphc_stitch_dat <- prep_iphc_stitch_dat(
      survey_dat = dat_iphc$set_counts,
      hook_dat = iphc_hook_counts
    )
  }

  stitched_iphc <- get_iphc_stitched_index(
    survey_dat = iphc_stitch_dat,
    species = spp,
    form = "catch ~ 1",
    family = sdmTMB::nbinom2(link = "log"),
    time = "year",
    spatial = "on",
    spatiotemporal = "rw",
    model_type = stitch_model_type,
    offset = "offset",
    gradient_thresh = 0.001,
    cutoff = 20,
    grid = iphc_grid, silent = FALSE,
    cache = file.path(stitch_cache, "iphc"),
    check_cache = TRUE
  )

  if (length(stitched_iphc) > 1) {
    stitched_iphc <- stitched_iphc |>
      mutate(survey_abbrev = "IPHC Geostat") |>
      select(survey_abbrev, year, biomass, lowerci, upperci, mean_cv, num_sets, num_pos_sets)
  } else {
    stitched_iphc <- data.frame(
      survey_abbrev = "IPHC Geostat", year = NA, biomass = NA,
      lowerci = NA, upperci = NA, mean_cv = NA, num_sets = NA, num_pos_sets = NA
    )
  }

  stitched_df <- bind_rows(stitched_syn, stitched_out, stitched_ins, stitched_iphc)
  stitched_lvls <- unique(stitched_df$survey_abbrev)
  stitched_df <- stitched_df |>
    mutate(survey_abbrev = factor(survey_abbrev, levels = stitched_lvls))

  dat_tidy_survey_index <- bind_rows(dat_tidy_survey_index, stitched_df) |>
    filter(!survey_abbrev %in% c("HBLL INS N", "HBLL INS S"))

  if (all(is.na(dat_tidy_survey_index$biomass))) {
    g_survey_index <- ggplot() +
      theme_pbs()
  } else {
    dat_tidy_survey_index <- dplyr::filter(
      dat_tidy_survey_index,
      !(survey_abbrev == "HBLL INS S" & year == 2009 & !is.na(year))
    ) # only half survey conducted

    suppressMessages({
      g_survey_index <- plot_survey_index(dat_tidy_survey_index,
        col = c("grey60", "grey20"), survey_cols = survey_cols,
        xlim = c(1984 - 0.2, final_year_surv + 0.2), french = french
      ) +
        scale_x_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE))
    })
  }

  if (!is.null(d_geostat_index)) {
    # Get geostatistical index calculations
    d_geostat_index <- d_geostat_index %>%
      rename(
        survey_abbrev = survey, biomass_scaled = est,
        lowerci_scaled = lwr, upperci_scaled = upr
      ) %>%
      filter(type == "Spatiotemporal") %>%
      filter(species == spp_file) %>%
      group_by(survey_abbrev) %>%
      mutate(st_geo_mean = exp(mean(log(biomass_scaled), na.rm = TRUE))) %>%
      ungroup() %>%
      mutate(
        survey_abbrev =
          factor(survey_abbrev,
            levels = levels(g_survey_index$data$survey_abbrev)
          )
      )

    # Add the geostatistical index calculations to the existing data and plot
    if (nrow(d_geostat_index) > 0L) {
      design_index_geo_means <- group_by(g_survey_index$data, survey_abbrev) %>%
        summarise(design_geo_mean = exp(mean(log(biomass_scaled), na.rm = TRUE)))
      d_geostat_index <-
        suppressWarnings(left_join(d_geostat_index, design_index_geo_means,
          by = "survey_abbrev"
        ))
      d_geostat_index <- group_by(d_geostat_index, survey_abbrev) %>%
        mutate(
          lowerci_scaled = lowerci_scaled * (design_geo_mean / st_geo_mean),
          upperci_scaled = upperci_scaled * (design_geo_mean / st_geo_mean),
          biomass_scaled = biomass_scaled * (design_geo_mean / st_geo_mean)
        )

      g_survey_index <- g_survey_index +
        ggplot2::geom_line(
          data = d_geostat_index, lty = 1, size = 0.85,
          colour = "#00000050"
        ) +
        ggplot2::geom_point(
          data = d_geostat_index, stroke = 0.8, size = 1.05,
          pch = 21, fill = "grey70",
          colour = "grey45"
        ) +
        ggplot2::geom_ribbon(
          data = d_geostat_index,
          ggplot2::aes_string(ymin = "lowerci_scaled", ymax = "upperci_scaled"),
          fill = NA, lty = "12", size = 0.35, colour = "grey40"
        )
      g_survey_index <- suppressMessages({
        g_survey_index + coord_cartesian(
          ylim = c(-0.005, 1.03),
          xlim = c(1984, final_year_surv) + c(-0.5, 0.5), expand = FALSE
        )
      })
    }
  }

  g_survey_index <- g_survey_index +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) + ggplot2::ggtitle(en2fr("Survey relative biomass indices", french))

  # Specimen numbers: ----------------------------------------------------------

  temp <- tidy_sample_avail(dat$commercial_samples, year_range = c(1996, final_year_surv))
  # FIXME: na_colour always white!?
  na_colour <- if (all(is.na(temp$n_plot))) "transparent" else "grey75"
  g_comm_samples <- plot_sample_avail(temp,
    title = en2fr("Commercial samples", french), year_range = c(1996, final_year_surv),
    french = french
  ) +
    ggplot2::ggtitle(en2fr("Commercial specimen counts", french))
  suppressMessages({
    g_comm_samples <- g_comm_samples +
      viridis::scale_fill_viridis(option = "D", end = 0.82, na.value = na_colour)
  })
  temp <- tidy_sample_avail(dat$survey_samples, year_range = c(1996, final_year_surv))
  na_colour <- if (all(is.na(temp$n_plot))) "transparent" else "grey75"
  g_survey_samples <- plot_sample_avail(temp,
    title = en2fr("Survey samples", french), year_range = c(1996, final_year_surv),
    french = french
  ) +
    ggplot2::ggtitle(en2fr("Survey specimen counts", french))
  suppressMessages({
    g_survey_samples <- g_survey_samples +
      viridis::scale_fill_viridis(option = "C", end = 0.82, na.value = na_colour)
  })

  # Maturity by month: ---------------------------------------------------------
  dat_tidy_maturity_months <- tidy_maturity_months(dat$combined_samples, french = french)
  if (nrow(dplyr::filter(dat_tidy_maturity_months, !is.na(maturity))) == 0L) {
    g_maturity_month <- ggplot() +
      theme_pbs() +
      ggtitle(en2fr("Maturity frequencies", french))
  } else {
    g_maturity_month <- dat_tidy_maturity_months %>%
      plot_maturity_months(min_fish = 0, french = french) +
      guides(colour = "none", fill = "none") +
      ggtitle(en2fr("Maturity frequencies", french))
  }

  # Growth fits: ---------------------------------------------------------------

  if (nrow(dat$survey_samples) >= 10) {
    check_convergence_tmb <- if (identical(spp, "shortspine thornyhead")) FALSE else TRUE

    tmb_init <- list(k = 0.5, linf = 40, log_sigma = log(0.1), t0 = -1)

    if (spp == "yelloweye rockfish") {
      tmb_init <- list(k = 0.9, linf = 55, log_sigma = log(0.1), t0 = -1)
    }

    if (spp == "sablefish") {
      tmb_init <- list(k = 0.12, linf = 70, log_sigma = log(0.1), t0 = -8)
    }
    # https://spo.nmfs.noaa.gov/content/estimating-von-bertalanffy-growth-parameters-sablefish-anoplopoma-fimbria-and-pacific-cod

    if (spp == "shortspine thornyhead") {
      tmb_init <- list(k = 0.03, linf = 47, log_sigma = log(0.1), t0 = -8.5)
    }
    # https://waves-vagues.dfo-mpo.gc.ca/Library/40603039.pdf

    sink(tempfile())
    vb_m <- fit_vb(dat$survey_samples,
      sex = "male", method = "tmb",
      too_high_quantile = 1, check_convergence_tmb = check_convergence_tmb,
      tmb_init = tmb_init
    )
    vb_f <- fit_vb(dat$survey_samples,
      sex = "female", method = "tmb",
      too_high_quantile = 1, check_convergence_tmb = check_convergence_tmb,
      tmb_init = tmb_init
    )
    sink()
    vb <- list()
    vb$m <- vb_m
    vb$f <- vb_f

    # FIXME: these look way off; omitting them for now
    if (identical(spp, "shortspine thornyhead")) {
      vb$f$predictions$age <- NA
      vb$f$predictions$length <- NA
      vb$f$pars <- list(k = NA, linf = NA, t0 = NA)
    }

    g_vb <- plot_vb(object_female = vb$f, object_male = vb$m, french = french) +
      guides(colour = "none", fill = "none", lty = "none") +
      ggtitle(en2fr("Growth", french)) +
      xlab(en2fr("Age (years)", french)) + ylab(paste0(en2fr("Length", french), " (cm)"))

    sink(tempfile())
    lw_m <- fit_length_weight(dat$survey_samples,
      sex = "male", method = "tmb",
      too_high_quantile = 1
    )
    lw_f <- fit_length_weight(dat$survey_samples,
      sex = "female", method = "tmb",
      too_high_quantile = 1
    )
    sink()

    g_length_weight <-
      plot_length_weight(object_female = lw_f, object_male = lw_m, french = french) +
      ggplot2::theme(
        legend.position = c(0.9, 0.2),
        legend.key.width = grid::unit(1.8, units = "char")
      ) +
      ggplot2::guides(
        lty =
          guide_legend(override.aes = list(lty = c(1, 2), lwd = c(.7, .7)))
      ) +
      xlab(paste0(en2fr("Length", french), " (cm)")) + ylab(paste0(en2fr("Weight", french), " (kg)")) +
      ggtitle(en2fr("Length-weight relationship", french))
  } else {
    g_vb <- ggplot2::ggplot() +
      theme_pbs() +
      xlab(en2fr("Age (years)", french)) +
      ylab(paste0(en2fr("Length", french), " (cm)")) +
      ggtitle(en2fr("Growth", french))
    g_length_weight <- ggplot2::ggplot() +
      theme_pbs() +
      xlab(paste0(en2fr("Length", french), " (cm)")) +
      ylab(paste0(en2fr("Weight", french), " (kg)")) +
      ggtitle(en2fr("Length-weight relationship", french))
  }

  # Maturity ogives: -----------------------------------------------------------

  if (sum(!is.na(dat$survey_samples$maturity_code)) > 10) {
    mat_age <- dat$survey_samples %>%
      fit_mat_ogive(
        type = "age",
        months = seq(1, 12)
      )
  } else {
    mat_age <- NA
  }

  type <- "none"

  if (length(mat_age) > 1L) {
    sample_size <- mat_age$data %>%
      group_by(female, mature) %>%
      summarise(N = n()) %>%
      group_by(female) %>%
      summarise(N_min = min(N, na.rm = TRUE))

    sample_size <- reshape::melt(table(mat_age$data$mature, mat_age$data$female))
    names(sample_size) <- c("mature", "female", "N")
    sample_size <- sample_size %>%
      group_by(female) %>%
      summarise(N_min = min(N, na.rm = TRUE))

    # if prob. mature looks wrong, fake a low sample size to not plot it:
    prob_mat <- mat_age$pred_data %>%
      select(age_or_length, female, glmm_fe) %>%
      unique() %>%
      group_by(female) %>%
      filter(age_or_length < quantile(age_or_length, probs = 0.1)) %>%
      summarise(mean_mat = mean(glmm_fe, na.rm = TRUE))

    sample_size <- left_join(sample_size, prob_mat, by = "female") %>%
      mutate(N_min = ifelse(mean_mat > 0.5, 0, N_min))

    if (nrow(sample_size) > 1L && length(unique(sample_size$female)) > 1L && nrow(prob_mat) >= 1L) {
      if (sample_size[sample_size$female == 0, "N_min", drop = TRUE] < mat_min_n &&
        sample_size[sample_size$female == 1, "N_min", drop = TRUE] < mat_min_n) {
        type <- "none"
      }
      if (sample_size[sample_size$female == 0, "N_min", drop = TRUE] >= mat_min_n &&
        sample_size[sample_size$female == 1, "N_min", drop = TRUE] >= mat_min_n) {
        type <- "all"
      }
      if (sample_size[sample_size$female == 0, "N_min", drop = TRUE] >= mat_min_n &&
        sample_size[sample_size$female == 1, "N_min", drop = TRUE] < mat_min_n) {
        type <- "male"
      }
      if (sample_size[sample_size$female == 0, "N_min", drop = TRUE] < mat_min_n &&
        sample_size[sample_size$female == 1, "N_min", drop = TRUE] >= mat_min_n) {
        type <- "female"
      }
    }
  }

  if (length(mat_age) > 1L && type != "none") {
    g_mat_age <- gfplot::plot_mat_ogive(mat_age, prediction_type = type, french = french) +
      guides(colour = "none", fill = "none", lty = "none") +
      ggplot2::guides(lty = "none", colour = "none") +
      ggtitle(en2fr("Age at maturity", french)) +
      ggplot2::labs(x = en2fr("Age (years)", french), y = en2fr("Probability mature", french), colour = en2fr("Sex", french), lty = en2fr("Sex", french))
  } else {
    g_mat_age <- ggplot() +
      theme_pbs() +
      ggtitle(en2fr("Age at maturity", french)) +
      ggplot2::labs(x = en2fr("Age (years)", french), y = en2fr("Probability mature", french)) +
      ggplot2::guides(lty = "none", colour = "none")
  }

  if (sum(!is.na(dat$survey_samples$maturity_code)) > 10) {
    mat_length <- dat$survey_samples %>%
      fit_mat_ogive(
        type = "length",
        months = 1:12
      )
  } else {
    mat_length <- NA
  }

  type <- "none"
  if (length(mat_length) > 1L) {
    sample_size <- reshape::melt(table(mat_length$data$mature, mat_length$data$female))
    names(sample_size) <- c("mature", "female", "N")
    sample_size <- sample_size %>%
      group_by(female) %>%
      summarise(N_min = min(N, na.rm = TRUE))

    if (nrow(sample_size) > 1L && length(unique(sample_size$female)) > 1L) {
      if (sample_size[sample_size$female == 0, "N_min", drop = TRUE] < mat_min_n &&
        sample_size[sample_size$female == 1, "N_min", drop = TRUE] < mat_min_n) {
        type <- "none"
      }
      if (sample_size[sample_size$female == 0, "N_min", drop = TRUE] >= mat_min_n &&
        sample_size[sample_size$female == 1, "N_min", drop = TRUE] >= mat_min_n) {
        type <- "all"
      }
      if (sample_size[sample_size$female == 0, "N_min", drop = TRUE] >= mat_min_n &&
        sample_size[sample_size$female == 1, "N_min", drop = TRUE] < mat_min_n) {
        type <- "male"
      }
      if (sample_size[sample_size$female == 0, "N_min", drop = TRUE] < mat_min_n &&
        sample_size[sample_size$female == 1, "N_min", drop = TRUE] >= mat_min_n) {
        type <- "female"
      }
    }
  }
  if (spp == "spotted ratfish") type <- "none" # FIXME almost none; way off
  if (spp == "tope shark") type <- "none" # FIXME almost none; way off
  if (spp == "stripetail rockfish") type <- "none" # FIXME almost none; way off
  if (spp == "curlfin sole") type <- "none" # FIXME almost none; way off

  if (length(mat_length) > 1L && type != "none") {
    g_mat_length <- gfplot::plot_mat_ogive(mat_length, prediction_type = type, french = french) +
      ggplot2::theme(
        legend.position = c(0.9, 0.2),
        legend.key.width = grid::unit(1.8, units = "char")
      ) +
      ggplot2::guides(
        lty =
          guide_legend(override.aes = list(lty = c(1, 2), lwd = c(.7, .7)))
      ) +
      ggtitle(en2fr("Length at maturity", french)) +
      ggplot2::labs(x = paste0(en2fr("Length", french), " (cm)"), y = paste0(en2fr("Probability mature", french)))
  } else {
    g_mat_length <- ggplot() +
      theme_pbs() +
      ggtitle(en2fr("Length at maturity", french)) +
      ggplot2::labs(x = paste0(en2fr("Length", french), " (cm)"), y = paste0(en2fr("Probability mature", french))) +
      ggplot2::guides(lty = "none", colour = "none")
  }
  # Commercial CPUE maps -------------------------------------------------------

  coord_cart <- coord_cartesian(xlim = map_xlim, ylim = map_ylim)

  # for checking if aspect ratio of map is 1:1
  checking_square <- geom_polygon(
    data = data.frame(
      x = c(400, 600, 600, 400),
      y = c(5500, 5500, 5700, 5700)
    ), aes_string(x = "x", y = "y"),
    inherit.aes = FALSE, fill = "grey50", lwd = 1, col = "black"
  )

  g_cpue_spatial <- dat$cpue_spatial %>%
    plot_cpue_spatial(
      bin_width = 7, n_minimum_vessels = 3,
      rotation_angle = 40, xlim = map_xlim, ylim = map_ylim,
      show_historical = TRUE, start_year = 2013,
      fill_scale = ggplot2::scale_fill_viridis_c(trans = "fourth_root_power", option = "D"),
      colour_scale = ggplot2::scale_colour_viridis_c(trans = "fourth_root_power", option = "D"),
      percent_excluded_xy = if (!french) c(0.015, -0.02) else c(0.08, -0.02),
      percent_excluded_text = if (!french) "Fishing events excluded due to Privacy Act" else "Activités de pêche exclues en raison de la Loi sur la protection des\nrenseignements personnel"
    ) +
    ggplot2::ggtitle(en2fr("Commercial trawl CPUE", french)) +
    theme(legend.position = "none") +
    ggplot2::annotate("text", 360, 6172,
      label = paste0("2013–", final_year_comm), col = "grey30",
      hjust = 0
    )
  suppressMessages({
    g_cpue_spatial <- g_cpue_spatial +
      coord_cart + theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
  })
  # g_cpue_spatial <- ggplot() + theme_pbs()

  if (include_map_square) {
    g_cpue_spatial <- g_cpue_spatial + checking_square
  }

  suppressMessages({
    g_cpue_spatial_ll <- dat$cpue_spatial_ll %>%
      plot_cpue_spatial(
        bin_width = 7, n_minimum_vessels = 3,
        rotation_angle = 40, xlim = map_xlim, ylim = map_ylim,
        start_year = 2008,
        fill_scale = ggplot2::scale_fill_viridis_c(trans = "fourth_root_power", option = "D"),
        colour_scale = ggplot2::scale_colour_viridis_c(trans = "fourth_root_power", option = "D"),
        fill_lab = "CPUE (kg/fe)",
        percent_excluded_xy = if (!french) c(0.015, -0.02) else c(0.08, -0.02),
        percent_excluded_text = if (!french) "Fishing events excluded due to Privacy Act" else "Activités de pêche exclues en raison de la Loi sur la protection des\nrenseignements personnel"
      ) +
      ggplot2::ggtitle(en2fr("Commercial H & L CPUE", french)) +
      theme(legend.position = "none") +
      coord_cart + theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      ) +
      ggplot2::annotate("text", 360, 6172,
        label = paste0("2008–", final_year_comm), col = "grey30",
        hjust = 0
      )
  })
  # g_cpue_spatial_ll <- ggplot() + theme_pbs()

  # Survey maps: ---------------------------------------------------------------
  if (!file.exists(map_cache_spp_synoptic)) {
    # Multiply by 1000 for computational reasons.
    # Otherwise the numbers are too small sometimes:
    dat$survey_sets$density_kgpm2 <- dat$survey_sets$density_kgpm2 * 1000
    surv <- c("SYN QCS", "SYN HS", "SYN WCHG", "SYN WCVI")
    syn_fits <- gfsynopsis::fit_survey_maps(dat$survey_sets,
      surveys = surv, species = spp,
      silent = TRUE, years = seq(2002, max(unlist(synoptic_max_survey_years)))
    )
    saveRDS(syn_fits, map_cache_spp_synoptic)
    # file = file.path("report", "map-cache", "synoptic",
    #   spp_file, gsub(" ", "-", surv[i]), ".rds"), compress = FALSE)
  } else {
    syn_fits <- readRDS(map_cache_spp_synoptic)
  }

  # if (!file.exists(map_cache_spp_iphc)) {
  #   iphc_fits <- gfsynopsis::fit_survey_maps(dat$survey_sets,
  #     species = spp,
  #     surveys = "IPHC FISS",
  #     silent = TRUE, years = 2020)
  #   iphc_fits$models <- NULL # save space
  #   saveRDS(iphc_fits, file = map_cache_spp_iphc, compress = FALSE)
  # } else {
  #   iphc_fits <- readRDS(map_cache_spp_iphc)
  # }

  if (!file.exists(map_cache_spp_hbll)) {
    hbll_fits <- gfsynopsis::fit_survey_maps(dat$survey_sets,
      species = spp,
      surveys = c("HBLL OUT N", "HBLL OUT S"),
      silent = TRUE, years = unlist(hbll_out_max_survey_years)
    )
    saveRDS(hbll_fits, file = map_cache_spp_hbll, compress = FALSE)
  } else {
    hbll_fits <- readRDS(map_cache_spp_hbll)
  }

  # squash massive outlying cells for colour scale goodness:
  if ("combined" %in% names(syn_fits$pred_dat)) {
    if (!is.null(syn_fits$pred_dat$combined)) {
      qs <- quantile(syn_fits$pred_dat$combined, probs = survey_map_outlier, na.rm = TRUE)[[1]]
      syn_fits$pred_dat$combined[syn_fits$pred_dat$combined > qs] <- qs
    }
  }
  if ("combined" %in% names(hbll_fits$pred_dat)) {
    if (!is.null(hbll_fits$pred_dat$combined)) {
      qs <- quantile(hbll_fits$pred_dat$combined, probs = survey_map_outlier, na.rm = TRUE)[[1]]
      hbll_fits$pred_dat$combined[hbll_fits$pred_dat$combined > qs] <- qs
    }
  }

  round_density <- function(x) {
    if (x >= 10000) {
      x <- gfplot:::round_any(x, 1000)
    }
    if (x >= 1000 && x < 10000) {
      x <- gfplot:::round_any(x, 100)
    }
    if (x >= 100 && x < 1000) {
      x <- gfplot:::round_any(x, 100)
    }
    if (x >= 10 && x < 100) {
      x <- gfplot:::round_any(x, 1)
    }
    if (x >= 1 && x < 10) {
      x <- sprintf("%.1f", round(x, 1))
    }
    if (x < 1) {
      x <- sprintf("%.2f", round(x, 2))
    }
    x
  }

  format_french_1000s_expr <- function(x) {
    out <- format(as.numeric(x), big.mark = " ", scientific = FALSE, trim = TRUE)
    gsub(" ", "~", out)
  }

  if (nrow(syn_fits$raw_dat) >= 1L) { # calculate a density to label on the map
    # we've already multiplied the density by 1000 for computational reasons
    # so we need to divided by 1000 here to get back to the original units:
    syn_density <- mean(syn_fits$raw_dat$density, na.rm = TRUE) * (1000 * 1000) / 1000
    dens_units <- "~kg/km^2"
    if (syn_density > 1000) {
      dens_units <- "~t/km^2"
      syn_density <- syn_density / 1000
    }
    syn_density <- round_density(syn_density)
    if (french) syn_density <- format_french_1000s_expr(syn_density)
  }

  suppressMessages({
    g_survey_spatial_syn <-
      gfsynopsis::plot_survey_maps(syn_fits$pred_dat, syn_fits$raw_dat,
        north_symbol = TRUE, annotations = "SYN",
        syn_wchg_year = synoptic_max_survey_years[["SYN WCHG"]],
        syn_wcvi_year = synoptic_max_survey_years[["SYN WCVI"]],
        syn_qcs_hs_year = synoptic_max_survey_years[["SYN QCS"]], # FIXME HS hardcoded to QCS
        show_model_predictions = "combined" %in% names(syn_fits$pred_dat)
      ) +
      coord_cart + ggplot2::ggtitle(en2fr("Synoptic survey biomass", french)) +
      ggplot2::scale_fill_viridis_c(trans = "fourth_root_power", option = "C") +
      ggplot2::scale_colour_viridis_c(trans = "fourth_root_power", option = "C")

    if (nrow(syn_fits$raw_dat) >= 1L) { # calculate a density to label on the map
      .text <- paste0(en2fr("Mean", french), "~", syn_density, dens_units)
      if (french) {
        .text <- gsub(",", "*','*", .text) # make annotate(parse = TRUE) happy; yikes
      }
      g_survey_spatial_syn <- g_survey_spatial_syn +
        ggplot2::annotate("text", 360, 5253,
          col = "grey30", hjust = 0,
          label = .text, parse = TRUE
        )
    }
  })

  # an internal IPHC function:
  # if (!is.null(dat_iphc)) {
  #   iphc_map_dat <- format_final_year_for_map_iphc(dat_iphc$set_counts,
  #     final_year = iphc_max_survey_year)
  #   # iphc_map_dat$akima_depth <- mean(iphc_fits$raw_dat$depth, na.rm = TRUE)
  #   iphc_map_dat$combined <- ifelse(iphc_map_dat$combined == 0, NA,
  #     iphc_map_dat$combined)
  #
  #   if (sum(!is.na(iphc_map_dat$combined)) >= 1L) { # calculate a density to label on the map
  #     iphc_density <- mean(iphc_map_dat$combined, na.rm = TRUE)
  #     iphc_density <- round_density(iphc_density)
  #   }
  # } else {
  #   stop("No IPHC data.")
  #   iphc_density <- ""
  #   iphc_map_dat <- iphc_fits$pred_dat
  # }

  # hack to create IPHC raw data as of 2020:
  dd <- dat_iphc$set_counts
  dd <- dd[dd$year == iphc_max_survey_year, , drop = FALSE]
  # if (iphc_max_survey_year == 2020) {
  #   iphc2019 <- dat_iphc$set_counts[
  #     dat_iphc$set_counts$year == iphc_max_survey_year, , drop = FALSE]
  #   iphc_south <- filter(iphc2019, !station %in% dd$station)
  #   dd <- bind_rows(dd, iphc_south) # adds empty WCVI stations
  #   dd$year <- iphc_max_survey_year # fake
  # }
  dd$X <- dd$lon
  dd$Y <- dd$lat
  dd <- dplyr::as_tibble(gfplot:::ll2utm(dd, utm_zone = 9))
  dd$present <- ifelse(dd$C_it20 > 0, 1, 0)
  dd$density <- dd$C_it20
  dd$combined <- dd$density
  dd$combined <- ifelse(dd$combined == 0, NA, dd$combined)
  dd$depth <- NA
  dd$depth[1] <- 0 # fake for min()
  dd$depth[2] <- 1e4 # fake for max()
  dd$survey <- "IPHC FISS"
  if (sum(!is.na(dd$combined)) >= 1L) { # calculate a density to label on the map
    iphc_density <- mean(dd$C_it20, na.rm = TRUE)
    iphc_density <- round_density(iphc_density)
    if (french) iphc_density <- format_french_1000s_expr(iphc_density)
  } else {
    iphc_density <- ""
  }

  suppressMessages({
    g_survey_spatial_iphc <-
      gfsynopsis::plot_survey_maps(
        pred_dat = dd, raw_dat = dd,
        show_raw_data = FALSE, cell_size = 2.0, circles = TRUE,
        show_model_predictions = TRUE, # hack to make plotting work; actually raw data
        annotations = "IPHC", iphc_year = iphc_max_survey_year
      ) +
      coord_cart + ggplot2::ggtitle(en2fr("IPHC survey catch rate", french)) +
      ggplot2::scale_fill_viridis_c(
        trans = "fourth_root_power", option = "C",
        na.value = "white"
      ) +
      ggplot2::scale_colour_viridis_c(
        trans = "fourth_root_power", option = "C",
        na.value = "grey35"
      )
    if (sum(!is.na(dd$combined)) >= 1L) {
      .text <- paste0(en2fr("Mean", french), "~", iphc_density, "~", en2fr("fish/skate", french))
      if (french) {
        .text <- gsub(",", "*','*", .text) # make annotate(parse = TRUE) happy
      }
      g_survey_spatial_iphc <- g_survey_spatial_iphc +
        ggplot2::annotate("text", 360, 5253,
          col = "grey30", hjust = 0,
          label = .text, parse = TRUE
        )
    }
  })

  if (nrow(hbll_fits$raw_dat) >= 1L) { # calculate a density to label on the map
    hbll_density <- mean(hbll_fits$raw_dat$density, na.rm = TRUE)
    hbll_density <- round_density(hbll_density)
    if (french) hbll_density <- format_french_1000s_expr(hbll_density)
  }
  suppressMessages({
    g_survey_spatial_hbll <-
      gfsynopsis::plot_survey_maps(hbll_fits$pred_dat, hbll_fits$raw_dat,
        pos_pt_col = "#FFFFFF35",
        bin_pt_col = "#FFFFFF12",
        pos_pt_fill = "#FFFFFF03",
        hbll_n_year = hbll_out_max_survey_years[["HBLL OUT N"]],
        hbll_s_year = hbll_out_max_survey_years[["HBLL OUT S"]],
        show_model_predictions = "combined" %in% names(hbll_fits$pred_dat),
        annotations = "HBLL"
      ) +
      coord_cart + ggplot2::ggtitle(en2fr("HBLL OUT survey biomass", french)) +
      ggplot2::scale_fill_viridis_c(trans = "fourth_root_power", option = "C") +
      ggplot2::scale_colour_viridis_c(trans = "fourth_root_power", option = "C")
    dens_units <- paste0("~", en2fr("fish", french), "/km^2")
    if (nrow(hbll_fits$raw_dat) >= 1L) { # calculate a density to label on the map

      .text <- paste0(en2fr("Mean", french), "~", hbll_density, dens_units)
      if (french) {
        .text <- gsub(",", "*','*", .text) # make annotate(parse = TRUE) happy
      }
      g_survey_spatial_hbll <- g_survey_spatial_hbll +
        ggplot2::annotate("text", 360, 5253,
          col = "grey30", hjust = 0,
          label = .text, parse = TRUE
        )
    }
  })

  # Page 1 layout: -------------------------------------------------------------

  gg_catch <- ggplot2::ggplotGrob(g_catch)
  gg_survey_index <- ggplot2::ggplotGrob(g_survey_index)
  gg_cpue_spatial <- ggplot2::ggplotGrob(g_cpue_spatial)
  gg_cpue_spatial_ll <- ggplot2::ggplotGrob(g_cpue_spatial_ll)
  gg_cpue_index <- ggplot2::ggplotGrob(g_cpue_index)
  gg_survey_spatial_syn <- ggplot2::ggplotGrob(g_survey_spatial_syn)
  gg_survey_spatial_iphc <- ggplot2::ggplotGrob(g_survey_spatial_iphc)
  gg_survey_spatial_hbll <- ggplot2::ggplotGrob(g_survey_spatial_hbll)

  fg_catch <- egg::gtable_frame(gg_catch, debug = debug)
  fg_survey_index <- egg::gtable_frame(gg_survey_index, debug = debug)
  fg_cpue_spatial <- egg::gtable_frame(gg_cpue_spatial, debug = debug)
  fg_cpue_spatial_ll <- egg::gtable_frame(gg_cpue_spatial_ll, debug = debug)
  fg_cpue_index <- egg::gtable_frame(gg_cpue_index,
    debug = debug,
    width = grid::unit(0.7, "null")
  )
  fg_survey_spatial_syn <- egg::gtable_frame(gg_survey_spatial_syn, debug = debug)
  fg_survey_spatial_iphc <- egg::gtable_frame(gg_survey_spatial_iphc, debug = debug)
  fg_survey_spatial_hbll <- egg::gtable_frame(gg_survey_spatial_hbll, debug = debug)

  f_topleft <- egg::gtable_frame(
    fg_survey_index,
    width = grid::unit(1, "null"),
    height = grid::unit(1.35, "null"),
    debug = debug
  )

  f_topright <- egg::gtable_frame(
    gridExtra::gtable_cbind(fg_catch, fg_cpue_index),
    width = grid::unit(1, "null"),
    height = grid::unit(1, "null"),
    debug = debug
  )

  f_fake_text <- egg::gtable_frame(
    ggplot2::ggplotGrob(ggplot2::ggplot() +
      ggplot2::ggtitle(" ") +
      theme_pbs()),
    width = grid::unit(1, "null"),
    height = grid::unit(0.3, "null"),
    debug = debug
  )

  f_top <- egg::gtable_frame(
    gridExtra::gtable_cbind(f_topleft, f_topright),
    width = grid::unit(1, "null"),
    height = grid::unit(1, "null"),
    debug = debug
  )

  f_bottom <- egg::gtable_frame(
    gridExtra::gtable_cbind(
      fg_survey_spatial_syn, fg_survey_spatial_hbll, fg_survey_spatial_iphc,
      fg_cpue_spatial, fg_cpue_spatial_ll
    ),
    width = grid::unit(1, "null"),
    height = grid::unit(1.35, "null"),
    debug = debug
  )

  f_all <- gridExtra::gtable_rbind(f_top, f_bottom)

  if (png_format) {
    png(fig_folder_spp1,
      width = width * resolution,
      height = height * resolution * short_page_height_ratio, res = resolution
    )
  } else {
    pdf(fig_folder_spp1, width = width, height = height * short_page_height_ratio)
  }
  grid::grid.newpage()
  grid::grid.draw(f_all)
  dev.off()

  # Page 2 layout: -------------------------------------------------------------
  gg_mat_age <- ggplot2::ggplotGrob(g_mat_age)
  gg_mat_length <- ggplot2::ggplotGrob(g_mat_length)
  gg_mat_month <- ggplot2::ggplotGrob(g_maturity_month)
  gg_lengths <- ggplot2::ggplotGrob(g_lengths)
  gg_ages <- ggplot2::ggplotGrob(g_ages)
  gg_length_weight <- ggplot2::ggplotGrob(g_length_weight)
  gg_vb <- ggplot2::ggplotGrob(g_vb)
  gg_comm_samples <- ggplot2::ggplotGrob(g_comm_samples)
  gg_survey_samples <- ggplot2::ggplotGrob(g_survey_samples)

  fg_mat_age <- egg::gtable_frame(gg_mat_age, debug = debug)
  fg_mat_length <- egg::gtable_frame(gg_mat_length, debug = debug)
  fg_mat_month <- egg::gtable_frame(gg_mat_month, debug = debug)
  fg_lengths <- egg::gtable_frame(gg_lengths, debug = debug)
  fg_ages <- egg::gtable_frame(gg_ages, debug = debug)
  fg_length_weight <- egg::gtable_frame(gg_length_weight, debug = debug)
  fg_vb <- egg::gtable_frame(gg_vb, debug = debug)
  fg_comm_samples <- egg::gtable_frame(gg_comm_samples, debug = debug)
  fg_survey_samples <- egg::gtable_frame(gg_survey_samples, debug = debug)

  f_topright <- egg::gtable_frame(
    gridExtra::gtable_rbind(fg_vb, fg_length_weight),
    width = grid::unit(1, "null"),
    height = grid::unit(1, "null"),
    debug = debug
  )

  f_topleft <- egg::gtable_frame(
    fg_lengths,
    width = grid::unit(2.2, "null"),
    height = grid::unit(1, "null"),
    debug = debug
  )

  f_top <- egg::gtable_frame(
    gridExtra::gtable_cbind(f_topleft, f_topright),
    width = grid::unit(1, "null"),
    height = grid::unit(1, "null"),
    debug = debug
  )

  f_middle <- egg::gtable_frame(
    fg_ages,
    width = grid::unit(1, "null"),
    height = grid::unit(0.75, "null"),
    debug = debug
  )

  f_bottom <- egg::gtable_frame(
    gridExtra::gtable_cbind(fg_mat_age, fg_mat_length, fg_mat_month),
    width = grid::unit(1, "null"),
    height = grid::unit(0.3, "null"),
    debug = debug
  )

  f_very_bottom <- egg::gtable_frame(
    gridExtra::gtable_cbind(fg_survey_samples, fg_comm_samples),
    width = grid::unit(1, "null"),
    height = grid::unit(0.17, "null"),
    debug = debug
  )

  f_all <- gridExtra::gtable_rbind(f_top, f_middle, f_bottom, f_very_bottom)

  if (png_format) {
    png(fig_folder_spp2,
      width = width * resolution,
      height = height * resolution, res = resolution
    )
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
