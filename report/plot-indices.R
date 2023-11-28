# assemble data frame of design-based indices
# assemble data frame of geostat-based indices
# scale geostat to max 1
# scale design to have some geo mean
# add any shading

# make base design-plot dots/lines
# overlay geostat ribbons

# library(purrr)
# library(dplyr)
# library(ggplot2)
# library(here)
# devtools::load_all(".")

make_index_panel <- function(spp_w_hyphens, final_year_surv = 2022, french = FALSE) {
  cat(crayon::green(clisymbols::symbol$tick), spp_w_hyphens, "\n")
  # setup -------------------------------------------------
  # spp_w_hyphens <- "pacific-cod"
  # dc <- here("report", "data-cache-oct-2023")
  dat <- readRDS(paste0(file.path(dc, spp_w_hyphens), ".rds"))
  report_folder <- "report"
  iphc_index_cache <- file.path(report_folder, "iphc-cache")
  dir.create(iphc_index_cache, showWarnings = FALSE, recursive = TRUE)
  iphc_index_cache_spp <- paste0(file.path(iphc_index_cache, spp_w_hyphens), ".rds")

  survey_cols <- c(
    "SYN WCHG" = "#E41A1C",
    "SYN WCVI" = "#984EA3",
    "SYN HS" = "#377EB8",
    "SYN QCS" = "#4DAF4A",
    "HBLL OUT N" = "#FF7F00",
    "HBLL OUT S" = "#FDBF6F",
    "HBLL INS N/S" = "#A65628",
    "IPHC FISS" = "#F781BF",
    "MSSM WCVI" = "#6c6c6c",
    "MSSM Geostat" = "#6c6c6c",
    "MSA HS" = "#6c6c6c",
    "SYN HS/QCS/WCHG/WCVI" = "#6c6c6c",
    "SYN HS/QCS/WCVI" = "#6c6c6c",
    "HBLL OUT N/S" = "#6c6c6c",
    "Commercial" = "#303030"
  )

  lvls <- c(
    "SYN WCHG",
    "SYN HS",
    "SYN QCS",
    "SYN WCVI",
    "HBLL OUT N",
    "HBLL OUT S",
    "SYN WCHG/HS/QCS/WCVI",
    "SYN HS/QCS/WCVI",
    "HBLL OUT N/S",
    "HBLL INS N/S",
    "IPHC FISS",
    "OTHER HS MSA", # changes!
    "MSSM WCVI"
  )

  # get design indices ---------------------------------------

  dat_design <- tidy_survey_index(
    dat$survey_index,
    survey = c(
      lvls
    )
  )

  lvls[lvls == "OTHER HS MSA"] <- "MSA HS"
  dat_design$survey_abbrev <- gsub(
    "OTHER HS MSA", "MSA HS",
    dat_design$survey_abbrev
  )

  # sub iphc -------------------------------------------------

  dat_design <- gfsynopsis:::sub_iphc_design(
    dc,
    iphc_index_cache_spp, spp_w_hyphens, dat_design
  )

  # read geostat ---------------------------------------------

  get_index <- function(folder, spp, .family = "", model_tag = "st-rw") {
    paths <- list.files(folder, pattern = ".rds", full.names = TRUE)
    path <- paths[grepl(spp, paths)]
    if (length(path)) {
      file_names <- list.files(folder, pattern = ".rds")
      sp <- gsub("-", " ", spp)
      if (file.exists(path)) {
        d <- readRDS(path)
        if (length(d) > 1L) {
          return(dplyr::mutate(d, species = sp, family = .family))
        }
      }
    }
  }

  families <- c(
    "delta-gamma",
    "delta-poisson-link-gamma",
    "tweedie",
    "delta-poisson-link-lognormal",
    "delta-lognormal"
  )
  geo <- list()
  take_min_aic <- function(x) {
    if (nrow(x)) {
      filter(x, aic == min(aic))
    }
  }
  geo$syn_coast <- purrr::map_dfr(families, \(f) {
    get_index(paste0("report/stitch-cache/synoptic-", f, "/"), spp_w_hyphens, .family = f)
  }) |> take_min_aic()
  geo$syn_hs <- purrr::map_dfr(families, \(f) {
    get_index(paste0("report/stitch-cache/synoptic-SYN HS-", f, "/"), spp_w_hyphens, .family = f)
  }) |> take_min_aic()
  geo$syn_qcs <- purrr::map_dfr(families, \(f) {
    get_index(paste0("report/stitch-cache/synoptic-SYN QCS-", f, "/"), spp_w_hyphens, .family = f)
  }) |> take_min_aic()
  geo$syn_wchg <- purrr::map_dfr(families, \(f) {
    get_index(paste0("report/stitch-cache/synoptic-SYN WCHG-", f, "/"), spp_w_hyphens, .family = f)
  }) |> take_min_aic()
  geo$syn_wcvi <- purrr::map_dfr(families, \(f) {
    get_index(paste0("report/stitch-cache/synoptic-SYN WCVI-", f, "/"), spp_w_hyphens, .family = f)
  }) |> take_min_aic()
  geo$hbll_out_n <- geo$hbll_ins <- get_index("report/stitch-cache/hbll_outside_n/", spp_w_hyphens)
  geo$hbll_out_s <- geo$hbll_ins <- get_index("report/stitch-cache/hbll_outside_s/", spp_w_hyphens)
  geo$mssm <- purrr::map_dfr(families, \(f) {
    get_index(paste0("report/stitch-cache/mssm-", f, "/"), spp_w_hyphens, .family = f)
  }) |> take_min_aic()
  geo$hbll_ins <- get_index("report/stitch-cache/hbll_inside/", spp_w_hyphens)
  geo$hbll_out <- get_index("report/stitch-cache/hbll_outside/", spp_w_hyphens)
  geo$iphc <- get_index(paste0("report/stitch-cache/iphc"), spp_w_hyphens)
  if ("iphc" %in% names(geo)) {
    geo$iphc$stitch_regions <- "IPHC FISS"
  }
  if (length(geo)) {
    dat_geo <- bind_rows(geo)
    dat_geo$survey_abbrev <- dat_geo$stitch_regions
    dat_geo$survey_abbrev <- gsub("HBLL INS N, HBLL INS S", "HBLL INS N/S", dat_geo$survey_abbrev)
    dat_geo$survey_abbrev <- gsub("HBLL OUT N, HBLL OUT S", "HBLL OUT N/S", dat_geo$survey_abbrev)
    dat_geo$survey_abbrev <- gsub("SYN HS, SYN QCS, SYN WCHG, SYN WCVI", "SYN WCHG/HS/QCS/WCVI", dat_geo$survey_abbrev)
    dat_geo$survey_abbrev <- gsub("SYN HS, SYN QCS, SYN WCVI", "SYN HS/QCS/WCVI", dat_geo$survey_abbrev)
    dat_geo <- select(dat_geo, survey_abbrev, year, biomass, lowerci, upperci, mean_cv, num_sets, num_pos_sets) |> as_tibble()

    # combine both types -------------------------------------
    dat_design$method <- "design"
    dat_geo$method <- "geostat"
    dat_design$survey_abbrev <- as.character(dat_design$survey_abbrev)
    both <- bind_rows(dat_geo, dat_design)
    has_geo <- TRUE
  } else {
    has_geo <- FALSE
    both <- dat_design
    both$method <- "design"
  }
  # both <- filter(both, !(survey_abbrev == "SYN WCHG" & year %in% c(2013, 2014, 2015)))

  # scale as needed ----------------------------------------

  # prior_levels <- levels(both$survey_abbrev)
  both_scaled <- group_by(both, survey_abbrev) |>
    group_split() |>
    purrr::map_dfr(\(x) {
      both_present <- length(unique(x$method[!is.na(x$biomass)])) > 1L
      if (both_present) {
        x_geo <- filter(x, method == "geostat")
        x_des <- filter(x, method == "design")
        overlapping_years <- intersect(x_geo$year, x_des$year)

        x_geo_mean <- exp(mean(log(x_geo$biomass[x_geo$year %in% overlapping_years])))

        # some may be zero!
        temp <- x_des$biomass[x_des$year %in% overlapping_years]
        temp[temp == 0] <- min(temp[temp != 0], na.rm = TRUE)
        x_des_mean <- exp(mean(log(temp)))

        x_geo <- mutate(x_geo,
          biomass_scaled = biomass / x_geo_mean,
          lowerci_scaled = lowerci / x_geo_mean,
          upperci_scaled = upperci / x_geo_mean
        )
        x_des <- mutate(x_des,
          biomass_scaled = biomass / x_des_mean,
          lowerci_scaled = lowerci / x_des_mean,
          upperci_scaled = upperci / x_des_mean
        )

        max_geo <- max(x_geo$upperci_scaled, na.rm = TRUE)
        xx <- bind_rows(x_geo, x_des)
        mutate(xx,
          biomass_scaled = biomass_scaled / max_geo,
          lowerci_scaled = lowerci_scaled / max_geo,
          upperci_scaled = upperci_scaled / max_geo
        )
      } else {
        if (sum(!is.na(x$biomass))) {
          mutate(x,
            biomass_scaled = biomass / max(upperci, na.rm = TRUE),
            lowerci_scaled = lowerci / max(upperci, na.rm = TRUE),
            upperci_scaled = upperci / max(upperci, na.rm = TRUE)
          )
        } else {
          mutate(x,
            biomass_scaled = NA_real_,
            lowerci_scaled = NA_real_,
            upperci_scaled = NA_real_
          )
        }
      }
    }) |>
    mutate(biomass = biomass_scaled, lowerci = lowerci_scaled, upperci = upperci_scaled) |>
    mutate(survey_abbrev = factor(survey_abbrev, levels = lvls))

  # some cleanup --------------------------------------------------

  # nothing SYN COASTWIDE present:
  if (all(is.na(filter(both_scaled, survey_abbrev == "SYN HS/QCS/WCVI")$biomass)) &&
      all(is.na(filter(both_scaled, survey_abbrev == "SYN WCHG/HS/QCS/WCVI")$biomass))) {
    both_scaled <- filter(both_scaled, survey_abbrev != "SYN HS/QCS/WCVI")
    both_scaled$survey_abbrev <- forcats::fct_drop(both_scaled$survey_abbrev)
  }
  # "SYN HS/QCS/WCVI present:
  if (any(!is.na(filter(both_scaled, survey_abbrev == "SYN HS/QCS/WCVI")$biomass))) {
    both_scaled <- filter(both_scaled, survey_abbrev != "SYN WCHG/HS/QCS/WCVI")
    both_scaled$survey_abbrev <- forcats::fct_drop(both_scaled$survey_abbrev)
  }
  # "SYN WCHG/HS/QCS/WCVI present:
  if (any(!is.na(filter(both_scaled, survey_abbrev == "SYN WCHG/HS/QCS/WCVI")$biomass))) {
    both_scaled <- filter(both_scaled, survey_abbrev != "SYN HS/QCS/WCVI")
    both_scaled$survey_abbrev <- forcats::fct_drop(both_scaled$survey_abbrev)
  }

  geo_scaled <- filter(both_scaled, method == "geostat")
  des_scaled <- filter(both_scaled, method == "design")

  labs <- distinct(select(both_scaled, survey_abbrev))
  yrs <- c(1984, final_year_surv)

  # get stats ----------------------------------------------

  stats_df <- select(both_scaled, survey_abbrev, mean_cv, num_sets, num_pos_sets, method) %>%
    distinct() |>
    group_by(survey_abbrev) %>%
    group_split() |>
    purrr::map_dfr(\(x) {
      if (length(unique(x$method)) > 1) {
        filter(x, method == "geostat")
      } else {
        x
      }
    }) |>
    group_by(survey_abbrev) |>
    summarise(
      mean_cv = sprintf("%.2f", round(mean(mean_cv, na.rm = TRUE), 2)),
      mean_num_pos_sets = sprintf("%.0f", round(mean(num_pos_sets, na.rm = TRUE), 0)),
      mean_num_sets = sprintf("%.0f", round(mean(num_sets, na.rm = TRUE), 0))
    ) %>%
    mutate(sets = paste0(en2fr("Mean +ve sets", french), ": ", mean_num_pos_sets, "/", mean_num_sets)) %>%
    mutate(cv = paste0(en2fr("Mean", french), " CV: ", mean_cv)) %>%
    mutate(cv = ifelse(mean_cv == "NaN", "", cv)) %>%
    mutate(sets = ifelse(mean_num_pos_sets == "NaN", "", sets)) %>%
    mutate(mean_cv = as.numeric(mean_cv)) %>%
    mutate(
      mean_num_pos_sets = as.numeric(mean_num_pos_sets),
      mean_num_sets = as.numeric(mean_num_sets)
    )

  # plot ----------------------------------------

  suppressMessages(suppressWarnings({
    g <- plot_survey_index(
      dat = des_scaled,
      scale = FALSE,
      col = c("grey60", "grey20"),
      max_cv = 1,
      survey_cols = survey_cols,
      xlim = c(1984 - 0.2, final_year_surv + 0.2),
      french = french,
      scale_type = "max-CI",
      pjs_mode = TRUE
    ) +
      coord_cartesian(
        ylim = c(-0.004, 1.03),
        xlim = c(yrs[1], final_year_surv) + c(-0.75, 0.75), expand = FALSE
      )

    if (has_geo) {
      g <- g + geom_line(data = geo_scaled, mapping = aes(colour = survey_abbrev)) +
        geom_ribbon(
          data = geo_scaled,
          mapping = aes(ymin = lowerci_scaled, ymax = upperci_scaled, fill = survey_abbrev), alpha = 0.2
        )
    }
    g <- g + scale_x_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      ) + ggplot2::ggtitle(en2fr("Survey relative biomass indices", french)) +
      theme(
        strip.background = element_blank(),
        strip.text.x = element_blank()
      ) +
      geom_text(
        data = labs, x = yrs[1] + 0.5, y = 0.89,
        aes(label = survey_abbrev),
        inherit.aes = FALSE, colour = "grey30", size = 3, hjust = 0
      )

    if ("MSSM WCVI" %in% both_scaled$survey_abbrev && !all(is.na(filter(both_scaled, survey_abbrev == "MSSM WCVI")$biomass_scaled))) {
      g <- g +
        geom_rect(
          data = filter(both_scaled, survey_abbrev == "MSSM WCVI")[1, , drop = FALSE],
          mapping = ggplot2::aes(xmin = yrs[1] - 2, xmax = 2003, ymin = -Inf, ymax = Inf),
          alpha = 0.15
        )
    }

    g <- g + geom_text(
      data = stats_df, aes(label = cv),
      x = yrs[1] + 0.5, y = 0.67,
      colour = "grey35", size = 2.65, hjust = 0
    ) +
      geom_text(
        data = stats_df,
        aes(label = sets),
        x = yrs[1] + 0.5, y = 0.49,
        colour = "grey35", size = 2.65, hjust = 0
      )

  }))
  g
}
