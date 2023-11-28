sub_iphc_design <- function(dc, iphc_index_cache_spp, spp_w_hyphens, orig_design) {
  dat_iphc <- readRDS(file.path(dc, "iphc", paste0(spp_w_hyphens, ".rds")))

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
      if (nrow(orig_design)) {
        out <- orig_design |>
          filter(survey_abbrev != "IPHC FISS") |>
          rbind(iphc_set_counts_sp_format)
      } else {
        out <- iphc_set_counts_sp_format
      }
      return(out)
    } else {
      return(orig_design)
    }
  }
}
