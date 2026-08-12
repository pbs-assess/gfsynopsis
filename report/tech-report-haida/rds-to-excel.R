library(writexl)
library(purrr)

report_dir <- here::here("report/tech-report-haida")
rds_dir <- file.path(report_dir, "data-export")
out_dir <- file.path(report_dir, "data-export", "excel")
dir.create(out_dir, showWarnings = FALSE)

f <- list.files(rds_dir, pattern = "\\.rds$", full.names = TRUE)

create_data_summary_sheet <- function(species_data, species_name) {
  descriptions <- c(
    survey_index = "Survey biomass/abundance indices",
    catch_totals = "Total commercial catch by year",
    trawl_cpue_index = "Standardized trawl CPUE index from commercial data (whole area only)",
    maps_synoptic_biomass = "Synoptic survey biomass density map data",
    maps_hbll_catch = "Hard bottom longline (HBLL) survey catch map data",
    maps_iphc_catch = "IPHC FISS survey catch match",
    maps_commercial_trawl_cpue = "Commercial trawl CPUE match data",
    maps_commercial_longline_cpue = "Commercial longline CPUE map data",
    length_compositions = "Length frequency distributions",
    age_compositions = "Age frequency distributions",
    survey_specimen_counts = "Number of biological specimens collected from surveys by year",
    commercial_specimen_counts = "Number of biological specimens collected from commercial catch by year"
  )

  # Create status column (simplified - all "no data" cases unified)
  status_info <- sapply(species_data, function(x) {
    # Check for "no data" conditions
    if (is.null(x) ||
        (length(x) == 1 && is.logical(x) && is.na(x)) ||
        (is.data.frame(x) && nrow(x) == 0)) {
      return("No data, worksheet not included")
    }

    # Data exists - show row count if data frame, otherwise generic message
    if (is.data.frame(x)) return(paste0(format(nrow(x), big.mark = ","), " rows"))
  })

  tibble(
    `Data type` = names(species_data),
    `Description` = descriptions[names(species_data)],
    `Data availability` = status_info
  )
}

export_species_to_excel <- function(species_rds_path, output_dir) {
  message("Exporting ", basename(species_rds_path), " to Excel")
  species_data <- readRDS(species_rds_path)
  species_name <- tools::file_path_sans_ext(basename(species_rds_path))

  data_summary <- create_data_summary_sheet(species_data, species_name)

  species_data_clean <- species_data[vapply(species_data, function(x) {
    !is.null(x) &&
      !(length(x) == 1 && is.logical(x) && is.na(x)) &&
      (if (is.data.frame(x)) nrow(x) > 0 else length(x) > 0)
  }, logical(1))]

  if ("catch_totals" %in% names(species_data_clean)) {
    species_data_clean$catch_totals <- species_data_clean$catch_totals |>
      filter(area != "3CD")
  }

  if ("survey_index" %in% names(species_data_clean)) {
    species_data_clean$survey_index <- species_data_clean$survey_index |>
      filter(!is.na(num_sets))
  }

  if ("maps_synoptic_biomass" %in% names(species_data_clean)) {
    species_data_clean$maps_synoptic_biomass <- species_data_clean$maps_synoptic_biomass |>
      select(-any_of(c("pos", "bin", "year", "survey_domain_year", "utm_zone", "cell_area", "survey_series_name"))) |>
      rename(depth = "akima_depth") |>
      mutate(year = ifelse(survey == "SYN QCS-HS", 2023, 2024))
  }

  if ("maps_hbll_catch" %in% names(species_data_clean)) {
    species_data_clean$maps_hbll_catch <- species_data_clean$maps_hbll_catch |>
      select(-any_of(c("pos", "bin"))) |>
      rename(depth = "akima_depth") |>
      mutate(year = ifelse(survey == "HBLL OUT N", 2023, 2024))
  }

  if ("maps_iphc_catch" %in% names(species_data_clean)) {
      species_data_clean$maps_iphc_catch <- species_data_clean$maps_iphc_catch |>
      select(
        survey,
        year, station, station_key,
        lon, lat,
        catch = number_observed,
        effective_skates
      )
  }

  species_data_with_readme <- c(list(data_summary = data_summary), species_data_clean)

  output_path <- file.path(output_dir, paste0(species_name, ".xlsx"))
  writexl::write_xlsx(species_data_with_readme, path = output_path)

  message("Exported ", length(species_data_clean), " README + data sheets for ",
          species_name)

  invisible(output_path)
}

walk(f, export_species_to_excel, output_dir = out_dir)
