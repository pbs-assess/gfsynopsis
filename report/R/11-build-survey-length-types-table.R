# Build the LaTeX table of survey length measurements used in the synopsis.

library(dplyr)
library(purrr)

data_cache <- here::here("report", "data-cache-2026-07")
output_file <- here::here(
  "report", "tech-report-main", "survey-length-types.tex"
)

# report_length_surveys <- c(
#   "SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI",
#   "HBLL OUT N", "HBLL OUT S", "HBLL INS N", "HBLL INS S",
#   "MSSM WCVI", "IPHC FISS"
# )

# Keep this definition aligned with report/R/03-load-data.R so that the table
# covers the same species as the main report.
report_spp <- gfsynopsis::get_spp_names() |>
  filter(species_common_name != "herrings") |>
  arrange(species_common_name)

species_files <- file.path(data_cache, paste0(report_spp$spp_w_hyphens, ".rds"))
missing_files <- species_files[!file.exists(species_files)]
if (length(missing_files)) {
  stop(
    "Missing species data files: ",
    paste(basename(missing_files), collapse = ", "),
    call. = FALSE
  )
}

survey_samples <- map_dfr(species_files, \(file) readRDS(file)$survey_samples)

length_type_table <- survey_samples |>
  filter(
    # survey_abbrev %in% report_length_surveys,
    !is.na(length),
    !is.na(length_type)
  ) |>
  distinct(species_common_name, length_type) |>
  group_by(species_common_name) |>
  summarise(
    length_type = paste(sort(unique(length_type)), collapse = ", "),
    .groups = "drop"
  ) |>
  arrange(species_common_name) |>
  rename(
    Species = species_common_name,
    `Length type` = length_type
  ) |>
  mutate(Species = stringr::str_to_title(Species))

# Species without usable length samples in a report survey have no row. This
# avoids reporting a length type based solely on a survey that is not plotted.
latex_table <- knitr::kable(
  length_type_table,
  format = "latex",
  booktabs = TRUE,
  longtable = TRUE,
  caption = "Length type used for length samples."
)

writeLines(latex_table, output_file)
