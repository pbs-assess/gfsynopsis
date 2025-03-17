#' Get COSEWIC data
#'
#' @importFrom dplyr group_by summarise filter n_distinct distinct rename
#' @importFrom readr read_csv
#' @export
get_cosewic_data <- function() {
  # downloaded from:
  # https://species-registry.canada.ca/index-en.html#/species?ranges=Pacific%20Ocean&taxonomyId=Fishes%20%28marine%29&sortBy=commonNameSort&sortDirection=asc&pageSize=10
  # they seem to keep changing URL query syntax, so: Range - Pacific Ocean; Taxonomic group - Fishes (marine)
  # on 2024-06-18

  # Also check for new Species at Risk/COSEWIC documents ---
  # See: https://species-registry.canada.ca/index-en.html#/documents?documentTypeId=18,14,11,9&ranges=18&sortBy=documentTypeSort&sortDirection=asc&pageSize=10
  # Can download and sort by publication date

  cos <- readr::read_csv(here::here("report/COSEWIC-species.csv"), show_col_types = FALSE)
  cos <- dplyr::filter(cos, !grepl("Salmon", `COSEWIC common name`))
  cos <- dplyr::filter(cos, !grepl("Trout", `COSEWIC common name`))
  # cos <- dplyr::filter(cos, !grepl("Pixie Poacher", `COSEWIC common name`)) # pixie poacher seems to have been removed since 2024
  cos <- rename(cos, species_science_name = `Scientific name`,
    cosewic_status = `COSEWIC status`, sara_status = `Schedule status`,
    cosewic_pop = `COSEWIC population`)
  # For species with different cosewic statuses for different populations, combine the text:
  cos_multi_pop <- cos |>
    group_by(species_science_name) |>
    filter(n_distinct(cosewic_status) > 1) |>
    group_by(cosewic_status, .add = TRUE) |>
    summarise(cosewic_pop_status =
        paste0(unique(cosewic_status), " (", paste(unique(cosewic_pop), collapse = ", "), ")"),
      .groups = 'drop_last') |>
    summarise(cosewic_pop_status = paste0(unique(cosewic_pop_status), collapse = ", "), .groups = 'drop')
  # duplicate of inside YE:
  cos <- select(cos, species_science_name, cosewic_status, sara_status)
  cos <- mutate(cos, species_science_name = ifelse(grepl("type I", species_science_name), "Sebastes aleutianus/melanostictus complex", species_science_name))
  cos <- left_join(cos, cos_multi_pop, by = "species_science_name") %>%
    mutate(cosewic_status = ifelse(!is.na(cosewic_pop_status), cosewic_pop_status, cosewic_status)) %>%
    select(-cosewic_pop_status)
  cos$species_science_name <- tolower(cos$species_science_name)
  cos <- distinct(cos) # to remove duplicates (e.g., Eulachon, YE, RE/BS rockfish)
  cos
}
