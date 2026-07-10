#' Join ITIS data to species data frame
#'
#' @param spp A species data frame from [get_spp_names()]
#'
#' @returns A cleaned data frame
#' @export
join_itis_spp <- function(spp) {
  if (!file.exists(here("report", "itis.rds"))) {
    cls <- taxize::classification(spp$itis_tsn[!is.na(spp$itis_tsn)], db = 'itis')
    saveRDS(cls, file = here("report", "itis.rds"))
  } else {
    cls <- readRDS(here("report", "itis.rds"))
  }
  cls <- plyr::ldply(cls) %>%
    rename(itis_tsn = .id) %>%
    filter(rank %in% c('order', 'family')) %>%
    reshape2::dcast(itis_tsn ~ rank, value.var = 'name')
  spp <- left_join(spp, mutate(cls, itis_tsn = as.integer(itis_tsn)),
    by = "itis_tsn")

  spp[grep("tope", spp$species_common_name),"worms_id"] <- "105820"
  spp[spp$worms_id == "unknown", "worms_id"] <- NA

  # Missing from ITIS:
  spp$order[spp$species_common_name == "deacon rockfish"] <-
    spp$order[spp$species_common_name == "vermilion rockfish"]
  spp$family[spp$species_common_name == "deacon rockfish"] <-
    spp$family[spp$species_common_name == "vermilion rockfish"]

  spp
}

#' Join WoRMS data to species data frame
#'
#' @param spp A species data frame from [get_spp_names()]
#'
#' @returns A cleaned data frame
#' @export
join_worms_spp <- function(spp, check_cache = TRUE) {
  worms_file <- here("report", "worms.rds")

  spp[grep("tope", spp$species_common_name), "worms_id"] <- "105820"
  valid_worms_ids <- spp$worms_id[
    !is.na(spp$worms_id) &
      spp$worms_id != "unknown" &
      nzchar(trimws(spp$worms_id))
  ]

  if (!file.exists(worms_file) | !check_cache) {
    cls <- taxize::classification(valid_worms_ids, db = "worms")
    saveRDS(cls, file = worms_file)
  } else {
    cls <- readRDS(worms_file)
  }

  is_valid_entry <- vapply(cls, function(x) inherits(x, "data.frame"), logical(1)) &
    nzchar(names(cls))
  if (any(!is_valid_entry)) {
    warning(
      sprintf(
        "Skipping %d malformed WoRMS cache entr%s in %s.",
        sum(!is_valid_entry),
        if (sum(!is_valid_entry) == 1L) "y" else "ies",
        worms_file
      ),
      call. = FALSE
    )
  }
  cls_valid <- cls[is_valid_entry]

  cls <- cls_valid |>
    purrr::map_dfr(~ .x, .id = "worms_id") |>
    dplyr::filter(rank %in% c("Order", "Family")) |>
    tidyr::pivot_wider(
      id_cols = -id,
      names_from = rank,
      values_from = name
    ) |>
    dplyr::rename_with(tolower)

  spp <- left_join(spp, cls, by = "worms_id")

  spp$order[spp$species_common_name == "rougheye/blackspotted rockfish complex"] <-
    spp$order[spp$species_common_name == "pacific ocean perch"]
  spp$family[spp$species_common_name == "rougheye/blackspotted rockfish complex"] <-
    spp$family[spp$species_common_name == "pacific ocean perch"]

  spp
}
