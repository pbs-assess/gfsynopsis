#' Join ITIS data to species data frame
#'
#' @param spp A species data frame from [get_spp_names()]
#'
#' @returns A cleaned data frame
#' @export
#'
#' @examples
#' get_spp_names() |>
#'   join_itis_spp()
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
  spp[spp$worms_id == "unknown", ]

  # Missing from ITIS:
  spp$order[spp$species_common_name == "deacon rockfish"] <-
    spp$order[spp$species_common_name == "vermilion rockfish"]
  spp$family[spp$species_common_name == "deacon rockfish"] <-
    spp$family[spp$species_common_name == "vermilion rockfish"]

  spp
}
