#' Join reference data onto species data frame and clean up
#'
#' @param spp Output from [get_spp_names()]
#'
#' @returns A modified data frame
#' @export
#'
#' @examples
#' get_spp_names() |> join_refs_spp()
join_refs_spp <- function(spp, french = FALSE) {

  refs <- readr::read_csv(here("report/spp-refs.csv"), show_col_types = FALSE)
  spp <- left_join(spp, refs, by = "species_common_name")
  spp$type_other_ref[!is.na(spp$type_other_ref)] <-
    rosettafish::en2fr(spp$type_other_ref[!is.na(spp$type_other_ref)], french, allow_missing = TRUE)

  spp$species_science_name <- gfplot:::firstup(spp$species_science_name)
  spp$species_science_name <- gsub(" complex", "", spp$species_science_name)
  spp$resdoc <- gsub(", ", ", @", spp$resdoc)
  spp$resdoc <- ifelse(is.na(spp$resdoc), "", paste0("@", spp$resdoc, ""))
  spp$sar <- gsub(", ", ", @", spp$sar)
  spp$sar <- ifelse(is.na(spp$sar), "", paste0("@", spp$sar, ""))
  spp$other_ref <- gsub(", ", ", @", spp$other_ref)
  spp$other_ref_cite <- ifelse(is.na(spp$other_ref), "",
    paste0(spp$type_other_ref, ": @", spp$other_ref, "")
  )
  spp <- arrange(spp, species_code)
  spp <- spp %>%
    mutate(species_common_name = gsub(
      "rougheye/blackspotted rockfish complex",
      "Rougheye/Blackspotted Rockfish Complex", species_common_name
    )) %>%
    mutate(species_common_name = gsub(
      "c-o sole",
      "C-O Sole", species_common_name
    ))
  if (isFALSE(french)) {
    spp$species_french_name <-
      tolower(rosettafish::en2fr(gfsynopsis:::first_cap(spp$species_common_name)))
    spp$species_common_name <- tolower(spp$species_common_name)
  } else { # French
    spp$species_french_name <- rosettafish::en2fr(spp$species_common_name)
    spp$species_french_name <- purrr::map_chr(spp$species_french_name, gfsynopsis:::cap)
    spp$species_common_name <- tolower(spp$species_common_name)
  }
  spp
}
