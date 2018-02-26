get_spp_names <- function() {
  file <- system.file("extdata", "spp-of-interest.csv", package = "gfplot")
  spp <- readr::read_csv(file,
    col_types = list(species_common_name = readr::col_character(),
      type = readr::col_character()))
  spp$species_common_name <- tolower(gsub(" $", "", spp$species_common_name))
  spp <- dplyr::filter(spp, !.data$species_common_name %in% c(
    "sixgill shark",
    "soupfin shark",
    "pectoral rattail",
    "lamp grenadier",
    "pearly prickleback"
  ))
  spp$species_common_name <- sub("spiny dogfish", "north pacific spiny dogfish",
    spp$species_common_name)
  spp <- spp[!duplicated(spp), ]
  spp$spp_w_hyphens <- gsub("/", "-", gsub(" ", "-", spp$species_common_name))
  as.data.frame(spp)
}

