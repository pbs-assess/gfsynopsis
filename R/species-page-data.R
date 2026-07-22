#' Build shared data for one species page
#'
#' This is the common data source for the PDF/Rmd species pages and the web
#' species pages. Formatting for either output belongs in the corresponding
#' generator, not here.
#'
#' @param x A species common name.
#' @param spp The species metadata data frame used to build the report.
#' @param french Logical; use French display text where it is available.
#' @param ext Image file extension.
#'
#' @return A named list containing the species metadata, links, references,
#'   notes, and image paths.
#' @keywords internal
species_page_data <- function(x, spp, french = FALSE, ext = "png") {
  row_number <- which(spp$species_common_name == x)
  if (length(row_number) != 1L) {
    stop("Expected exactly one metadata row for species: ", x, call. = FALSE)
  }
  row <- spp[row_number, , drop = FALSE]

  field <- function(name, default = NA_character_) {
    if (!name %in% names(row)) return(default)
    value <- row[[name]][[1L]]
    if (length(value) == 0L) default else value
  }

  title <- if (french) {
    field("species_french_name")
  } else {
    stringr::str_to_title(x)
  }
  if (identical(title, "North Pacific Spiny Dogfish")) {
    title <- "Pacific Spiny Dogfish"
  }
  if (identical(title, "Popeye")) {
    title <- "Popeye grenadier"
  }

  scientific_name <- field("species_science_name")
  species_code <- field("species_code")
  image_slug <- clean_name(x)
  slug <- field("spp_w_hyphens", image_slug)
  if (is.na(slug) || !nzchar(slug)) slug <- image_slug

  fishbase_name <- gfplot:::firstup(scientific_name)
  fishbase_links <- if (identical(species_code, "394")) {
    split_names <- rougheye_split(fishbase_name)
    list(
      list(label = "FishBase 1", url = paste0(
        "http://www.fishbase.org/summary/", split_names[[1L]]
      )),
      list(label = "FishBase 2", url = paste0(
        "http://www.fishbase.org/summary/", split_names[[2L]]
      ))
    )
  } else if (identical(species_code, "039")) {
    list(list(
      label = "FishBase",
      url = "http://www.fishbase.org/Summary/FamilySummary.php?ID=11"
    ))
  } else {
    list(list(
      label = "FishBase",
      url = paste0(
        "http://www.fishbase.org/summary/",
        gsub(" ", "-", fishbase_name)
      )
    ))
  }

  worms_id <- field("worms_id")
  valid_worms_id <- length(worms_id) == 1L &&
    !is.na(worms_id) &&
    grepl("^[0-9]+$", as.character(worms_id))
  worms_link <- if (valid_worms_id) {
    list(
      label = "WoRMS",
      url = paste0(
        "http://www.marinespecies.org/aphia.php?p=taxdetails&id=", worms_id
      )
    )
  } else {
    NULL
  }

  notes <- character()
  if (identical(species_code, "225")) {
    notes <- if (!french) {
      "Note that Pacific Hake undergoes a directed joint
      Canada-US coastwide\n acoustic survey and annual assessment, which are not
      included in this report. The most recent\n stock assessment
      should be consulted for details on stock status."
    } else {
      "Il est à noter que le merlu du Chili fait l’objet d’un relevé et d’une évaluation annuels ciblés menés conjointement par le Canada et les É.-U. à l'échelle de la côte, qui ne sont pas compris dans le présent rapport. L’évaluation la plus récente des stocks doit être consultée pour obtenir des détails sur l’état des stocks."
    }
  }
  if (identical(species_code, "614")) {
    notes <- if (!french) {
      "Note that Pacific Halibut undergoes thorough assessment by the
      International Pacific\n Halibut Commission based on [the annual
      standardized setline survey](https://www.iphc.int/research/fishery-independent-monitoring/). The most\n recent [stock assessment](https://www.iphc.int/research/stock-assessment/)
      should be consulted for details on stock status."
    } else {
      "Il est à noter que le flétan du Pacifique fait l’objet d’une évaluation approfondie par la Commission internationale du flétan du Pacifique qui se fonde sur un relevé annuel normalisé en fonction de la ligne de référence. L’évaluation la plus récente des stocks doit être consultée pour obtenir des détails sur l’état des stocks."
    }
  }
  if (identical(species_code, "455")) {
    notes <- if (!french) {
      "The annual sablefish trap survey is not included in this report. Commercial biological samples from a head-only sampling program that began in 2018 [@lacko2023] are not shown."
    } else {
      "Il est à noter que la morue charbonnière fait l’objet de relevés annuels au casier ciblés qui servent à l’évaluation des stocks et qui ne sont pas compris dans le présent rapport. L’évaluation la plus récente des stocks doit être consultée pour obtenir des détails sur l’état des stocks."
    }
  }

  list(
    slug = slug,
    image_slug = image_slug,
    common_name = title,
    scientific_name = scientific_name,
    species_code = species_code,
    order = field("order"),
    family = field("family"),
    links = c(fishbase_links, if (!is.null(worms_link)) list(worms_link)),
    references = list(
      research_documents = field("resdoc", ""),
      science_advisory_reports = field("sar", ""),
      other = field("other_ref_cite", ""),
      cosewic_status_report = field("cosewic_status_reports", "")
    ),
    cosewic_status = field("cosewic_status"),
    sara_status = field("sara_status"),
    notes = notes,
    images = file.path(
      "figure-pages",
      paste0(image_slug, "-", seq_len(2L), ".", ext)
    )
  )
}

# Build shared page data for every species in report order.
#
# @noRd
species_pages_data <- function(spp, french = FALSE, ext = "png") {
  lapply(
    spp$species_common_name,
    species_page_data,
    spp = spp,
    french = french,
    ext = ext
  )
}
