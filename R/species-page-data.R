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
  if (identical(species_code, "096")) {
    notes <- if (!french) {
      "Note that only incidental Pacific Herring data from the groundfish databases are shown in this report. The latest cited CSAS reports should be consulted for details on Pacific Herring data and stock status."
    } else {
      "Il est à noter que seules les données accessoires sur le hareng du Pacifique provenant des bases de données sur les poissons de fond sont présentées dans le présent rapport. Les derniers rapports cités du SCCS devraient être consultés pour obtenir des détails sur l’état des stocks de hareng du Pacifique."
    }
  }
  if (identical(species_code, "405")) {
    notes <- c(
      notes,
      if (!french) {
        paste0(
          "In the 2025 assessment, it was recommended that if a 40% change in the ",
          "3-year running average of the coastwide geostatistical index or the ",
          "coastwide commercial CPUE index relative to the 2024 value was observed, ",
          "an early reassessment of Silvergray Rockfish should be brought forward for ",
          "consideration. In 2025, this exceptional circumstance criterion was **not** met."
        )
      } else {
        paste0(
          "Dans l’évaluation de 2025, il a été recommandé que, si une variation de ",
          "40 % de la moyenne mobile sur 3 ans de l’indice géostatistique à l’échelle ",
          "de la côte ou de l’indice de CPUE commerciale à l’échelle de la côte par ",
          "rapport à la valeur de 2024 était observée, une réévaluation anticipée du ",
          "sébaste argenté soit proposée pour examen. En 2025, ces critères de ",
          "circonstances exceptionnelles n’ont **pas** été remplis."
        )
      }
    )
  }
  if (identical(species_code, "626")) {
    notes <- c(
      notes,
      if (!french) {
        paste0(
          "In the 2025 assessment, it was recommended that if a 50% change in the ",
          "3-year running average of the coastwide geostatistical index relative to ",
          "the 2024 value was observed, an early reassessment of Dover Sole should be ",
          "brought forward for consideration. In 2025, this exceptional circumstance ",
          "criterion was **not** met."
        )
      } else {
        paste0(
          "Dans l’évaluation de 2025, il a été recommandé que, si une variation de ",
          "50 % de la moyenne mobile sur 3 ans de l’indice géostatistique à l’échelle ",
          "de la côte par rapport à la valeur de 2024 était observée, une réévaluation ",
          "anticipée de la limande-sole du Pacifique soit proposée pour examen. En ",
          "2025, ce critère de circonstances exceptionnelles n’a **pas** été rempli."
        )
      }
    )
  }
  if (identical(field("family"), "Salmonidae")) {
    notes <- c(notes, if (!french) {
      paste0(
        "Only incidental Pacific salmon data from the groundfish databases are shown ",
        "here; commercial catch is limited to incidental catch in groundfish fisheries. ",
        "Consult the latest relevant CSAS reports for details on Pacific salmon data. ",
        "For other salmon data, see the [Pacific Region ",
        "commercial salmon fishery in-season catch estimates](https://open.canada.ca/",
        "data/dataset/7ac5fe02-308d-4fff-b805-80194f8ddeb4), [NuSEDS (New Salmon ",
        "Escapement Database System)](https://open.canada.ca/data/en/dataset/",
        "c48669a3-045b-400d-b730-48aafe8c5ee6), and [Salmon Space](https://www.pac.",
        "dfo-mpo.gc.ca/science/smon-space-espace/index-eng.html). Salmon identification ",
        "and catch counts in the groundfish data shown here are not representative for ",
        "the period of September 2022 to February 2024; see [Lagasse et al. ",
        "(2024)](https://waves-vagues.dfo-mpo.gc.ca/",
        "library-bibliotheque/41221618.pdf)."
      )
    } else {
      paste0(
        "Seules les données accessoires sur le saumon du Pacifique provenant des bases ",
        "de données sur les poissons de fond sont présentées ici; les prises commerciales ",
        "se limitent aux prises accessoires dans les pêches de poissons de fond. ",
        "Consultez les derniers rapports pertinents du SCCS pour obtenir des détails sur ",
        "les données concernant le saumon du Pacifique. Pour obtenir d’autres données ",
        "sur le saumon, consultez les [estimations des prises en saison de la pêche ",
        "commerciale du saumon dans la région du Pacifique](https://ouvert.canada.ca/",
        "data/dataset/7ac5fe02-308d-4fff-b805-80194f8ddeb4), le [NuSEDS (Nouveau ",
        "système de base de données sur les échappées de saumon)](https://open.canada.ca/",
        "data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6) et [Salmon Space](https://",
        "www.pac.dfo-mpo.gc.ca/science/smon-space-espace/index-eng.html). L’identification ",
        "et les dénombrements des saumons dans les données sur les poissons de fond ",
        "présentées ici ne sont pas représentatifs pour la période de septembre 2022 à ",
        "février 2024; voir [Lagasse et al. (2024)]",
        "(https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/41221618.pdf)."
      )
    })
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
