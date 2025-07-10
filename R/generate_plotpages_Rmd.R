# Generate `plot-pages.Rmd`:

#' Generate plot-pages.Rmd for a species
#'
#' @param x A species common name
#' @param spp Output from [get_spp_names()] that has been cleaned by other
#'   functions.
#'
#' @returns Rmd code in a character vector.
#' @export
generate_plotpages_Rmd <- function(x, spp) {
  message(x)
  spp_file <- clean_name(x)
  if (french) {
    spp_title <- spp$species_french_name[spp$species_common_name == x]
  } else {
    spp_title <- stringr::str_to_title(x)
  }
  if (spp_title == "North Pacific Spiny Dogfish") {
    spp_title <- "Pacific Spiny Dogfish"
  }
  if (spp_title == "Popeye") {
    spp_title <- "Popeye grenadier"
  }
  spp_hyphen <- spp$spp_w_hyphens[spp$species_common_name == x]
  out <- list()
  latin_name <- spp$species_science_name[spp$species_common_name == x]
  sar <- spp$sar[spp$species_common_name == x]
  resdoc <- spp$resdoc[spp$species_common_name == x]
  species_code <- spp$species_code[spp$species_common_name == x]
  other_ref <- spp$other_ref_cite[spp$species_common_name == x]
  sara_status <- spp$sara_status[spp$species_common_name == x]
  cosewic_status <- spp$cosewic_status[spp$species_common_name == x]
  cosewic_report <- spp$cosewic_status_reports[spp$species_common_name == x]
  worms_id <- spp$worms_id[spp$species_common_name == x]

  resdoc_text <- if (grepl(",", resdoc)) {
    paste0(en2fr("Last Research Document", french), "s: ")
  } else {
    paste0(en2fr("Last Research Document", french), ": ")
  }
  sar_text <- if (grepl(",", sar)) {
    paste0(en2fr("Last Science Advisory Report", french), "s: ")
  } else {
    paste0(en2fr("Last Science Advisory Report", french), ": ")
  }

  i <- 1
  out[[i]] <- "\\clearpage\n"
  i <- i + 1
  out[[i]] <- paste0("## ", spp_title, " {#sec:", spp_hyphen, "}\n")
  i <- i + 1
  out[[i]] <- paste0(
    emph(latin_name), " (", species_code, ")", "\\\n",
    en2fr("Order", french), ": ", spp$order[spp$species_common_name == x], ", ",
    en2fr("Family", french), ": ", spp$family[spp$species_common_name == x],
    ","
  )
  i <- i + 1
  out[[i]] <- paste0(
    "[FishBase]",
    "(http://www.fishbase.org/summary/",
    gsub(" ", "-", gfplot:::firstup(latin_name)), ")"
  )
  if (species_code == "394") { # Sebastes aleutianus/melanostictus
    .names <- rougheye_split(gfplot:::firstup(latin_name))
    out[[i]] <- paste0(
      "[FishBase 1]",
      "(http://www.fishbase.org/summary/", .names[1], "),"
    )
    i <- i + 1
    out[[i]] <- paste0(
      "[FishBase 2]",
      "(http://www.fishbase.org/summary/", .names[2], ")"
    )
  }
  if (species_code == "039") { # Requiem Sharks
    out[[i]] <- paste0(
      "[FishBase]",
      "(http://www.fishbase.org/Summary/FamilySummary.php?ID=11)"
    )
  }

  if (!is.na(worms_id)) {
    out[[i]] <- paste0(out[[i]], ", ")
    i <- i + 1
    out[[i]] <- paste0(
      "[WoRMS]",
      "(http://www.marinespecies.org/aphia.php?p=taxdetails&id=",
      worms_id, ")"
    )
  }
  out[[i]] <- paste0(out[[i]], "\\")
  if (resdoc != "") {
    i <- i + 1
    out[[i]] <- paste0(resdoc_text, resdoc, "\\")
  }
  if (sar != "") {
    i <- i + 1
    out[[i]] <- paste0(sar_text, sar, "\\")
  }
  i <- i + 1

  if (!is.na(other_ref)) {
    if (other_ref != "") {
      out[[i]] <- paste0(other_ref, "\\")
      if (!is.na(cosewic_status) && cosewic_status != "") {
        # out[[i]] <- paste0(out[[i]], "\\")
      }
      if (french) {
        out[[i]] <- gsub("Last joint Canada-US stock assessment:", "Dernière évaluation conjointe des stocks Canada-États-Unis :", out[[i]])
        out[[i]] <- gsub("Last Science Response:", "Réponse des sciences :", out[[i]])
        out[[i]] <- gsub("IPHC Report of Assessment and Research Activities:", "Rapport des activités d'évaluation et de recherche de l'CIFP :", out[[i]])
        out[[i]] <- gsub("Technical Report:", "Rapport technique :", out[[i]])
        out[[i]] <- gsub("Species at Risk Act Management Plan Series:", "Série de plans de gestion de la Loi sur les espèces en péril :", out[[i]])
      }
      if (species_code == "034" && french) {
        out[[i]] <- "Stratégie et plan d'action pour le rétablissement de la Loi sur les espèces en péril : @dfo2011baskingshark, @cosewic2020baskingshark"
      }
      i <- i + 1
    }
  }
  if (!is.na(cosewic_report)) {
    if (cosewic_report != "") {
      out[[i]] <- paste0(en2fr("COSEWIC Status Report", french), ": @", cosewic_report, "\\")
      i <- i + 1
    }
  }
  if (!is.na(cosewic_status)) {
    if (cosewic_status != "") {
      out[[i]] <- paste0(en2fr("COSEWIC Status", french), ": ", en2fr(cosewic_status, french))
      if (!is.na(sara_status)) {
        if (sara_status != "") {
          out[[i]] <- paste0(out[[i]], ", ", en2fr("SARA Status", french), ": ", en2fr(sara_status, french))
        }
      }
      out[[i]] <- paste0(out[[i]], "\n")
      i <- i + 1
    }
  }
  # if (species_code == "610" && french) {
  #   out[[i - 1]] <- "Document de recherche présentant une étude de cas de 3CD Rex Sole : @anderson2021mp"
  # }
  # if (species_code == "394") {
  #   if (!french) {
  #     out[[i]] <- paste0(en2fr("COSEWIC Status", french), ": ", en2fr("Special Concern", french), ", ", en2fr("SARA Status",french), ": ",  en2fr("Special Concern", french), "\n")
  #   } else {
  #     out[[i]] <- paste0(en2fr("COSEWIC Status", french), ":", en2fr("Special Concern", french), ", ", en2fr("SARA Status", french), ":", en2fr("Special Concern"), "\n")
  #   }
  #   i <- i + 1
  # }
  if (species_code == "225") {
    if (!french) {
      out[[i]] <- "Note that Pacific Hake undergoes a directed joint
      Canada-US coastwide\n acoustic survey and annual assessment, which are not
      included in this report. The most recent\n stock assessment
      should be consulted for details on stock status."
    } else {
      out[[i]] <- "Il est à noter que le merlu du Chili fait l’objet d’un relevé et d’une évaluation annuels ciblés menés conjointement par le Canada et les É.-U. à l'échelle de la côte, qui ne sont pas compris dans le présent rapport. L’évaluation la plus récente des stocks doit être consultée pour obtenir des détails sur l’état des stocks."
    }
    i <- i + 1
  }
  if (species_code == "614") {
    if (!french) {
      out[[i]] <- "Note that Pacific Halibut undergoes thorough assessment by the
      International Pacific\n Halibut Commission based on [the annual
      standardized setline survey](https://www.iphc.int/research/fishery-independent-monitoring/). The most\n recent [stock assessment](https://www.iphc.int/research/stock-assessment/)
      should be consulted for details on stock status."
    } else {
      out[[i]] <- "Il est à noter que le flétan du Pacifique fait l’objet d’une évaluation approfondie par la Commission internationale du flétan du Pacifique qui se fonde sur un relevé annuel normalisé en fonction de la ligne de référence. L’évaluation la plus récente des stocks doit être consultée pour obtenir des détails sur l’état des stocks."
    }
    i <- i + 1
  }
  if (species_code == "455") {
    if (!french) {
      out[[i]] <- "The annual sablefish trap survey is not included in this report. Commercial biological samples from a head-only sampling program that began in 2018 [@lacko2023] are not shown."
    } else {
      out[[i]] <- "Il est à noter que la morue charbonnière fait l’objet de relevés annuels au casier ciblés qui servent à l’évaluation des stocks et qui ne sont pas compris dans le présent rapport. L’évaluation la plus récente des stocks doit être consultée pour obtenir des détails sur l’état des stocks."
    }
    i <- i + 1
  }
  out[[i]] <- "\\begin{figure}[b!]"
  i <- i + 1
  out[[i]] <- paste0(
    "\\includegraphics[width=6.4in]{figure-pages/",
    spp_file, "-1.", ext, "}"
  )
  i <- i + 1
  out[[i]] <- "\\end{figure}"
  i <- i + 1
  out[[i]] <- "\\clearpage"
  i <- i + 1
  out[[i]] <- "\\begin{figure}[b!]"
  i <- i + 1
  out[[i]] <- paste0(
    "\\includegraphics[width=6.4in]{figure-pages/",
    spp_file, "-2.", ext, "}"
  )
  i <- i + 1
  out[[i]] <- "\\end{figure}\n"
  i <- i + 1
  out[[i]] <- "\n"
  out
}
