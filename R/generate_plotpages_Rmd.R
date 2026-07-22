# Generate `plot-pages.Rmd`:

#' Generate plot-pages.Rmd for a species
#'
#' @param x A species common name
#' @param spp Output from [get_spp_names()] that has been cleaned by other
#'   functions.
#' @param french Logical; generate French display text.
#' @param ext Image file extension.
#'
#' @returns Rmd code in a character vector.
#' @export
generate_plotpages_Rmd <- function(x, spp, french = FALSE, ext = "png") {
  message(x)
  page <- species_page_data(x, spp, french = french, ext = ext)
  spp_title <- page$common_name
  spp_hyphen <- page$slug
  out <- list()
  latin_name <- page$scientific_name
  sar <- page$references$science_advisory_reports
  resdoc <- page$references$research_documents
  species_code <- page$species_code
  other_ref <- page$references$other
  sara_status <- page$sara_status
  cosewic_status <- page$cosewic_status
  cosewic_report <- page$references$cosewic_status_report

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
    en2fr("Order", french), ": ", page$order, ", ",
    en2fr("Family", french), ": ", page$family,
    ","
  )
  for (link_number in seq_along(page$links)) {
    i <- i + 1
    link <- page$links[[link_number]]
    out[[i]] <- paste0("[", link$label, "](", link$url, ")")
    if (link_number < length(page$links)) {
      separator <- if (identical(
        page$links[[link_number + 1L]]$label, "WoRMS"
      )) ", " else ","
      out[[i]] <- paste0(out[[i]], separator)
    }
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
  for (note in page$notes) {
    out[[i]] <- note
    i <- i + 1
  }
  out[[i]] <- "\\begin{figure}[b!]"
  i <- i + 1
  out[[i]] <- paste0(
    "\\includegraphics[width=6.4in]{", page$images[[1L]], "}"
  )
  i <- i + 1
  out[[i]] <- "\\end{figure}"
  i <- i + 1
  out[[i]] <- "\\clearpage"
  i <- i + 1
  out[[i]] <- "\\begin{figure}[b!]"
  i <- i + 1
  out[[i]] <- paste0(
    "\\includegraphics[width=6.4in]{", page$images[[2L]], "}"
  )
  i <- i + 1
  out[[i]] <- "\\end{figure}\n"
  i <- i + 1
  out[[i]] <- "\n"
  out
}
