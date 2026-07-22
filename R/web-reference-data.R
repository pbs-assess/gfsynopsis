# Helpers for converting report bibliography keys into web-ready references.

web_reference_scalar <- function(x) {
  length(x) == 1L && !is.na(x) && nzchar(trimws(x))
}

web_clean_bib_text <- function(x) {
  if (!web_reference_scalar(x)) return("")
  x <- as.character(x)
  # Keep the one piece of BibTeX formatting that is meaningful in the web
  # citation. Other commands are still reduced to their displayed text below.
  x <- gsub("\\\\emph\\{([^{}]*)\\}", "<em>\\1</em>", x)
  repeat {
    cleaned <- gsub("\\\\[[:alpha:]]+\\{([^{}]*)\\}", "\\1", x)
    if (identical(cleaned, x)) break
    x <- cleaned
  }
  x <- gsub("\\\\[[:alpha:]]+", "", x)
  x <- gsub("[{}]", "", x)
  x <- gsub("~+", " ", x)
  x <- gsub("--", "–", x, fixed = TRUE)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

web_bib_field <- function(entry, field, default = "") {
  value <- entry[[field]]
  if (is.null(value) || !length(value)) return(default)
  web_clean_bib_text(value[[1L]])
}

web_bib_author_text <- function(entry) {
  if (is.null(entry$author)) return("")
  authors <- trimws(format(entry$author))
  authors <- authors[nzchar(authors)]
  if (!length(authors)) return("")
  if (length(authors) > 3L) {
    authors <- c(authors[seq_len(3L)], "et al.")
  }
  paste(authors, collapse = ", ")
}

web_bib_entry <- function(bibliography, key) {
  if (!key %in% names(bibliography)) return(NULL)
  unclass(bibliography[[key]])[[1L]]
}

web_bib_entry_label <- function(entry) {
  authors <- web_bib_author_text(entry)
  year <- web_bib_field(entry, "year")
  if (grepl(", et al\\.$", authors)) {
    authors <- sub(", et al\\.$", " et al.", authors)
  }
  if (!nzchar(authors)) authors <- "Reference"
  paste0(authors, if (nzchar(year)) paste0(" (", year, ")") else "")
}

web_bib_entry_citation <- function(entry) {
  authors <- web_bib_author_text(entry)
  year <- web_bib_field(entry, "year")
  title <- web_bib_field(entry, "title")
  journal <- web_bib_field(entry, "journal")
  booktitle <- web_bib_field(entry, "booktitle")
  volume <- web_bib_field(entry, "volume")
  pages <- web_bib_field(entry, "pages")
  note <- web_bib_field(entry, "note")
  source <- paste(c(journal, booktitle)[nzchar(c(journal, booktitle))], collapse = ". ")
  if (nzchar(volume)) source <- paste(source, volume)
  if (nzchar(pages)) source <- paste0(source, if (nzchar(source)) ": " else "", pages)
  title <- sub("[.]+$", "", title)
  source <- sub("[.]+$", "", source)
  note <- sub("[.]+$", "", note)

  pieces <- character()
  if (nzchar(authors)) pieces <- c(pieces, authors)
  if (nzchar(year)) pieces <- c(pieces, paste0("(", year, ")"))
  if (nzchar(title)) pieces <- c(pieces, paste0(title, "."))
  if (nzchar(source)) pieces <- c(pieces, paste0(source, "."))
  if (nzchar(note) && !grepl(note, source, fixed = TRUE)) {
    pieces <- c(pieces, paste0(note, "."))
  }
  trimws(paste(pieces, collapse = " "))
}

# Read URLs retained in the older report bibliography. The current report
# bibliography is refreshed from gfbib with URL fields removed, but this older
# tracked copy retains useful document links as comments beside their entries.
web_reference_comment_urls <- function(path) {
  if (!file.exists(path)) return(character())
  lines <- readLines(path, warn = FALSE)
  current_key <- NA_character_
  urls <- character()
  for (line in lines) {
    key_match <- regexec("^@[^\\{]*\\{[[:space:]]*([^,[:space:]]+),", line)
    key_parts <- regmatches(line, key_match)[[1L]]
    if (length(key_parts)) current_key <- key_parts[[2L]]
    url_match <- regmatches(
      line,
      gregexpr("https://[^[:space:]}]+", line, perl = TRUE)
    )[[1L]]
    if (!length(url_match) || identical(url_match, "-1") || is.na(current_key)) {
      next
    }
    urls[current_key] <- url_match[[1L]]
  }
  urls
}

web_reference_url_overrides <- function() {
  c(
    # Canadian technical report 3580 (the document cited in the Sablefish note).
    lacko2023 = "https://publications.gc.ca/collections/collection_2023/mpo-dfo/Fs97-6-3580-eng.pdf",
    # The IPHC landing page lists the annual assessment cited by the report.
    iphc2025 = "https://www.iphc.int/research/stock-assessment/"
  )
}

web_derived_csas_url <- function(entry) {
  journal <- web_bib_field(entry, "journal")
  volume <- web_bib_field(entry, "volume")
  volume_match <- regexec("^([0-9]{4})/([0-9]+)$", volume)
  volume_parts <- regmatches(volume, volume_match)[[1L]]
  if (!length(volume_parts)) return(NA_character_)

  path <- if (grepl("Res\\. Doc\\.", journal, fixed = FALSE)) {
    "ResDocs-DocRech"
  } else if (grepl("Sci\\. Advis\\. Rep\\.", journal, fixed = FALSE)) {
    "SAR-AS"
  } else if (grepl("Sci\\. Resp\\.", journal, fixed = FALSE)) {
    "ScR-RS"
  } else {
    return(NA_character_)
  }
  paste0(
    "https://www.dfo-mpo.gc.ca/csas-sccs/Publications/", path, "/",
    volume_parts[[2L]], "/", gsub("/", "_", volume), "-eng.html"
  )
}

web_reference_urls <- function(bibliography_file, legacy_bibliography_file = NULL) {
  bibliography <- RefManageR::ReadBib(bibliography_file, check = FALSE)
  urls <- character()
  if (!is.null(legacy_bibliography_file)) {
    urls <- web_reference_comment_urls(legacy_bibliography_file)
  }
  for (key in names(bibliography)) {
    entry <- web_bib_entry(bibliography, key)
    derived_url <- web_derived_csas_url(entry)
    if (!is.na(derived_url) && !key %in% names(urls)) {
      urls[[key]] <- derived_url
    }
  }
  overrides <- web_reference_url_overrides()
  urls[names(overrides)] <- overrides
  urls
}

web_reference_keys <- function(value, at_required = FALSE) {
  if (!web_reference_scalar(value)) return(character())
  if (at_required) {
    matches <- regmatches(
      value,
      gregexpr("@[A-Za-z][A-Za-z0-9_-]*", value, perl = TRUE)
    )[[1L]]
    return(sub("^@", "", matches))
  }
  keys <- trimws(unlist(strsplit(value, ",", fixed = TRUE), use.names = FALSE))
  sub("^@", "", keys)
}

web_reference_group <- function(type, value) {
  if (!identical(type, "other") || !web_reference_scalar(value)) {
    return(switch(
      type,
      research_documents = "Research document",
      science_advisory_reports = "Science advisory report",
      cosewic_status_report = "COSEWIC status report",
      "Related document"
    ))
  }
  label <- trimws(sub("@.*$", "", value))
  label <- sub(":[[:space:]]*$", "", label)
  if (!nzchar(label)) "Related document" else label
}

web_reference_records <- function(page, bibliography, urls = character()) {
  reference_sources <- list(
    list(
      type = "research_documents",
      value = page$references$research_documents,
      at_required = FALSE
    ),
    list(
      type = "science_advisory_reports",
      value = page$references$science_advisory_reports,
      at_required = FALSE
    ),
    list(
      type = "other",
      value = page$references$other,
      at_required = TRUE
    ),
    list(
      type = "cosewic_status_report",
      value = page$references$cosewic_status_report,
      at_required = FALSE
    )
  )
  records <- list()
  unresolved <- character()
  for (source in reference_sources) {
    keys <- web_reference_keys(source$value, source$at_required)
    if (!length(keys)) next
    group <- web_reference_group(source$type, source$value)
    for (key in keys) {
      entry <- web_bib_entry(bibliography, key)
      if (is.null(entry)) {
        unresolved <- c(unresolved, key)
        next
      }
      url <- unname(urls[key])
      if (!length(url) || is.na(url) || !nzchar(url)) url <- NULL
      records[[length(records) + 1L]] <- list(
        type = source$type,
        group = group,
        key = key,
        label = web_bib_entry_label(entry),
        citation = web_bib_entry_citation(entry),
        url = url
      )
    }
  }
  if (length(unresolved)) {
    stop(
      "Unresolved bibliography key(s): ",
      paste(unique(unresolved), collapse = ", "),
      call. = FALSE
    )
  }
  unname(records)
}

web_resolve_note_citations <- function(
    notes,
    reference_records,
    bibliography,
    urls = character()) {
  if (!length(notes)) return(character())
  by_key <- setNames(reference_records, vapply(reference_records, `[[`, character(1), "key"))
  lapply(notes, function(note) {
    note <- trimws(gsub("[[:space:]]+", " ", note))
    matches <- regmatches(
      note,
      gregexpr("\\[@[A-Za-z][A-Za-z0-9_-]*\\]", note, perl = TRUE)
    )[[1L]]
    if (!length(matches) || identical(matches, "-1")) return(note)
    for (match in matches) {
      key <- sub("^\\[@", "", match)
      key <- sub("\\]$", "", key)
      record <- by_key[[key]]
      if (is.null(record)) {
        entry <- web_bib_entry(bibliography, key)
        if (is.null(entry)) {
          stop("Unresolved note bibliography key: ", key, call. = FALSE)
        }
        url <- unname(urls[key])
        if (!length(url) || is.na(url) || !nzchar(url)) url <- NULL
        record <- list(
          key = key,
          label = web_bib_entry_label(entry),
          citation = web_bib_entry_citation(entry),
          url = url
        )
      }
      replacement <- if (!is.null(record$url)) {
        paste0("[", record$label, "](", record$url, ")")
      } else {
        record$label
      }
      note <- sub(match, replacement, note, fixed = TRUE)
    }
    note
  }) |> unname()
}
