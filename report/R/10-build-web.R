# Build the data and assets for the interactive species website.

source(here::here("report", "web", "trim-silhouettes.R"))

resolve_phylopic_silhouettes <- function(pages, asset_dir) {
  rphylopic_available <- requireNamespace("rphylopic", quietly = TRUE)
  dir.create(asset_dir, recursive = TRUE, showWarnings = FALSE)
  local_silhouettes <- c(
    "arrowtooth-flounder" = "WDFW",
    "bocaccio" = "WDFW",
    "canary-rockfish" = "WDFW",
    "chum-salmon" = "NOAA",
    "coho-salmon" = "WDFW",
    "copper-rockfish" = "WDFW",
    "pacific-cod" = "NOAA",
    "north-pacific-spiny-dogfish" = "WDFW",
    "petrale-sole" = "WDFW",
    "pacific-halibut" = "WDFW",
    "pacific-herring" = "Bulletin of the United States Fish Commission",
    "quillback-rockfish" = "WDFW",
    "salmon-shark" = "Shorefishes of the Eastern Pacific online information system",
    "shortraker-rockfish" = "WDFW",
    "yelloweye-rockfish" = "NOAA"
  )
  local_silhouette_dir <- file.path(asset_dir, "inkscape")
  local_records <- list()
  cache_file <- file.path(asset_dir, "phylopic-cache.json")
  cache <- if (file.exists(cache_file)) {
    tryCatch(
      jsonlite::read_json(cache_file, simplifyVector = FALSE),
      error = function(...) list()
    )
  } else {
    list()
  }

  license_url <- function(value) {
    if (is.null(value) || !length(value) || is.na(value[[1L]])) {
      NA_character_
    } else {
      as.character(value[[1L]])
    }
  }

  silhouette_label <- function(level, matched_name) {
    if (identical(level, "species")) {
      matched_name
    } else {
      paste(sub("[[:space:]].*$", "", matched_name), "sp.")
    }
  }

  failures <- character()
  for (page in pages) {
    local_file <- paste0(page$slug, ".svg")
    local_path <- file.path(local_silhouette_dir, local_file)
    if (file.exists(local_path)) {
      credit <- unname(local_silhouettes[[page$slug]])
      if (is.null(credit)) {
        stop(
          "Missing attribution for local silhouette: ", local_path,
          call. = FALSE
        )
      }
      local_records[[page$slug]] <- list(
        available = TRUE,
        file = file.path("inkscape", local_file),
        scientific_name = page$scientific_name,
        matched_name = page$scientific_name,
        level = "species",
        label = page$scientific_name,
        alt = paste("Silhouette for", page$common_name),
        credit = paste("Original image:", credit),
        source_url = NULL,
        license = NULL,
        license_url = NULL
      )
      next
    }
    genus <- sub("[[:space:]].*$", "", page$scientific_name)
    cached <- cache[[page$slug]]
    cached_file <- if (!is.null(cached)) cached$file else NULL
    cached_match <- if (!is.null(cached$matched_name)) {
      cached$matched_name
    } else {
      page$scientific_name
    }
    cached_ok <- !is.null(cached) &&
      isTRUE(cached$available) &&
      identical(cached$scientific_name, page$scientific_name) &&
      cached_match %in% c(page$scientific_name, genus) &&
      length(cached_file) == 1L &&
      file.exists(file.path(asset_dir, cached_file))
    if (cached_ok) {
      if (is.null(cached$matched_name)) {
        cached$matched_name <- page$scientific_name
      }
      if (is.null(cached$level)) {
        cached$level <- if (identical(cached$matched_name, page$scientific_name)) {
          "species"
        } else {
          "genus"
        }
      }
      cached$label <- silhouette_label(cached$level, cached$matched_name)
      cache[[page$slug]] <- cached
      next
    }
    cached_failure <- !is.null(cached) &&
      isFALSE(cached$available) &&
      identical(cached$scientific_name, page$scientific_name)
    if (cached_failure) next

    if (!rphylopic_available) {
      stop(
        "The rphylopic package is required to resolve uncached silhouettes. ",
        "Install it with install.packages('rphylopic').",
        call. = FALSE
      )
    }

    asset_name <- paste0(page$slug, ".svg")
    asset_path <- file.path(asset_dir, asset_name)
    record <- tryCatch({
      lookup_names <- unique(c(
        page$scientific_name,
        genus
      ))
      uuid_url <- NULL
      matched_name <- NULL
      for (lookup_name in lookup_names) {
        uuid_url <- tryCatch(
          rphylopic::get_uuid(name = lookup_name, n = 1, url = TRUE),
          error = function(...) NULL
        )
        if (length(uuid_url)) {
          matched_name <- lookup_name
          break
        }
      }
      if (!length(uuid_url)) stop("no silhouette returned")
      level <- if (identical(matched_name, page$scientific_name)) {
        "species"
      } else {
        "genus"
      }
      uuid <- names(uuid_url)[[1L]]
      vector_url <- unname(uuid_url)[[1L]]
      if (!nzchar(uuid) || !nzchar(vector_url)) {
        stop("the silhouette response was incomplete")
      }
      if (!file.exists(asset_path)) {
        utils::download.file(
          vector_url,
          asset_path,
          mode = "wb",
          quiet = TRUE
        )
      }
      attribution <- rphylopic::get_attribution(uuid)$images[[uuid]]
      credit <- attribution$attribution
      if (is.null(credit) || !length(credit) || !nzchar(credit)) {
        credit <- attribution$contributor
      }
      list(
        available = TRUE,
        file = asset_name,
        scientific_name = page$scientific_name,
        matched_name = matched_name,
        level = level,
        label = silhouette_label(level, matched_name),
        alt = if (identical(level, "species")) {
          paste("Species-level PhyloPic silhouette for", page$common_name)
        } else {
          paste("Genus-level PhyloPic silhouette for", page$common_name)
        },
        credit = as.character(credit),
        source_url = paste0("https://www.phylopic.org/images/", uuid),
        license = as.character(attribution$license_abbr),
        license_url = license_url(attribution$license)
      )
    }, error = function(error) {
      failures <<- c(failures, paste0(page$scientific_name, ": ", error$message))
      NULL
    })
    if (is.null(record)) {
      unlink(asset_path)
      cache[[page$slug]] <- list(
        available = FALSE,
        scientific_name = page$scientific_name
      )
    } else {
      cache[[page$slug]] <- record
    }
    jsonlite::write_json(
      cache,
      cache_file,
      auto_unbox = TRUE,
      pretty = TRUE,
      na = "null",
      null = "null"
    )
  }

  jsonlite::write_json(
    cache,
    cache_file,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null",
    null = "null"
  )
  trim_silhouette_directory(asset_dir, quiet = TRUE)
  if (length(failures)) {
    message(
      "PhyloPic silhouettes unavailable for ", length(failures),
      " species; continuing without those images."
    )
  }

  lapply(pages, function(page) {
    record <- local_records[[page$slug]]
    if (is.null(record)) record <- cache[[page$slug]]
    if (!is.null(record) && isTRUE(record$available)) {
      if (is.null(record$matched_name)) {
        record$matched_name <- page$scientific_name
      }
      if (is.null(record$level)) {
        record$level <- if (identical(record$matched_name, page$scientific_name)) {
          "species"
        } else {
          "genus"
        }
      }
      if (is.null(record$label)) {
        record$label <- silhouette_label(record$level, record$matched_name)
      }
      page$silhouette <- list(
        image = file.path("assets", record$file),
        matched_name = record$matched_name,
        level = record$level,
        label = record$label,
        alt = record$alt,
        credit = record$credit,
        source_url = record$source_url,
        license = record$license,
        license_url = record$license_url
      )
    } else {
      page$silhouette <- NULL
    }
    page
  })
}

build_web_species_pages <- function(
    spp,
    figure_dir,
    french_figure_dir = here::here(
      "report", "tech-report-fr-main", "figure-pages"
    ),
    web_dir = here::here("report", "web"),
    edition = NA_character_,
    ext = "png",
    bibliography_file = here::here(
      "report", "tech-report-main", "bib", "spp-refs.bib"
    ),
    legacy_bibliography_file = here::here(
      "report", "report-rmd", "bib", "spp-refs.bib"
    )) {
  pages <- gfsynopsis:::species_pages_data(
    spp,
    french = FALSE,
    ext = ext
  )
  french_pages <- gfsynopsis:::species_pages_data(
    spp,
    french = TRUE,
    ext = ext
  )
  web_dir <- normalizePath(web_dir, mustWork = TRUE)
  pages <- resolve_phylopic_silhouettes(
    pages,
    asset_dir = file.path(web_dir, "assets")
  )
  if (!file.exists(bibliography_file)) {
    stop("Missing web bibliography: ", bibliography_file, call. = FALSE)
  }
  bibliography <- RefManageR::ReadBib(bibliography_file, check = FALSE)
  reference_urls <- gfsynopsis:::web_reference_urls(
    bibliography_file,
    legacy_bibliography_file = legacy_bibliography_file
  )

  scalar_value <- function(x) {
    length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  }
  french_sentence_case <- function(x) {
    x <- tolower(x)
    proper_names <- c(
      "pacifique" = "Pacifique",
      "alaska" = "Alaska",
      "amérique" = "Amérique",
      "chili" = "Chili",
      "californie" = "Californie",
      "japon" = "Japon",
      "goode" = "Goode",
      "blackgill" = "Blackgill",
      "deacon" = "Deacon",
      "c-o" = "C-O"
    )
    for (name in names(proper_names)) {
      x <- gsub(name, proper_names[[name]], x, fixed = TRUE)
    }
    x <- gsub("Pacifique nord", "Pacifique Nord", x, fixed = TRUE)
    x <- gsub("Pacifique sud", "Pacifique Sud", x, fixed = TRUE)
    substr(x, 1L, 1L) <- toupper(substr(x, 1L, 1L))
    x
  }
  required_fields <- c(
    "slug", "common_name", "scientific_name", "species_code", "order", "family"
  )
  for (page in pages) {
    missing_fields <- required_fields[!vapply(
      page[required_fields], scalar_value, logical(1)
    )]
    if (length(missing_fields)) {
      stop(
        "Missing required metadata for ", page$slug, ": ",
        paste(missing_fields, collapse = ", "),
        call. = FALSE
      )
    }
    if (length(page$images) != 2L) {
      stop("Expected exactly two images for ", page$slug, call. = FALSE)
    }
  }
  french_common_names <- vapply(
    french_pages, `[[`, character(1), "common_name"
  )
  if (any(is.na(french_common_names) | !nzchar(trimws(french_common_names)))) {
    stop("Missing French common name in web metadata.", call. = FALSE)
  }
  if (!identical(
    vapply(pages, `[[`, character(1), "slug"),
    vapply(french_pages, `[[`, character(1), "slug")
  )) {
    stop("English and French species pages do not align.", call. = FALSE)
  }

  slugs <- vapply(pages, `[[`, character(1), "slug")
  species_codes <- vapply(pages, `[[`, character(1), "species_code")
  if (anyDuplicated(slugs)) {
    stop("Duplicate species slug: ", slugs[duplicated(slugs)][[1L]], call. = FALSE)
  }
  if (anyDuplicated(species_codes)) {
    stop(
      "Duplicate species code: ",
      species_codes[duplicated(species_codes)][[1L]],
      call. = FALSE
    )
  }

  english_source_images <- unlist(lapply(
    pages,
    function(page) file.path(figure_dir, basename(page$images))
  ), use.names = FALSE)
  french_source_images <- unlist(lapply(
    pages,
    function(page) file.path(french_figure_dir, basename(page$images))
  ), use.names = FALSE)
  if (anyDuplicated(english_source_images) || anyDuplicated(french_source_images)) {
    stop("The same image is assigned to more than one species.", call. = FALSE)
  }
  missing_images <- c(
    english_source_images[!file.exists(english_source_images)],
    french_source_images[!file.exists(french_source_images)]
  )
  if (length(missing_images)) {
    stop(
      "Missing species image: ", missing_images[[1L]],
      if (length(missing_images) > 1L) {
        paste0(" (and ", length(missing_images) - 1L, " more)")
      } else {
        ""
      },
      call. = FALSE
    )
  }
  image_sizes <- file.info(c(english_source_images, french_source_images))$size
  if (any(is.na(image_sizes) | image_sizes <= 0)) {
    stop("One or more species images are empty or unreadable.", call. = FALSE)
  }

  display_fields <- unlist(lapply(
    pages,
    function(page) unlist(page[c(
      "common_name", "scientific_name", "species_code", "order", "family",
      "cosewic_status", "sara_status"
    )], use.names = FALSE)
  ), use.names = FALSE)
  display_fields <- display_fields[!is.na(display_fields)]
  if (any(grepl("@[[:alnum:]_-]+", display_fields))) {
    stop("Raw citation syntax found in a basic display field.", call. = FALSE)
  }

  optional_value <- function(x) {
    if (!scalar_value(x)) NA_character_ else x
  }
  web_pages <- Map(pages, french_pages, f = function(page, french_page) {
    page$links <- lapply(page$links, function(link) {
      link$url <- sub("^http://", "https://", link$url)
      link
    })
    page$references <- gfsynopsis:::web_reference_records(
      page,
      bibliography = bibliography,
      urls = reference_urls
    )
    page$notes <- gfsynopsis:::web_resolve_note_citations(
      page$notes,
      reference_records = page$references,
      bibliography = bibliography,
      urls = reference_urls
    )
    french_notes <- gfsynopsis:::web_resolve_note_citations(
      french_page$notes,
      reference_records = page$references,
      bibliography = bibliography,
      urls = reference_urls
    )
    page$references <- lapply(page$references, function(reference) {
      reference$key <- NULL
      reference
    })
    page$cosewic_status <- optional_value(page$cosewic_status)
    page$sara_status <- optional_value(page$sara_status)
    image_names <- basename(page$images)
    page$images <- list(
      en = file.path("figures", "en", image_names),
      fr = file.path("figures", "fr", image_names)
    )
    page$translations <- list(fr = list(
      common_name = french_sentence_case(french_page$common_name),
      notes = french_notes
    ))
    page
  })

  web_image_paths <- unlist(lapply(web_pages, `[[`, "images"), use.names = FALSE)
  if (any(grepl("^/|^[A-Za-z]:[/\\\\]", web_image_paths))) {
    stop("Absolute image path found in web metadata.", call. = FALSE)
  }

  web_silhouette_paths <- unlist(lapply(web_pages, function(page) {
    if (is.null(page$silhouette)) character() else page$silhouette$image
  }), use.names = FALSE)
  if (any(grepl("^/|^[A-Za-z]:[/\\\\]", web_silhouette_paths))) {
    stop("Absolute silhouette path found in web metadata.", call. = FALSE)
  }

  output_dir <- file.path(web_dir, "generated")
  if (!identical(dirname(output_dir), web_dir) ||
      !identical(basename(output_dir), "generated")) {
    stop("Refusing to clean an unexpected output directory.", call. = FALSE)
  }

  if (dir.exists(output_dir)) unlink(output_dir, recursive = TRUE)
  english_figure_output_dir <- file.path(output_dir, "figures", "en")
  french_figure_output_dir <- file.path(output_dir, "figures", "fr")
  silhouette_output_dir <- file.path(output_dir, "assets")
  dir.create(english_figure_output_dir, recursive = TRUE)
  dir.create(french_figure_output_dir, recursive = TRUE)
  dir.create(silhouette_output_dir, recursive = TRUE)

  frontend_files <- c("index.html", "app.css", "app.js", "_headers")
  frontend_files <- file.path(web_dir, frontend_files)
  frontend_files <- frontend_files[file.exists(frontend_files)]
  if (length(frontend_files) && !all(file.copy(frontend_files, output_dir))) {
    stop("Could not copy one or more frontend files.", call. = FALSE)
  }
  if (!all(file.copy(english_source_images, english_figure_output_dir)) ||
      !all(file.copy(french_source_images, french_figure_output_dir))) {
    stop("Could not copy one or more species images.", call. = FALSE)
  }
  if (length(web_silhouette_paths)) {
    silhouette_source_files <- file.path(web_dir, web_silhouette_paths)
    if (!all(file.exists(silhouette_source_files))) {
      stop(
        "Missing silhouette asset: ",
        silhouette_source_files[!file.exists(silhouette_source_files)][[1L]],
        call. = FALSE
      )
    }
    silhouette_output_files <- file.path(output_dir, web_silhouette_paths)
    for (i in seq_along(silhouette_source_files)) {
      dir.create(dirname(silhouette_output_files[[i]]), recursive = TRUE,
        showWarnings = FALSE)
      if (!file.copy(silhouette_source_files[[i]], silhouette_output_files[[i]],
          overwrite = TRUE)) {
        stop("Could not copy silhouette asset: ", silhouette_source_files[[i]],
          call. = FALSE)
      }
    }
  }
  dfo_logo <- file.path(web_dir, "assets", "dfo-logo.svg")
  if (!file.exists(dfo_logo) ||
      !file.copy(dfo_logo, silhouette_output_dir, overwrite = TRUE)) {
    stop("Missing DFO logo asset.", call. = FALSE)
  }

  output <- list(
    metadata = list(
      generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      edition = optional_value(edition),
      languages = c("en", "fr"),
      species_count = length(web_pages)
    ),
    species = web_pages
  )
  json_file <- file.path(output_dir, "species.json")
  jsonlite::write_json(
    output,
    json_file,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null",
    null = "null"
  )

  repository_path <- normalizePath(here::here(), mustWork = TRUE)
  json_text <- paste(readLines(json_file, warn = FALSE), collapse = "\n")
  if (grepl(repository_path, json_text, fixed = TRUE)) {
    stop("Local repository path found in generated JSON.", call. = FALSE)
  }
  if (grepl("@[[:alnum:]_-]+", json_text)) {
    stop("Raw citation syntax found in generated web output.", call. = FALSE)
  }

  size_mb <- sum(image_sizes) / 1024^2
  message(
    "Built web data: ", length(web_pages), " species, ",
    length(english_source_images), " English and ", length(french_source_images),
    " French images, ", sprintf("%.1f MB", size_mb), "\n",
    "Output: ", output_dir
  )
  invisible(output_dir)
}

load_web_species_metadata <- function() {
  spp <- gfsynopsis::get_spp_names() |>
    dplyr::select(
      species_common_name, species_code, species_science_name, spp_w_hyphens,
      type, itis_tsn, worms_id
    ) |>
    dplyr::filter(species_common_name != "herrings") |>
    dplyr::arrange(species_common_name)
  spp <- gfsynopsis::join_worms_spp(spp, check_cache = TRUE)
  spp <- dplyr::left_join(
    spp,
    gfsynopsis::get_cosewic_data(),
    by = "species_science_name"
  )
  gfsynopsis::join_refs_spp(spp, french = FALSE)
}

if (!requireNamespace("here", quietly = TRUE)) {
  stop("The here package is required to build the web data.", call. = FALSE)
}
if (!"gfsynopsis" %in% loadedNamespaces()) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("The devtools package is required when running this script directly.")
  }
  devtools::load_all(here::here(), quiet = TRUE)
}
if (!exists("spp", inherits = FALSE)) spp <- load_web_species_metadata()

web_figure_dir <- if (exists("build_dir", inherits = FALSE)) {
  file.path(build_dir, "figure-pages")
} else {
  here::here("report", "tech-report-main", "figure-pages")
}
web_edition <- if (exists("final_year_comm", inherits = FALSE)) {
  as.character(final_year_comm)
} else {
  sub("^.*-([0-9]{4})$", "\\1", basename(here::here()))
}
web_ext <- if (exists("ext", inherits = FALSE)) ext else "png"

build_web_species_pages(
  spp = spp,
  figure_dir = web_figure_dir,
  edition = web_edition,
  ext = web_ext
)
