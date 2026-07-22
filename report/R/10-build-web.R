# Build the data and assets for the interactive species website.

build_web_species_pages <- function(
    spp,
    figure_dir,
    web_dir = here::here("report", "web"),
    edition = NA_character_,
    french = FALSE,
    ext = "png",
    bibliography_file = here::here(
      "report", "tech-report-main", "bib", "spp-refs.bib"
    ),
    legacy_bibliography_file = here::here(
      "report", "report-rmd", "bib", "spp-refs.bib"
    )) {
  if (french) {
    stop("The web build currently supports the English report only.", call. = FALSE)
  }

  pages <- gfsynopsis:::species_pages_data(
    spp,
    french = french,
    ext = ext
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

  source_images <- unlist(lapply(
    pages,
    function(page) file.path(figure_dir, basename(page$images))
  ), use.names = FALSE)
  if (anyDuplicated(source_images)) {
    stop("The same image is assigned to more than one species.", call. = FALSE)
  }
  missing_images <- source_images[!file.exists(source_images)]
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
  image_sizes <- file.info(source_images)$size
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
  web_pages <- lapply(pages, function(page) {
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
    page$references <- lapply(page$references, function(reference) {
      reference$key <- NULL
      reference
    })
    page$cosewic_status <- optional_value(page$cosewic_status)
    page$sara_status <- optional_value(page$sara_status)
    page$images <- paste0("figures/", basename(page$images))
    page
  })

  web_image_paths <- unlist(lapply(web_pages, `[[`, "images"), use.names = FALSE)
  if (any(grepl("^/|^[A-Za-z]:[/\\\\]", web_image_paths))) {
    stop("Absolute image path found in web metadata.", call. = FALSE)
  }

  web_dir <- normalizePath(web_dir, mustWork = TRUE)
  output_dir <- file.path(web_dir, "generated")
  if (!identical(dirname(output_dir), web_dir) ||
      !identical(basename(output_dir), "generated")) {
    stop("Refusing to clean an unexpected output directory.", call. = FALSE)
  }

  if (dir.exists(output_dir)) unlink(output_dir, recursive = TRUE)
  figure_output_dir <- file.path(output_dir, "figures")
  dir.create(figure_output_dir, recursive = TRUE)

  frontend_files <- c("index.html", "app.css", "app.js", "_headers")
  frontend_files <- file.path(web_dir, frontend_files)
  frontend_files <- frontend_files[file.exists(frontend_files)]
  if (length(frontend_files) && !all(file.copy(frontend_files, output_dir))) {
    stop("Could not copy one or more frontend files.", call. = FALSE)
  }
  if (!all(file.copy(source_images, figure_output_dir))) {
    stop("Could not copy one or more species images.", call. = FALSE)
  }

  output <- list(
    metadata = list(
      generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      edition = optional_value(edition),
      language = "en",
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
    length(source_images), " images, ", sprintf("%.1f MB", size_mb), "\n",
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
web_french <- if (exists("french", inherits = FALSE)) french else FALSE
web_ext <- if (exists("ext", inherits = FALSE)) ext else "png"

build_web_species_pages(
  spp = spp,
  figure_dir = web_figure_dir,
  edition = web_edition,
  french = web_french,
  ext = web_ext
)
