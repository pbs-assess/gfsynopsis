# Compare survey-set selection between the 2026 and 2025 data caches.
#
# The comparison is deliberately limited to synoptic and HBLL surveys. A
# survey set is identified by fishing_event_id, within survey abbreviation and
# year. Values in the other columns are not compared here. Use
# --species=pacific-cod (or a comma-separated list) to print the exact IDs for
# selected species; all species are compared by default. Survey-years that
# occur in only one cache are ignored (for example, 2025).

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
repo_dir <- if (length(script_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", script_arg[1])), ".."))
} else {
  normalizePath(".")
}

new_dir <- file.path(repo_dir, "report", "data-cache-2026-07", "survey-sets")
old_dir <- file.path(
  path.expand("~"), "src", "gfsynopsis-2024", "report", "data-cache-2025-03"
)
convert_file <- file.path(
  path.expand("~"), "src", "gfdata", "R", "convert-to-old-get-survey-sets.R"
)

if (!file.exists(convert_file)) {
  stop("Could not find the gfdata conversion helper at: ", convert_file)
}
source(convert_file)

survey_pattern <- "^(SYN|HBLL)"

new_files <- list.files(new_dir, pattern = "\\.rds$", full.names = TRUE)
old_files <- list.files(old_dir, pattern = "\\.rds$", full.names = TRUE)
new_names <- sub("\\.rds$", "", basename(new_files))
old_names <- sub("\\.rds$", "", basename(old_files))
names(new_files) <- new_names
names(old_files) <- old_names

new_species <- names(new_files)
old_species <- setdiff(names(old_files), c("bait-counts", "cpue-index-dat"))
common_species <- intersect(new_species, old_species)

species_arg <- grep("^--species=", commandArgs(), value = TRUE)
requested_species <- if (length(species_arg)) {
  strsplit(sub("^--species=", "", species_arg[1]), ",")[[1]]
} else {
  NULL
}

normalise_sets <- function(x) {
  # Apply the same column names and classes to both generations of data.
  convert_to_old_sets(x)
}

set_ids <- function(x) {
  ids <- as.character(x$fishing_event_id)
  if (anyNA(ids)) {
    stop("A survey-set data frame contains missing fishing_event_id values.")
  }
  unique(ids)
}

survey_rows <- function(x, survey, year) {
  x[x$survey_abbrev == survey & x$year == year, , drop = FALSE]
}

compare_species <- function(species) {
  message("Comparing ", species)
  new <- normalise_sets(readRDS(new_files[[species]]))
  old_object <- readRDS(old_files[[species]])
  if (!is.list(old_object) || !"survey_sets" %in% names(old_object) ||
      !is.data.frame(old_object$survey_sets)) {
    stop("The prior cache file has no survey_sets element: ", species)
  }
  old <- normalise_sets(old_object$survey_sets)

  new <- new[grepl(survey_pattern, new$survey_abbrev), , drop = FALSE]
  old <- old[grepl(survey_pattern, old$survey_abbrev), , drop = FALSE]

  groups <- unique(rbind(
    unique(new[c("survey_abbrev", "year")]),
    unique(old[c("survey_abbrev", "year")])
  ))
  groups <- groups[order(groups$survey_abbrev, groups$year), , drop = FALSE]

  if (!nrow(groups)) {
    return(list(
      species_summary = data.frame(
        species = species, new_rows = nrow(new), old_rows = nrow(old),
        new_sets = 0L, old_sets = 0L, common_sets = 0L,
        new_only = 0L, old_only = 0L,
        old_duplicate_rows = 0L,
        stringsAsFactors = FALSE
      ),
      by_survey_year = data.frame(), differences = data.frame()
    ))
  }

  by_survey_year <- lapply(seq_len(nrow(groups)), function(i) {
    survey <- groups$survey_abbrev[i]
    year <- groups$year[i]
    n <- survey_rows(new, survey, year)
    o <- survey_rows(old, survey, year)
    n_ids <- set_ids(n)
    o_ids <- set_ids(o)
    data.frame(
      species = species,
      survey_abbrev = survey,
      year = year,
      new_rows = nrow(n),
      old_rows = nrow(o),
      new_sets = length(n_ids),
      old_sets = length(o_ids),
      common_sets = length(intersect(n_ids, o_ids)),
      new_only = length(setdiff(n_ids, o_ids)),
      old_only = length(setdiff(o_ids, n_ids)),
      stringsAsFactors = FALSE
    )
  })
  by_survey_year <- do.call(rbind, by_survey_year)
  # A missing survey-year in one cache is a coverage difference, not a
  # selection difference. Compare only survey-years represented in both.
  by_survey_year <- by_survey_year[
    by_survey_year$new_sets > 0 & by_survey_year$old_sets > 0,
    , drop = FALSE
  ]

  if (!nrow(by_survey_year)) {
    return(list(
      species_summary = data.frame(
        species = species, new_rows = 0L, old_rows = 0L,
        new_sets = 0L, old_sets = 0L, common_sets = 0L,
        new_only = 0L, old_only = 0L, old_duplicate_rows = 0L,
        stringsAsFactors = FALSE
      ),
      by_survey_year = by_survey_year, differences = data.frame()
    ))
  }

  differences <- lapply(seq_len(nrow(by_survey_year)), function(i) {
    row <- by_survey_year[i, ]
    if (!row$new_only && !row$old_only) return(NULL)

    n <- survey_rows(new, row$survey_abbrev, row$year)
    o <- survey_rows(old, row$survey_abbrev, row$year)
    n_ids <- set_ids(n)
    o_ids <- set_ids(o)
    out <- list()

    if (row$new_only) {
      ids <- setdiff(n_ids, o_ids)
      z <- n[match(ids, as.character(n$fishing_event_id)), , drop = FALSE]
      out[[length(out) + 1L]] <- data.frame(
        species = species, source = "new_only",
        survey_abbrev = z$survey_abbrev, year = z$year,
        fishing_event_id = z$fishing_event_id, trip_id = z$trip_id,
        fe_major_level_id = z$fe_major_level_id,
        stringsAsFactors = FALSE
      )
    }

    if (row$old_only) {
      ids <- setdiff(o_ids, n_ids)
      z <- o[match(ids, as.character(o$fishing_event_id)), , drop = FALSE]
      out[[length(out) + 1L]] <- data.frame(
        species = species, source = "old_only",
        survey_abbrev = z$survey_abbrev, year = z$year,
        fishing_event_id = z$fishing_event_id, trip_id = z$trip_id,
        fe_major_level_id = z$fe_major_level_id,
        stringsAsFactors = FALSE
      )
    }
    do.call(rbind, out)
  })
  differences <- Filter(Negate(is.null), differences)
  differences <- if (length(differences)) {
    do.call(rbind, differences)
  } else {
    data.frame()
  }

  list(
    species_summary = data.frame(
      species = species,
      new_rows = sum(by_survey_year$new_rows),
      old_rows = sum(by_survey_year$old_rows),
      new_sets = sum(by_survey_year$new_sets),
      old_sets = sum(by_survey_year$old_sets),
      common_sets = sum(by_survey_year$common_sets),
      new_only = sum(by_survey_year$new_only),
      old_only = sum(by_survey_year$old_only),
      old_duplicate_rows = sum(
        by_survey_year$old_rows - by_survey_year$old_sets
      ),
      stringsAsFactors = FALSE
    ),
    by_survey_year = by_survey_year,
    differences = differences
  )
}

compare_survey_sets <- function(species = "pacific-cod") {
  selected_species <- intersect(common_species, species)
  if (!length(selected_species)) {
    stop("No requested species files were found in both caches.")
  }
  comparison <- compare_species(selected_species[1])
  cat("\nCompared", selected_species[1], "in SYN/HBLL surveys.\n")
  cat("Set identity: survey_abbrev + year + fishing_event_id.\n\n")
  print(comparison$species_summary, row.names = FALSE)
  changed <- comparison$by_survey_year[
    comparison$by_survey_year$new_only > 0 |
      comparison$by_survey_year$old_only > 0,
    , drop = FALSE
  ]
  cat("\nOverlapping survey-year groups with different selected set IDs:\n")
  if (nrow(changed)) print(changed, row.names = FALSE) else cat("None\n")
  cat("\nDiffering set IDs in overlapping survey-year groups:\n")
  if (nrow(comparison$differences)) {
    print(comparison$differences, row.names = FALSE)
  } else {
    cat("None\n")
  }
  invisible(comparison)
}

if (!interactive()) {
  selected_species <- if (is.null(requested_species)) {
    common_species
  } else {
    intersect(common_species, requested_species)
  }
  if (!length(selected_species)) {
    stop("No requested species files were found in both caches.")
  }

comparisons <- lapply(selected_species, compare_species)
names(comparisons) <- selected_species
species_summary <- do.call(
  rbind, lapply(comparisons, `[[`, "species_summary")
)
bind_nonempty <- function(x) {
  x <- Filter(function(z) is.data.frame(z) && nrow(z) > 0, x)
  if (length(x)) do.call(rbind, x) else data.frame()
}
by_survey_year <- bind_nonempty(lapply(comparisons, `[[`, "by_survey_year"))
differences <- bind_nonempty(lapply(comparisons, `[[`, "differences"))

cat("\nCompared", length(selected_species), "species in SYN/HBLL surveys.\n")
cat("Set identity: survey_abbrev + year + fishing_event_id.\n\n")

cat("Species present only in the current cache:\n")
print(setdiff(new_species, old_species), quote = FALSE)
cat("Species present only in the prior cache:\n")
print(setdiff(old_species, new_species), quote = FALSE)

cat("\nSpecies-level selection summary (non-zero new_only or old_only indicates a difference):\n")
print(species_summary, row.names = FALSE)

changed <- by_survey_year[
  by_survey_year$new_only > 0 | by_survey_year$old_only > 0,
  , drop = FALSE
]
cat("\nOverlapping survey-year groups with different selected set IDs:\n")
if (nrow(changed)) print(changed, row.names = FALSE) else cat("None\n")

if (length(species_arg)) {
  difference_key <- paste(
    differences$species, differences$survey_abbrev, differences$year,
    sep = "\r"
  )
  overlap_key <- paste(
    changed$species, changed$survey_abbrev, changed$year, sep = "\r"
  )
  exact_differences <- differences[difference_key %in% overlap_key, , drop = FALSE]
  cat("\nDiffering set IDs in overlapping survey-year groups:\n")
  if (nrow(exact_differences)) {
    print(exact_differences, row.names = FALSE)
  } else {
    cat("None\n")
  }
} else {
  cat("\nExact differing set IDs were not printed for all species. Use --species=<name> to print them.\n")
}

invisible(list(
  species_summary = species_summary,
  by_survey_year = by_survey_year,
  differences = differences
))
}

# compare_survey_sets()
