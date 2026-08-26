#!/usr/bin/env Rscript

# Crop transparent margins from silhouette SVG viewBoxes. The bounds are found
# by rendering the SVG, so this works for both path-based and Inkscape SVGs.

svg_view_box <- function(svg_file) {
  lines <- readLines(svg_file, warn = FALSE)
  svg_line <- grep('viewBox="', lines, fixed = TRUE)
  if (!length(svg_line)) return(NULL)
  svg_line <- svg_line[[1L]]
  match <- regexec(
    'viewBox="[[:space:]]*([-+.0-9eE]+)[[:space:]]+([-+.0-9eE]+)[[:space:]]+([-+.0-9eE]+)[[:space:]]+([-+.0-9eE]+)[[:space:]]*"',
    lines[[svg_line]], perl = TRUE
  )
  values <- regmatches(lines[[svg_line]], match)[[1L]]
  if (length(values) != 5L) return(NULL)
  as.numeric(values[-1L])
}

trim_svg_view_box <- function(
    svg_file, render_width = 2048L, padding = 12L, tolerance = 2L) {
  view_box <- svg_view_box(svg_file)
  if (is.null(view_box)) {
    warning("Skipping SVG without a viewBox: ", svg_file, call. = FALSE)
    return(FALSE)
  }
  remove_root_dimensions <- function(lines) {
    svg_start <- grep("^<svg", lines)[[1L]]
    svg_end <- svg_start - 1L + grep(">", lines[svg_start:length(lines)], fixed = TRUE)[[1L]]
    root_lines <- seq.int(svg_start, svg_end)
    lines[root_lines] <- gsub(
      '[[:space:]](?:width|height)="[^"]*"', "", lines[root_lines], perl = TRUE
    )
    lines
  }
  original_lines <- readLines(svg_file, warn = FALSE)
  normalized_lines <- remove_root_dimensions(original_lines)
  dimensions_removed <- !identical(original_lines, normalized_lines)
  png_file <- tempfile(fileext = ".png")
  on.exit(unlink(png_file), add = TRUE)
  status <- system2(
    "rsvg-convert",
    c("--width", render_width, "--output", png_file, svg_file),
    stdout = FALSE, stderr = FALSE
  )
  if (!identical(status, 0L)) {
    stop("Could not render SVG: ", svg_file, call. = FALSE)
  }
  geometry <- system2(
    "identify", c("-format", shQuote("%w %h %@"), png_file),
    stdout = TRUE, stderr = TRUE
  )
  values <- regmatches(
    geometry,
    regexec("^([0-9]+) ([0-9]+) ([0-9]+)x([0-9]+)\\+(-?[0-9]+)\\+(-?[0-9]+)$", geometry)
  )[[1L]]
  if (length(values) != 7L) {
    stop("Could not find visible SVG bounds: ", svg_file, call. = FALSE)
  }
  values <- as.numeric(values[-1L])
  canvas_width <- values[[1L]]
  canvas_height <- values[[2L]]
  content_width <- values[[3L]]
  content_height <- values[[4L]]
  content_x <- values[[5L]]
  content_y <- values[[6L]]
  margins <- c(
    content_x, content_y,
    canvas_width - content_x - content_width,
    canvas_height - content_y - content_height
  )
  if (all(abs(margins - padding) <= tolerance)) {
    if (dimensions_removed) writeLines(normalized_lines, svg_file, useBytes = TRUE)
    return(dimensions_removed)
  }

  x0 <- content_x - padding
  y0 <- content_y - padding
  x1 <- content_x + content_width + padding
  y1 <- content_y + content_height + padding
  cropped <- c(
    view_box[[1L]] + x0 / canvas_width * view_box[[3L]],
    view_box[[2L]] + y0 / canvas_height * view_box[[4L]],
    (x1 - x0) / canvas_width * view_box[[3L]],
    (y1 - y0) / canvas_height * view_box[[4L]]
  )
  lines <- readLines(svg_file, warn = FALSE)
  svg_line <- grep('viewBox="', lines, fixed = TRUE)[[1L]]
  formatted <- paste(formatC(cropped, digits = 8L, format = "fg", flag = "#"), collapse = " ")
  lines[[svg_line]] <- sub(
    'viewBox="[^"]+"', paste0('viewBox="', formatted, '"'), lines[[svg_line]]
  )
  lines <- remove_root_dimensions(lines)
  writeLines(lines, svg_file, useBytes = TRUE)
  TRUE
}

trim_silhouette_directory <- function(asset_dir, quiet = FALSE) {
  if (!nzchar(Sys.which("rsvg-convert")) || !nzchar(Sys.which("identify"))) {
    stop("Trimming silhouettes requires rsvg-convert and ImageMagick identify.",
      call. = FALSE)
  }
  svg_files <- list.files(asset_dir, pattern = "[.]svg$", recursive = TRUE,
    full.names = TRUE)
  svg_files <- svg_files[basename(svg_files) != "dfo-logo.svg"]
  trimmed_files <- character()
  pending_files <- svg_files
  for (pass in seq_len(20L)) {
    trimmed <- vapply(pending_files, trim_svg_view_box, logical(1))
    pending_files <- pending_files[trimmed]
    trimmed_files <- union(trimmed_files, pending_files)
    if (!length(pending_files)) break
  }
  if (length(pending_files)) {
    warning(
      "Some silhouettes could not reach the requested padding after 20 passes: ",
      paste(basename(pending_files), collapse = ", "),
      call. = FALSE
    )
  }
  if (!quiet) {
    message("Trimmed ", length(trimmed_files), " of ", length(svg_files),
      " silhouette SVGs.")
  }
  invisible(trimmed_files)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  asset_dir <- if (length(args)) args[[1L]] else file.path("report", "web", "assets")
  trim_silhouette_directory(asset_dir)
}
