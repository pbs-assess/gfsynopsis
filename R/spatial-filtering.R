#' Subset a data frame spatially using an \pkg{sf} polygon
#'
#' Filters a dataset based on spatial intersection with a given polygon.
#' The function converts the input data frame to an \pkg{sf} object using
#' specified coordinate columns, applies coordinate transformations if
#' necessary, and returns either a filtered dataframe (default) or an
#' \pkg{sf} object.
#'
#' @param dat A data frame (not \pkg{sf} object) containing spatial data with
#'   longitude and latitude coordinates.
#' @param sf_poly An \pkg{sf} polygon object used to spatially filter the dataset.
#' @param xy_coords A character vector of length two specifying the
#'   column names for longitude and latitude in `dat`.
#' @param dat_crs Integer. Coordinate reference system (CRS) for `dat`.
#'   Default is `4326` (WGS84).
#' @param return_sf Logical. If `TRUE`, returns an \pkg{sf} object; if
#'   `FALSE`, returns a data frame. Default is `FALSE`.
#'
#' @return A dataframe without geometry (default) or an \pkg{sf} object if
#' `return_sf = TRUE`.
#'
#' @export
subset_spatial <- function(dat, sf_poly, xy_coords, dat_crs = 4326, return_sf = FALSE) {
  xy_name_check <- setdiff(xy_coords, names(dat))
  if (length(xy_name_check) > 0) {
    stop("Check lon/lat names: ", paste(xy_name_check[TRUE], collapse = ", "), call. = FALSE)
  }
  if (sum(is.na(dat[xy_coords[1]])) > 0L) {
    message(cat("Warning: ", sum(is.na(dat$xy_coords[1]), " NA latitudes removed")))
  }
  if (sum(is.na(dat[xy_coords[2]])) > 0L) {
    message(cat("Warning: ", sum(is.na(dat$xy_coords[2]), " NA longitudes removed")))
  }

  out_sf <- sf::st_as_sf(dat, coords = xy_coords, remove = FALSE, crs = dat_crs) |>
    sf::st_transform(crs = sf::st_crs(sf_poly)) |>
    sf::st_filter(sf_poly)

  if (!return_sf) {
    sf::st_drop_geometry(out_sf)
  } else {
    out_sf
  }
}

# table(filtered_catch_latlon$year)
#
# required:
# landed_kg
# discarded_kg
# landed_pcs
# discarded_pcs
# species_common_name
# year
# gear [UNKNOWN, BOTTOM TRAWL, HOOK AND LINE, LONGLINE, MIDWATER TRAWL, TRAP,
# UNKNOWN TRAWL]
# major_stat_area_name
#

# could join on CPUE data for 1996-2005 for trawl:
#   cpue <- readRDS("report/data-cache-2025-03/cpue-index-dat.rds")
#   glimpse(dat$catch)
#   glimpse(cpue)
#
#   select(dat$catch, major_stat_area_name, major_stat_area_code) |>
#     distinct() |> as.data.frame() |> dput()
#
#   lu <- structure(list(major_stat_area_name = c("4B: STRAIT OF GEORGIA",
# "3C: S.W. VANCOUVER ISLAND", "5A: SOUTHERN Q.C. SOUND", "5B: NORTHERN Q.C.
#  SOUND",
# "5D: NORTHERN HECATE STRAIT", "3D: N.W. VANCOUVER ISLAND", "5C: SOUTHERN H
# ECATE STRAIT",
# "5E: WEST COAST Q.C. ISLANDS", "ALASKA", "UNKNOWN: NO POSITION INFORMATION
# ",
# "4A:PUGET SOUND", "BRITISH COLUMBIA OFFSHORE WATERS", "3A:CAPE FALCON TO C
# APE ELIZABETH (45 46' TO 47 20')",
# "3B: CAPE FLATTERY (47 20' to 220 T)", "2C:CAPE PERPETUA TO CAPE FALCON (4
# 4 18' TO 45 46')",
# "1B: PIEDRAS BLANCAS TO CAPE MENDOCINO (36 00' TO 40 30')", "1A: US-MEXICO
#  BORDER TO PIEDRAS (32 30' TO 36 00')",
# "2A: OR-CA BORDER TO CAPE BLANCO (42 00' TO 42 50')", "1C: CAPE MENDOCINO
# TO OR-CA BORDER (40 30' TO 42 00')"
# ), major_stat_area_code = c("01", "03", "05", "06", "08", "04",
# "07", "09", "10", "00", "68", "11", "67", "02", "66", "62", "61",
# "64", "63")), row.names = c(NA, -19L), version = structure(list(
#     c(0L, 1L, 4L)), class = c("package_version", "numeric_version"
# )), date = structure(1741901615.74571, class = c("POSIXct", "POSIXt"
# )), class = "data.frame")
#
#   cpue <- left_join(cpue, lu)
#   glimpse(cpue)
#   unique(cpue$gear)
