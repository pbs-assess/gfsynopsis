##' Format the final year of IPHC data to use in the spatial map
##'
##' Takes the final year of data to get into the correct format to fit into
##'  plotting functions for gfsynopsis report.
##' @param set_counts species-specific set-level data from [gfiphc::tidy_iphc_survey()]
##' @param final_year year for which to plot the set-by-set catch rates
##' @return tibble in the format required by the plotting function, with a row
##'  of NA's if the data are not present.
##' @examples
##' \dontrun{
##' # If already loaded data via gfsynopsis then just, for any species,
##' format_final_year_for_map(
##'   readRDS("report/data-cache/iphc/china-rockfish.rds")$set_counts)
##'
##' # Else to load from scratch:
##' set_counts <- get_all_iphc_set_counts("yelloweye rockfish")
##' format_final_year_for_map(set_counts, 2017)
##' }
format_final_year_for_map_iphc <- function(set_counts, final_year)
{
  return_NA <- tibble(X = NA,
    Y = NA,
    akima_depth = NA,
    depth_scaled = NA,
    depth_scaled2 = NA,
    combined = NA,
    pos = NA,
    bin = NA,
    survey = "IPHC FISS")

  set_counts_final <- filter(set_counts,
    year == final_year,
    usable == "Y") %>%
    select(lat,
      lon,
      C_it)

  if(nrow(set_counts_final) == 0 ) {
    return(return_NA)
  }

  if(all(is.na(set_counts_final$C_it))) {
    return(return_NA)
  }

  utm <- gfplot:::ll2utm(select(set_counts_final,
    X = lon,
    Y = lat))

  mutate(utm,
    akima_depth = NA,
    depth_scaled = NA,
    depth_scaled2 = NA,
    combined = set_counts_final$C_it,
    pos = NA,
    bin = NA,
    survey = "IPHC FISS")
}
