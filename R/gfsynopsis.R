#' gfsynopsis package
#'
#' gfsynopsis package
#'
#' See the README on
#' \href{https://github.com/seananderson/gfsynopsis#readme}{GitHub}
#'
#' @docType package
#' @name gfsynopsis
#'
#' @importFrom rlang .data
#' @importFrom stats quantile
#' @importFrom utils capture.output
NULL

# from: https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
# quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "N", "area", "depth", "est", "est_unstandardized", "female", "lat", "lon", "lwr",
    "mature", "max_value", "model", "pos_catch", "pred", "sampling_desc",
    "species_code", "species_common_name", "survey_abbrev", "term", "tidy_survey_sets",
    "upr", "year"
  ))
}
