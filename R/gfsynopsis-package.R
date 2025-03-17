#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom rlang .data
#' @importFrom stats quantile
#' @importFrom utils capture.output
#' @importFrom ggplot2 aes
#' @importFrom dplyr group_split across all_of join_by
#' @importFrom here here
## usethis namespace: end
NULL

# from: https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
# quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "N", "area", "depth", "est", "est_unstandardized", "female", "lat", "lon", "lwr",
    "mature", "max_value", "model", "pos_catch", "pred", "sampling_desc",
    "species_code", "species_common_name", "survey_abbrev", "term", "tidy_survey_sets",
    "upr", "year", "min_raw_depth", "year_factor", "spp_w_hyphens",
    "spp_catch", "hours_fished",
    "akima_depth", "se", "usable", "C_it", 'major_stat_area_description',
    'survey', 'species', 'biomass_scaled', 'lowerci_scaled', 'st_geo_mean',
    'maturity', 'age_or_length', 'glmm_fe', 'mean_mat', 'N_min', 'locality',
    'est_link', 'se_link', 'survey', 'max_raw_depth', 'density', 'surv',
    'upperci_scaled', 'design_geo_mean',

    "C_it20", "E_it", "E_it20", "N_it", "N_it20", "X", "Y", "area_swept",
    "baited_hooks", "biomass", "catch", "catch_count", "catch_weight",
    "count_bait_only", "d_geomean", "d_scale_factor",
    "derivative", "doorspread_m",
    "duration_min", "effSkate", "effSkateIPHC", "effort",
    "fishing_event_id", "fit", "french",
    "geo_scale_factor", "get_iphc_sets_info",
    "hook_adjust_factor", "hook_count", "hook_removed",
    "hooksObserved", "include_in_stitch", "index_type", "iphcUsabilityCode",
    "iphcUsabilityDesc", "lower", "lowerci", "mean_cv", "mean_n_pos", "mean_n_sets",
    "mean_prop_pos", "measured", "month", "n_pos", "n_sets", "num_pos_sets",
    "num_sets", "obsHooksPerSet", "offset", "present", "prop_bait_hooks",
    "prop_pos", "prop_removed", "pstar", "scaled_geomean", "sign_change", "setID",
    "slope_xh", "specCount", "speed_mpm", "spNameIPHC", "standard", "station",
    "stitch_tally",
    "survey_abbrev2", "survey_domain_year", "survey_series_name", "tow_length_m",
    "upper", "upperci", "utm_zone", "year_bin",
    "COSEWIC", "Scientific", "Schedule", "itis_tsn", "baits_returned", "hooks_observed",
    "number_observed", "sara_status", "cosewic_pop", "cosewic_status"
  ))
}
