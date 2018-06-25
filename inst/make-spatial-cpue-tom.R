library(gfsynopsis)
library(gfplot)
library(dplyr)

spp <- get_spp_names() %>%
  filter(type == "A")

# gfplot::cache_pbs_data(
#   spp$species_common_name,
#   path = "report/data-cache3-uncompressed/",
#   compress = FALSE
# )

extract_cpue_dat <- function(years = 2013:2017, dat_name = "cpue_spatial",
  units = "kg/hr") {
  purrr::map_df(spp$spp_w_hyphens, function(spp_i) {
    cat(paste0(spp_i, "\n"))
    d <- readRDS(paste0("report/data-cache3-uncompressed/", spp_i, ".rds"))
    purrr::map_df(years, function(this_year) {
      xx <- d[[dat_name]] %>%
        filter(year == this_year) %>%
        gfplot::plot_cpue_spatial(
          bin_width = 10, n_minimum_vessels = 3,
          return_data = TRUE
        )
      if (!is.null(xx)) {
        xx %>%
          select(x, y, value) %>%
          mutate(
            year = this_year,
            species = gsub("-", " ", spp_i),
            units = units,
            resolution = "10km hexagon",
            min_vessels = 3
          ) %>%
          rename(easting = x, northing = y, cpue = value) %>%
          as_tibble()
      }
    })
  })
}

trawl <- extract_cpue_dat(years = 2013:2017, dat_name = "cpue_spatial")
ll <- extract_cpue_dat(years = 2008:2017, dat_name = "cpue_spatial_ll", units = "fish/set")
saveRDS(trawl, file = "inst/gf-cpue-spatial-trawl.rds")
saveRDS(ll, file = "inst/gf-cpue-spatial-ll.rds")
