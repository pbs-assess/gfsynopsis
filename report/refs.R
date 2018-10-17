library(dplyr)
check_refs <- function(file) {
  sr <- readxl::read_excel(file)
  sr$`TITLE FRENCH` <- NULL
  names(sr) <- tolower(names(sr))
  sr$id <- seq_len(nrow(sr))

  spp <- gfsynopsis::get_spp_names()

  check <- function(dat, spp_name1, spp_name2) {
    i1 <- grep(spp_name1, dat$`title english`, ignore.case = TRUE)
    i2 <- grep(spp_name2, dat$`title english`, ignore.case = TRUE)
    union(i1, i2)
  }

  out <- purrr::map2_df(
    .x = spp$species_common_name,
    .y = spp$species_science_name, function(.x, .y) {
    ii <- check(sr, .x, .y)
    if (length(ii) > 0L)
      data.frame(species_common_name = .x, id = ii,
        stringsAsFactors = FALSE)
    }
  )
  out <- dplyr::left_join(out, sr) %>%
    arrange(series, year, species_common_name)

  out
}

out <- list()
out[[1]] <- check_refs("inst/extdata/sr-2018-10-16.xlsx")
out[[2]] <- check_refs("inst/extdata/resdocs-2018-10-16.xlsx")
out[[3]] <- check_refs("inst/extdata/sar-2018-10-16.xlsx")
out[[4]] <- check_refs("inst/extdata/all-advisory-2018-10-16.xlsx")
out <- bind_rows(out)
spp <- gfsynopsis::get_spp_names()
out <- out[!duplicated(out),, drop=FALSE]
out <- left_join(out, select(spp, species_common_name, species_code, type))
out <- arrange(out, type, species_code, species_common_name, year, series)
# View(out)
readr::write_csv(out, "report/csas-filtered.csv")
