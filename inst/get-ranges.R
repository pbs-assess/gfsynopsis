# grab range for fitted spatial models by taxa:

f <- list.files("report/map-cache/synoptic/", full.names = T)

ff <- c(
  grep("petrale", f),
  grep("arrowtooth", f),
  grep("dover", f),
  grep("english-sole", f),
  grep("rex", f),
  grep("slender", f),
  grep("halibut", f)
)
sole <- grep("sole", f)
ff <- union(ff, sole)
f[ff]

pc <- grep("pacific-cod", f)
pc

rf <- grep("rockfis", f)
f[rf]

library(sdmTMB)

get_ranges <- function(x) {
  purrr::map_dfr(x, function(i) {
    d <- readRDS(f[i])
    cat(d$species, "\n")
    r <- purrr::map_dbl(d$models, function(.x) {
      m <- .x$models
      if (!identical(m, NA)) {
        tidy(m, "ran_pars") |>
          dplyr::filter(term == "range") |> dplyr::pull(estimate)
      } else {
        NA_real_
      }
    })
    tibble::tibble(species = d$species, range = r, survey = purrr::map_chr(d$models, "survey"))
  })
}
r_ff <- get_ranges(ff)
r_pc <- get_ranges(pc)
r_rf <- get_ranges(rf)

library(dplyr)
r_rf <- filter(r_rf, !is.na(range), range > 1)
r_ff <- filter(r_ff, !is.na(range), range > 1)

r_rf$taxa <- "rockfish"
r_pc$taxa <- "pacific cod"
r_ff$taxa <- "flatfish"

r <- bind_rows(r_rf, r_pc, r_ff)

group_by(r, taxa) |>
  summarise(mean_range = mean(range), sd_range = sd(range))

library(ggplot2)
ggplot(r, aes(range, taxa)) + geom_violin() + geom_jitter(width = 0, height = 0.1)
ggsave("inst/range-by-taxa-violin.png", width = 6, height = 3)

ggplot(r, aes(range, taxa)) + geom_boxplot() + geom_jitter(width = 0, height = 0.1)
ggsave("inst/range-by-taxa-boxplot.png", width = 6, height = 3)

ggplot(r, aes(range, species, colour = survey)) + geom_point() +
  facet_wrap(vars(taxa), scales = "free_y", ncol = 1L)
ggsave("inst/range-by-taxa-dot.png", width = 8, height = 8)
