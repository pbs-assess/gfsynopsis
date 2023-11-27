# assemble data frame of design-based indices
# assemble data frame of geostat-based indices
# scale geostat to max 1
# scale design to have some geo mean
# add any shading

# make base design-plot dots/lines
# overlay geostat ribbons

library(purrr)
library(dplyr)
library(ggplot2)
library(here)
devtools::load_all(".")

# setup -------------------------------------------------

spp_w_hyphens <- "lingcod"
dat <- readRDS("report/data-cache-oct-2023/lingcod.rds")
dc <- here("report", "data-cache-oct-2023")
report_folder <- "report"
iphc_index_cache <- file.path(report_folder, "iphc-cache")
dir.create(iphc_index_cache, showWarnings = FALSE, recursive = TRUE)
spp_file <- spp_w_hyphens
iphc_index_cache_spp <- paste0(file.path(iphc_index_cache, spp_file), ".rds")
final_year_surv <- 2022
french <- FALSE

survey_cols <- c(
  "SYN WCHG" = "#E41A1C",
  "SYN WCVI" = "#984EA3",
  "SYN HS" = "#377EB8",
  "SYN QCS" = "#4DAF4A",
  "HBLL OUT N" = "#FF7F00",
  "HBLL OUT S" = "#FDBF6F",
  "HBLL INS N/S" = "#A65628",
  "IPHC FISS" = "#F781BF",
  "MSSM WCVI" = "#6c6c6c",
  "MSSM Geostat" = "#6c6c6c",
  "MSA HS" = "#6c6c6c",
  "SYN HS/QCS/WCHG/WCVI" = "#6c6c6c",
  "SYN HS/QCS/WCVI" = "#6c6c6c",
  "HBLL OUT N/S" = "#6c6c6c",
  "Commercial" = "#303030"
)

lvls <- c(
  "SYN WCHG",
  "SYN WCVI",
  "SYN HS",
  "SYN QCS",
  "HBLL OUT N",
  "HBLL OUT S",
  "HBLL INS N/S",
  "IPHC FISS",
  "MSA HS",
  "SYN WCHG/HS/QCS/WCVI",
  "SYN HS/QCS/WCVI",
  "MSSM WCVI",
  "HBLL OUT N/S"
)

# get design indices ---------------------------------------
dat_design <- tidy_survey_index(
  dat$survey_index,
  survey = c(
    "SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI", "HBLL OUT N",
    "HBLL OUT S", "OTHER HS MSA", "MSSM WCVI", "IPHC FISS"
  )
)
dat_design$survey_abbrev <- gsub("OTHER HS MSA", "MSA HS", dat_design$survey_abbrev)

# sub iphc -------------------------------------------------

dat_design <- gfsynopsis:::sub_iphc_design(dc, iphc_index_cache_spp, spp_w_hyphens, dat_design)

# read geostat ---------------------------------------------

get_index <- function(folder, spp, .family = "", model_tag = "st-rw") {
  paths <- list.files(folder, pattern = ".rds", full.names = TRUE)
  path <- paths[grepl(spp, paths)]
  if (length(path)) {
    file_names <- list.files(folder, pattern = ".rds")
    sp <- gsub("-", " ", spp)
    if (file.exists(path)) {
      d <- readRDS(path)
      if (length(d) > 1L) {
        return(dplyr::mutate(d, species = sp, family = .family))
      }
    }
  }
}

families <- c(
  "delta-gamma",
  "delta-poisson-link-gamma",
  "tweedie",
  "delta-poisson-link-lognormal",
  "delta-lognormal"
)
geo <- list()
geo$syn_coast <- map_dfr(families, \(f) {
  get_index(paste0("report/stitch-cache/synoptic-", f, "/"), spp_w_hyphens, .family = f)
}) |> filter(aic == min(aic))
geo$mssm <- map_dfr(families, \(f) {
  get_index(paste0("report/stitch-cache/mssm-", f, "/"), spp_w_hyphens, .family = f)
}) |> filter(aic == min(aic))
geo$hbll_ins <- get_index(paste0("report/stitch-cache/hbll_inside"), spp_w_hyphens)
geo$hbll_out <- get_index(paste0("report/stitch-cache/hbll_outside"), spp_w_hyphens)
geo$iphc <- get_index(paste0("report/stitch-cache/iphc"), spp_w_hyphens)
geo$iphc$stitch_regions <- "IPHC FISS"
dat_geo <- bind_rows(geo)
dat_geo$survey_abbrev <- dat_geo$stitch_regions
dat_geo$survey_abbrev <- gsub("HBLL INS N, HBLL INS S", "HBLL INS N/S", dat_geo$survey_abbrev)
dat_geo$survey_abbrev <- gsub("HBLL OUT N, HBLL OUT S", "HBLL OUT N/S", dat_geo$survey_abbrev)
dat_geo$survey_abbrev <- gsub("SYN HS, SYN QCS, SYN WCHG, SYN WCVI", "SYN WCHG/HS/QCS/WCVI", dat_geo$survey_abbrev)
dat_geo$survey_abbrev <- gsub("SYN HS, SYN QCS, SYN WCVI", "SYN HS/QCS/WCVI", dat_geo$survey_abbrev)
dat_geo <- select(dat_geo, survey_abbrev, year, biomass, lowerci, upperci, mean_cv, num_sets, num_pos_sets) |> as_tibble()

# combine both types -------------------------------------
dat_design$method <- "design"
dat_geo$method <- "geostat"
dat_design$survey_abbrev <- as.character(dat_design$survey_abbrev)
both <- bind_rows(dat_geo, dat_design)

# scale as needed ----------------------------------------

both_scaled <- group_by(both, survey_abbrev) |>
  group_split() |>
  map_dfr(\(x) {
    both_present <- length(unique(x$method)) > 1L
    if (both_present) {
      x_geo <- filter(x, method == "geostat")
      x_des <- filter(x, method == "design")
      overlapping_years <- intersect(x_geo$year, x_des$year)
      x_geo_mean <- exp(mean(log(x_geo$biomass)))
      x_des_mean <- exp(mean(log(x_des$biomass)))
      x_geo <- mutate(x_geo,
        biomass_scaled = biomass / x_geo_mean,
        lowerci_scaled = lowerci / x_geo_mean,
        upperci_scaled = upperci / x_geo_mean
      )
      x_des <- mutate(x_des,
        biomass_scaled = biomass / x_des_mean,
        lowerci_scaled = lowerci / x_des_mean,
        upperci_scaled = upperci / x_des_mean
      )
      max_geo <- max(x_geo$upperci_scaled)
      xx <- bind_rows(x_geo, x_des)
      mutate(xx,
        biomass_scaled = biomass_scaled / max_geo,
        lowerci_scaled = lowerci_scaled / max_geo,
        upperci_scaled = upperci_scaled / max_geo
      )
    } else {
      mutate(x,
        biomass_scaled = biomass / max(upperci),
        lowerci_scaled = lowerci / max(upperci),
        upperci_scaled = upperci / max(upperci)
      )
    }
  }) |>
  mutate(biomass = biomass_scaled, lowerci = lowerci_scaled, upperci = upperci_scaled) |>
  mutate(survey_abbrev = factor(survey_abbrev, levels = lvls))

geo_scaled <- filter(both_scaled, method == "geostat")
des_scaled <- filter(both_scaled, method == "design")

# plot! --------------------------------------------------

g <- plot_survey_index(
  dat = des_scaled,
  scale = FALSE,
  col = c("grey60", "grey20"),
  survey_cols = survey_cols,
  xlim = c(1975 - 0.2, final_year_surv + 0.2),
  french = french,
  scale_type = "max-CI", pjs_mode = TRUE) +
  coord_cartesian(ylim = c(-0.005, 1.03),
    xlim = c(1975, final_year_surv) + c(-0.5, 0.5), expand = FALSE) +
  geom_line(data = geo_scaled, mapping = aes(colour = survey_abbrev)) +
  geom_ribbon(
    data = geo_scaled,
    mapping = aes(ymin = lowerci_scaled, ymax = upperci_scaled, fill = survey_abbrev), alpha = 0.2) +
  scale_x_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE))

if ("MSSM WCVI" %in% both_scaled$survey_abbrev) {
  g <- g +
    geom_rect(
      data = filter(both_scaled, survey_abbrev == "MSSM WCVI")[1,,drop=FALSE],
      mapping = ggplot2::aes(xmin = 1973, xmax = 2003, ymin = -Inf, ymax = Inf),
      alpha = 0.15
    )
}
print(g)
