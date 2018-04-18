devtools::load_all("../gfplot/")
devtools::load_all(".")
library(dplyr)

dc                  <- file.path("report", "data-cache2")
dat                 <- list()
dat$survey_sets     <- readRDS(file.path(dc, "pbs-survey-sets.rds"))
dat$survey_samples  <- readRDS(file.path(dc, "pbs-survey-samples.rds"))
dat$comm_samples    <- readRDS(file.path(dc, "pbs-comm-samples.rds"))
dat$catch           <- readRDS(file.path(dc, "pbs-catch.rds"))
dat$cpue_spatial    <- readRDS(file.path(dc, "pbs-cpue-spatial.rds"))
dat$cpue_spatial_ll <- readRDS(file.path(dc, "pbs-cpue-spatial-ll.rds"))
dat$survey_index    <- readRDS(file.path(dc, "pbs-survey-index.rds"))
dat$age_precision   <- readRDS(file.path(dc, "pbs-age-precision.rds"))
# dat$cpue_index    <- readRDS(file.path(dc, "pbs-cpue-index.rds"))
# feather::write_feather(dat$cpue_index, file.path(dc, "pbs-cpue-index.feather"))
dat$cpue_index      <- feather::read_feather(file.path(dc, "pbs-cpue-index.feather"))

spp <- get_spp_names()
spp <- dplyr::filter(spp, species_common_name %in%
  # unique(c(spp$species_common_name[13:15])))
  # "north pacific spiny dogfish",
  c("pacific ocean perch",
  "pacific cod"#,
  # "widow rockfish",
  # "redbanded rockfish",
  # "yelloweye rockfish"
  ))

# spp <- dplyr::filter(spp, species_common_name %in%
#       "redbanded rockfish")

refs <- readr::read_csv("report/spp-refs.csv")
spp <- left_join(spp, refs, by = "species_common_name")
spp$sar[is.na(spp$sar)] <- ""
spp$resdoc[is.na(spp$resdoc)] <- ""

for (i in seq_along(spp$species_common_name)) {
  cat(crayon::red(clisymbols::symbol$tick),
    "Building figures for", spp$species_common_name[i], "\n")
  suppressMessages(
    make_pages(dat, spp$species_common_name[i],
      include_map_square = FALSE,
      resolution = 185,
      save_gg_objects = TRUE,
      survey_cols = c(RColorBrewer::brewer.pal(7L, "Dark2"),
      # survey_cols = c(RColorBrewer::brewer.pal(5L, "Set1"),
      #   RColorBrewer::brewer.pal(8L, "Set1")[7:8],
        "#303030", "#a8a8a8", "#a8a8a8", "#a8a8a8")
    )
  )
}

temp <- lapply(spp$species_common_name, function(x) {
  spp_file <- clean_name(x)
  spp_title <- all_cap(x)
  out <- list()
  latin_name <- spp$spp_latin[spp$species_common_name == x]
  sar <- spp$sar[spp$species_common_name == x]
  resdoc <- spp$resdoc[spp$species_common_name == x]

  i <- 1
  out[[i]] <- "\\clearpage"
  i <- i + 1
  out[[i]] <- "\\begin{minipage}[t][4cm][t]{\\textwidth}"
  i <- i + 1
  out[[i]] <- paste0("\\subsection*{", spp_title, "}")
  i <- i + 1
  out[[i]] <- paste0(emph(latin_name), "\n")
  i <- i + 1
  out[[i]] <- "\\vspace{8pt}"
  i <- i + 1
  out[[i]] <- paste0(sar, "\n")
  i <- i + 1
  out[[i]] <- "\\vspace{8pt}"
  i <- i + 1
  out[[i]] <- resdoc
  i <- i + 1
  out[[i]] <- "\\end{minipage}\n"
  i <- i + 1
  out[[i]] <- "\\begin{figure}[ht]"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=6.4in]{../figure-pages/", spp_file, "-1.png}")
  i <- i + 1
  out[[i]] <- "\\end{figure}"
  i <- i + 1
  out[[i]] <- "\\clearpage"
  i <- i + 1
  out[[i]] <- "\\begin{figure}[ht]"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=6.4in]{../figure-pages/", spp_file, "-2.png}")
  i <- i + 1
  out[[i]] <- "\\end{figure}\n"
  i <- i + 1
  out[[i]] <- "% ---------------------------------------------------------------------------\n"
  out
})

temp <- lapply(temp, function(x) paste(x, collapse = "\n"))
temp <- paste(temp, collapse = "\n")
writeLines(temp, con = "report/report/doc/02-plots.Rnw")

# system("make pdf")
