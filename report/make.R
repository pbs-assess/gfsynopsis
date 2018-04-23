devtools::load_all("../gfplot/")
devtools::load_all(".")
library("dplyr")
# library("future")
# future::plan(multiprocess, workers = 2)
# options(future.globals.maxSize = 4000 * 1024 ^ 2) # 4GB
# future::plan(sequential)
# future::plan(transparent)
# library("doParallel")
# registerDoParallel(cores = 4L)

# ------------------------------------------------------------
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

# ------------------------------------------------------------
spp <- get_spp_names()
spp <- filter(spp, type == "A")
if (exists("N"))
  spp <- spp[N, , drop = FALSE]
spp <- filter(spp, species_common_name != "sablefish")
spp <- filter(spp, species_common_name != "pacific hake")

refs <- readr::read_csv("report/spp-refs.csv")
spp <- left_join(spp, refs, by = "species_common_name")

meta <- dat$survey_sets %>%
  select(species_common_name, species_science_name, species_code) %>%
  unique()

spp <- left_join(spp, meta, by = "species_common_name")
spp$species_science_name <- gfplot:::firstup(spp$species_science_name)
spp$species_science_name <- gsub(" complex", "", spp$species_science_name)
spp$resdoc <- ifelse(is.na(spp$resdoc), "", paste0("\\citet{", spp$resdoc, "}"))
spp$sar <- ifelse(is.na(spp$sar), "", paste0("\\citet{", spp$sar, "}"))

spp$other_ref_cite <- ifelse(is.na(spp$other_ref), "",
  paste0(spp$type_other_ref, ": \\citet{", spp$other_ref, "}"))

# ------------------------------------------------------------
# TODO: memory mapping problem:
tmb_cpp <- system.file("tmb", "deltalognormal.cpp", package = "gfplot")
TMB::compile(tmb_cpp)
dyn.load(TMB::dynlib(sub("\\.cpp", "", tmb_cpp)))
model_file <- system.file("stan", "vb.stan", package = "gfplot")
mod <- rstan::stan_model(model_file)

system.time({
# ------------------------------------------------------------
# plyr::l_ply(seq_along(spp$species_common_name), function(i) {
for (i in seq_along(spp$species_common_name)) {
  fig_check <- paste0(file.path("report", "figure-pages"), "/",
    gfsynopsis:::clean_name(spp$species_common_name[i]))
  fig_check1 <- paste0(fig_check, "-1.png")
  fig_check2 <- paste0(fig_check, "-2.png")

  if (!file.exists(fig_check1) || !file.exists(fig_check2)) {
    cat(crayon::red(clisymbols::symbol$cross),
      "Building figure pages for", spp$species_common_name[i], "\n")

    if (spp$species_common_name[i] %in% c("petrale sole"))
      save_gg_objects <- TRUE
    else
      save_gg_objects <- FALSE

    make_pages(dat, spp$species_common_name[i],
      include_map_square = FALSE,
      resolution = 160,
      save_gg_objects = save_gg_objects,
      survey_cols = c(RColorBrewer::brewer.pal(5L, "Set1"),
        RColorBrewer::brewer.pal(8L, "Set1")[7:8],
        "#303030", "#a8a8a8", "#a8a8a8", "#a8a8a8")
    )

  } else {
    cat(crayon::green(clisymbols::symbol$tick),
      "Figure pages for", spp$species_common_name[i], "already exist\n")
  }
}
# }, .parallel = TRUE)
})

# ------------------------------------------------------------
temp <- lapply(spp$species_common_name[c(1, 17, 19)], function(x) {
  spp_file <- gfsynopsis:::clean_name(x)
  spp_title <- gfsynopsis:::all_cap(x)
  out <- list()
  latin_name <- spp$species_science_name[spp$species_common_name == x]
  sar <- spp$sar[spp$species_common_name == x]
  resdoc <- spp$resdoc[spp$species_common_name == x]
  species_code <- spp$species_code[spp$species_common_name == x]
  other_ref <- spp$other_ref_cite[spp$species_common_name == x]
  resdoc_text <- if (grepl(",", resdoc)) "Last Research Documents: " else "Last Research Document: "
  sar_text <- if (grepl(",", sar)) "Last Science Advisory Reports: " else "Last Science Advisory Report: "

  i <- 1
  out[[i]] <- "\\clearpage"
  i <- i + 1
  out[[i]] <- "\\begin{minipage}[t][4cm][t]{\\textwidth}"
  i <- i + 1
  out[[i]] <- paste0("\\subsection{", spp_title, "}")
  i <- i + 1
  out[[i]] <- paste0(gfsynopsis:::emph(latin_name),
    "\\, / DFO species code: ", species_code, "\n")
  i <- i + 1
  out[[i]] <- "\\vspace{8pt}"
  i <- i + 1
  out[[i]] <- paste0(resdoc_text, resdoc, "\n")
  i <- i + 1
  out[[i]] <- "\\vspace{8pt}"
  i <- i + 1
  out[[i]] <- paste0(sar_text, sar)
  i <- i + 1

  if (other_ref != "") {
    out[[i]] <- "\n \\vspace{8pt}"
    i <- i + 1
    out[[i]] <- paste0(other_ref)
    i <- i + 1
  }

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
