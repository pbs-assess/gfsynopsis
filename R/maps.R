# cache             <- file.path("report", "data-cache2")
# dat     <- readRDS(file.path(cache, "pbs-survey-sets.rds"))
# library(dplyr)
# dat     <- filter(dat, species_common_name == "yelloweye rockfish")
# dat <- filter(dat, survey_abbrev %in% c("HBLL OUT N", "HBLL OUT S"))
# dat$survey_abbrev <- "HBLL OUT"
# dat$year <- 2017
# m <- fit_survey_sets(dat, survey = "HBLL OUT", years = 2017,
#   model = "inla", mcmc_posterior_samples = 200, include_depth = TRUE,
#   verbose = TRUE,
#   premade_grid = gfplot::hbll_grid, density_column = "density_ppkm2"
# )
# raw_dat <- tidy_survey_sets(dat, "HBLL OUT",
#   years = 2017, density_column = "density_ppkm2"
# )
# pred_dat <- m$predictions
# # pred_dat$combined <- pred_dat$combined * 1e6
#
#
# plot_survey_maps(pred_dat, raw_dat, show_axes = TRUE, show_raw_data = T,
#   pos_pt_col = "#FFFFFF25",
#   bin_pt_col = "#FFFFFF05",
#   pos_pt_fill = "#FFFFFF03")
#

# cache             <- file.path("report", "data-cache2")
# dat     <- readRDS(file.path(cache, "pbs-survey-sets.rds"))
# library(dplyr)
# dat     <- filter(dat, species_common_name == "yelloweye rockfish")
# dat <- filter(dat, survey_abbrev %in% c("IPHC FISS"))
# dat <- filter(dat, year %in% 2017)
#
# pred_grid <- select(dat, longitude, latitude, depth_m) %>%
#   rename(X = longitude, Y = latitude, depth = depth_m)
# pred_grid <- list(grid = pred_grid, cell_area = 1.0)
# nrow(pred_grid)
# m <- fit_survey_sets(dat, survey = "IPHC FISS", years = 2017,
#   model = "inla", mcmc_posterior_samples = 200, include_depth = TRUE,
#   verbose = TRUE,
#   premade_grid = pred_grid, density_column = "density_ppkm2"
# )
# raw_dat <- tidy_survey_sets(dat, "IPHC FISS",
#   years = 2017, density_column = "density_ppkm2"
# )
# pred_dat <- m$predictions
#
# plot_survey_maps(pred_dat, raw_dat, show_axes = TRUE, show_raw_data = F,
#   cell_size = 2.8, circles = TRUE)

# , "HBLL OUT", "IPHC FISS"

fit_survey_maps <- function(dat,
  species = "pacific cod", include_depth = TRUE,
  model = c("inla", "glmmfields"),
  surveys = c("SYN QCS", "SYN HS", "SYN WCHG", "SYN WCVI"),
  years = c(2016, 2017),
  ...) {
  dat <- dplyr::filter(dat, species_common_name %in% species)
  dat <- dplyr::filter(dat, year %in% years)

  model <- match.arg(model)
  out <- lapply(surveys, function(surv) {
    if (!surv %in% c("HBLL OUT", "IPHC FISS", "SYN QCS", "SYN HS", "SYN WCHG", "SYN WCVI"))
      stop("survey value was '", surv, "' but must be one of ",
        "c('HBLL OUT', 'IPHC FISS', 'SYN QCS', 'SYN HS', 'SYN WCHG', 'SYN WCVI')")
    message("Fitting model for the survey ", surv)

    if (surv == "HBLL OUT") {
      density_column <- "density_ppkm2"
      .dat <- filter(dat, survey_abbrev %in% c("HBLL OUT N", "HBLL OUT S"))
      .dat$survey_abbrev <- "HBLL OUT"
      .dat$year <- years[2]
      premade_grid <- gfplot::hbll_grid
      raw_dat <- tidy_survey_sets(.dat, "HBLL OUT",
        years = years[2], density_column = density_column
      )
    }
    if (surv == "IPHC FISS") {
      density_column <- "density_ppkm2"
      .dat <- filter(dat, year %in% years[2]) # just last year
      .dat <- filter(.dat, survey_abbrev %in% surv)
      raw_dat <- tidy_survey_sets(.dat, surv,
        years = years[2], density_column = density_column
      )
      premade_grid <- select(raw_dat, X, Y, depth)
      premade_grid <- list(grid = premade_grid, cell_area = 1.0)
    }
    if (surv %in% c("SYN QCS", "SYN HS", "SYN WCHG", "SYN WCVI")) {
      density_column <- "density_kgpm2"
      .dat <- filter(dat, survey_abbrev %in% surv)
      premade_grid <- NULL
      raw_dat <- tidy_survey_sets(.dat, surv,
        years = years, density_column = "density_kgpm2"
      )
    }

    if (model == "inla") {
      model <- fit_survey_sets(.dat, survey = surv, years = years,
        model = "inla", mcmc_posterior_samples = 800,
        density_column = density_column,
        premade_grid = premade_grid,
        ...)
    } else {
      stop("NEED TO CHECK GLMMFIELDS")
      model <- fit_survey_sets(.dat, survey = surv, years = years,
        model = "glmmfields", chains = 1, iter = 800,
        mcmc_posterior_samples = 300, n_knots = 25, ...)
    }
    list(model = model, raw_dat = raw_dat)
  })

  pred_dat <- purrr::map_df(out, function(x) x$model$predictions)
  raw_dat  <- purrr::map_df(out, function(x) x$raw_dat)
  models   <- purrr::map(out,    function(x) x$model)

  list(pred_dat = pred_dat, models = models, raw_dat = raw_dat,
    species = species, include_depth = include_depth)
}

plot_survey_maps <- function(pred_dat, raw_dat, show_axes = FALSE,
  show_raw_data = TRUE, pos_pt_col = "#FFFFFF60",
  bin_pt_col = "#FFFFFF40",
  pos_pt_fill = "#FFFFFF05",
  ...) {
  plot_survey_sets(pred_dat, raw_dat,
    fill_column = "combined", show_model_predictions = TRUE,
    show_raw_data = show_raw_data,
    pos_pt_col = pos_pt_col,
    bin_pt_col = bin_pt_col,
    pos_pt_fill = pos_pt_fill,
    fill_scale =
      viridis::scale_fill_viridis(trans = "sqrt", option = "C"),
    colour_scale =
      viridis::scale_colour_viridis(trans = "sqrt", option = "C"),
    rotation_center = c(500, 5700), rotation_angle = 40, north_symbol = TRUE,
    xlim = c(375, 680), ylim = c(5200, 6150), x_buffer = 0, y_buffer = 0,
    north_symbol_coord = c(130, 5975), show_axes = show_axes,
    extrapolate_depth = TRUE, ...
  ) + theme(legend.position = "bottom") +
    guides(fill = FALSE, size = FALSE) +
    ggplot2::annotate("text", 390, 6090, label = "2016", col = "grey30") +
    ggplot2::annotate("text", 390, 5800, label = "2017", col = "grey30") +
    ggplot2::annotate("text", 390, 5450, label = "2016", col = "grey30")
}

# d_survey_sets <- readRDS("report/data-cache/pbs-survey-sets.rds")
#
# z <- get_spp_names() %>% dplyr::filter(type == "A") %>%
#   dplyr::pull(species_common_name) %>% `[`(6)
#
# zz <- lapply(z, function(x) {
#   message("Fitting model for ", x, "...")
#   fit_survey_maps(d_survey_sets, species = x, model = "inla", plot = TRUE,
#     verbose = TRUE)
# })
#
# simple_cap <- function(x) {
#   s <- strsplit(x, " ")[[1]]
#   paste(toupper(substring(s, 1, 1)), substring(s, 2),
#     sep = "", collapse = " ")
# }
#
# # plot_survey_maps(zz[[2]]$pred_dat, zz[[2]]$raw_dat)
#
# for (i in 1) {
#   g <- plot_survey_maps(zz[[i]]$pred_dat, zz[[i]]$raw_dat) +
#     ggplot2::ggtitle(simple_cap(zz[[i]]$species))
#   # print(g)
#   ggplot2::ggsave(paste0("report/",
#     gsub(" ", "-", zz[[i]]$species), "-inla-tighter-priors-map.pdf"),
#     plot = g, width = 2.9, height = 8)
# }
#
# x <- zz[[1]]$models[[1]]$models$pos$model
# x$mode$mode.status
# #
# # # https://github.com/timcdlucas/INLAutils/blob/1f18f089f1a5c5b2edfb03f813dd1b7f2e5e65f4/R/autoplot_inla.R
# extractPriors <- function(x){
#   priors <- data.frame(var = x$names.fixed, mean = NA, prec = NA)
#   row.names(priors) <- x$names.fixed
#   priors['(Intercept)', 2:3] <- c(x$.args$control.fixed$mean.intercept, x$.args$control.fixed$prec.intercept)
#
#   # find and combine prior means
#   if(length(x$.args$control.fixed$mean) == 1){
#     priors$mean[!priors$var == '(Intercept)'] <- x$.args$control.fixed$mean
#   } else if(length(x$.args$control.fixed$mean) == length(x$names.fixed) - 1) {
#     priors$mean[names(x$.args$control.fixed$mean)] <- unlist(x$.args$control.fixed$mean)
#   } else {
#     priors$mean[!priors$var == '(Intercept)'] <- x$.args$control.fixed$mean$default
#     # Take mean values that are not defulat
#     nondef <- unlist(x$.args$control.fixed$mean)[names(x$.args$control.fixed$mean) != 'default']
#     priors[names(nondef), 'mean'] <- x$.args$control.fixed$mean[[1]]
#   }
#
#   # find and combine prior prec
#   if(length(x$.args$control.fixed$prec) == 1){
#     priors$prec[!priors$var == '(Intercept)'] <- x$.args$control.fixed$prec
#   } else if(length(x$.args$control.fixed$prec) == length(x$names.fixed) - 1) {
#     priors$prec[names(x$.args$control.fixed$prec)] <- unlist(x$.args$control.fixed$prec)
#   } else {
#     priors$prec[!priors$var == '(Intercept)'] <- x$.args$control.fixed$prec$default
#     # Take mean values that are not defulat
#     nondef <- unlist(x$.args$control.fixed$prec)[names(x$.args$control.fixed$prec) != 'default']
#     priors[names(nondef), 'mean'] <- x$.args$control.fixed$prec[[1]]
#   }
#
#   priors
# }
# extractPriors(x)
#
#
#
# x$all.hyper
#
# x$all.hyper$family
# # [[1]]$hyper$theta$fixed
# # [1] FALSE
# # attr(,"inla.read.only")
# # [1] FALSE
# #
# # [[1]]$hyper$theta$prior
# # [1] "loggamma"
# # attr(,"inla.read.only")
# # [1] FALSE
# #
# # [[1]]$hyper$theta$param
# # [1] 1.00 0.01
# # attr(,"inla.read.only")
# # [1] FALSE
#
#
