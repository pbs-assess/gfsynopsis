fit_survey_maps <- function(dat,
  species = "pacific cod", include_depth = TRUE,
  model = c("glmmfields", "inla"),
  surveys = c("SYN QCS", "SYN HS", "SYN WCHG", "SYN WCVI"),
  years = c(2016, 2017),
  ...) {
  dat <- dplyr::filter(dat, species_common_name == species)
  model <- match.arg(model)
  out <- lapply(surveys, function(surv) {
    message("Fitting model for the survey ", surv)

    if (model == "inla") {
      fit_survey_sets(dat, survey = surv, years = years,
        model = "inla", mcmc_posterior_samples = 800,
        ...)
    } else {
      fit_survey_sets(dat, survey = surv, years = years,
        model = "glmmfields", chains = 1, iter = 800,
        mcmc_posterior_samples = 300, n_knots = 25)
    }
  })

  raw_dat <- tidy_survey_sets(dat, surveys,
    years = years
  )

  pred_dat <- purrr::map_df(out, function(x) x$predictions)
  # pred_dat$combined <- pred_dat$combined * 1e6

  list(pred_dat = pred_dat, raw_dat = raw_dat, models = out,
    species = species, include_depth = include_depth)
}

plot_survey_maps <- function(pred_dat, raw_dat, show_axes = FALSE, ...) {
  plot_survey_sets(pred_dat, raw_dat,
    fill_column = "combined", show_model_predictions = TRUE,
    pos_pt_col = "#FFFFFF60",
    bin_pt_col = "#FFFFFF40",
    pos_pt_fill = "#FFFFFF05",
    show_raw_data = TRUE,
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
