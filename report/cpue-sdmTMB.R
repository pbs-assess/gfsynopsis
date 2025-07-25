fit_sdmTMB_cpue <- function(
    cpue_data_file, # e.g. report/data-cache-2025-03/cpue-index-dat.rds
    species,
    survey_grids_fit = c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG"),
    survey_grids_predict = list(
      c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG"),
      c("SYN HS", "SYN WCHG"),
      c("SYN QCS"),
      c("SYN WCVI")
    ),
    final_year = as.numeric(format(Sys.Date(), "%Y")) - 1L,
    min_positive_tows = 100L,
    min_positive_trips = 5L,
    min_yrs_with_trips = 5L,
    raw_cpue_caching_file = NULL,
    shapefile = NULL, # an sf polygon object; e.g. the Haida shape file
    plots = FALSE,
    silent = TRUE,
    return_raw_cpue = FALSE) {
  library(sdmTMB)
  library(sf)
  if (plots) library(ggplot2)

  if (require("RhpcBLASctl")) {
    RhpcBLASctl::blas_set_num_threads(1) # default currently is all/80!
    RhpcBLASctl::omp_set_num_threads(1) # default currently is all/80!
  }

  NA_return <-
    data.frame(
      year = NA, est = NA, lwr = NA, upr = NA, log_est = NA, se = NA,
      type = NA,
      # region = paste(survey_grids_fit, collapse = "; "),
      species = species,
      stringsAsFactors = FALSE
    )

  params <- list()
  params$area <- c("^5A|^5B|^5C|^5D|^5E|^3C|^3D")
  params$area_name <- c("Coastwide")
  params$min_positive_tows <- min_positive_tows
  params$min_positive_trips <- min_positive_trips
  params$min_yrs_with_trips <- min_yrs_with_trips
  params$final_year <- final_year
  params$depth_bin_quantiles <- c(0.001, 0.999)
  params$lat_range <- c(48, Inf)
  params$depth_range <- c(-Inf, Inf)
  params$species <- species

  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(params$species)))

  d1996 <- readr::read_rds(cpue_data_file)
  d1996$fishing_event_id_unique <- paste0(
    d1996$vessel_registration_number, "-",
    d1996$trip_id, "-", d1996$fishing_event_id
  )

  if (plots) {
    gfdata::survey_blocks |>
      filter(active_block) |>
      dplyr::filter(grepl("^SYN", survey_abbrev)) |>
      ggplot(aes(colour = survey_abbrev)) +
      geom_sf() +
      theme_minimal() +
      scale_colour_brewer(palette = "Dark2")
  }

  grid <- gfdata::survey_blocks |>
    filter(active_block) |>
    dplyr::filter(grepl("^SYN", survey_abbrev))

  if (plots) {
    grid |>
      ggplot(aes(colour = survey_abbrev, fill = survey_abbrev)) +
      geom_sf() +
      theme_minimal() +
      scale_colour_brewer(palette = "Dark2") +
      scale_fill_brewer(palette = "Dark2")
  }

  dat <- gfplot::tidy_cpue_index(
    d1996,
    species_common = tolower(params$species),
    gear = "bottom trawl",
    use_alt_year = FALSE,
    year_range = c(1996, params$final_year),
    lat_range = params$lat_range,
    min_positive_tows = params$min_positive_tows,
    min_positive_trips = params$min_positive_trips,
    min_yrs_with_trips = params$min_yrs_with_trips,
    depth_band_width = 1,
    area_grep_pattern = params$area,
    depth_bin_quantiles = params$depth_bin_quantiles,
    min_bin_prop = 0.001,
    lat_band_width = 0.02,
    return_raw_data = TRUE #<
  )

  if (nrow(dat) < 200) {
    if (!return_raw_cpue) {
      saveRDS(NA, file = raw_cpue_caching_file, compress = FALSE)
      return(NA_return)
    } else {
      return(NA)
    }
  }

  bathy <- marmap::getNOAA.bathy(
    lon1 = -138, lon2 = -120, lat1 = 47, lat2 = 57,
    resolution = 1, keep = TRUE
  )

  grid_ll <- sf::st_transform(grid, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  suppressWarnings({
    grid_ll_coord <- grid_ll |>
      sf::st_centroid() |>
      sf::st_coordinates()
  })

  x <- marmap::get.depth(bathy, grid_ll_coord[, 1:2], locator = FALSE) |>
    dplyr::mutate(depth_m = (depth * -1))
  grid$depth_marmap <- x$depth_m

  dat <- sdmTMB::add_utm_columns(dat, c("longitude", "latitude"), utm_crs = 32609, units = "km")
  x <- marmap::get.depth(bathy, dat[, c("longitude", "latitude")], locator = FALSE) |>
    dplyr::mutate(depth_m = (depth * -1))
  dat$depth_marmap <- x$depth_m

  grid <- dplyr::filter(grid, depth_marmap > 0)
  dat <- dplyr::filter(dat, depth_marmap > 0)
  dat_sf <- sf::st_as_sf(dat,
    coords = c("longitude", "latitude"),
    crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  )
  dat_sf <- sf::st_transform(dat_sf, crs = sf::st_crs(grid))

  grid_region <- dplyr::filter(grid, survey_abbrev %in% survey_grids_fit)

  if (plots) {
    g <- grid_region |>
      ggplot(aes(colour = survey_abbrev)) +
      geom_sf() +
      theme_minimal() +
      scale_colour_brewer(palette = "Dark2")
    print(g)
  }

  intersected <- sf::st_intersects(dat_sf, grid_region)

  ## Further sub-setting of data to new area of interest ----
  if (!is.null(shapefile)) {
    shapefile_sf <- sf::st_transform(shapefile, crs = sf::st_crs(grid))
    dat_reduced_intersected <- sf::st_intersects(dat_sf, shapefile_sf)
    intersected_i <- which(lengths(dat_reduced_intersected) > 0 & lengths(intersected) > 0)
    dat_sf_reduced <- dat_sf[intersected_i, ]
    dat_reduced <- dat[intersected_i, ]
  } else {
    dat_sf_reduced <- dat_sf[which(lengths(intersected) > 0), drop = FALSE]
    dat_reduced <- dat[which(lengths(intersected) > 0), , drop = FALSE]
  }
  dat_reduced$log_depth <- log(dat_reduced$depth_marmap)

  if (plots) {
    g <- dat_sf_reduced |>
      ggplot() +
      geom_sf(data = grid_region) +
      geom_sf(alpha = 0.6, pch = ".", colour = "red") +
      theme_light()
    print(g)
  }

  intersected_grid <- sf::st_intersects(grid_region, dat_sf_reduced)
  intersected_grid_i <- which(lengths(intersected_grid) > 0)
  grid_region_reduced <- grid_region[intersected_grid_i, ]

  ## Further subseting of grid to new area of interest if needed ----
  if (!is.null(shapefile)) {
    shapefile_sf <- sf::st_transform(shapefile, crs = sf::st_crs(grid))
    new_grid <- sf::st_intersects(grid_region_reduced, shapefile_sf)
    new_grid_i <- which(lengths(new_grid) > 0)
    grid_region_reduced <- grid_region_reduced[new_grid_i, ]
    if (nrow(grid_region_reduced) == 0L) {
      return(NA_return)
    }
  }

  if (plots) {
    g <- grid_region_reduced |>
      ggplot() +
      geom_sf() +
      theme_light()
    print(g)
  }

  suppressWarnings(
    co <- sf::st_centroid(grid_region_reduced)
  )
  co <- sf::st_coordinates(co)
  gg <- data.frame(X = co[, 1] / 1000, Y = co[, 2] / 1000)
  gg$depth_m <- grid_region_reduced$depth_m
  gg$depth_marmap <- grid_region_reduced$depth_marmap
  gg$survey_abbrev <- grid_region_reduced$survey_abbrev
  gg$log_depth <- log(gg$depth_marmap)
  gg$vessel <- factor(rep(NA, nrow(gg)))
  gg <- sdmTMB::replicate_df(gg, "year", time_values = sort(unique(dat_reduced$year)))
  gg$depth_scaled <- (gg$log_depth - mean(dat_reduced$log_depth)) / sd(dat_reduced$log_depth)

  if (plots) {
    g <- ggplot(gg, aes(X, Y)) +
      geom_tile(fill = "grey60", width = 2, height = 2) +
      geom_point(data = dat_reduced, colour = "red", size = .5, alpha = 0.1) +
      coord_fixed() +
      theme_light()
    print(g)
  }

  if (plots) {
    plot(dat_reduced$best_depth, dat_reduced$depth_marmap, pch = ".")
    abline(0, 1, col = "red")
    hist(dat_reduced$depth_marmap)
  }

  dat_reduced$area <- params$area_name
  gg$log_depth <- log(gg$depth_marmap)
  dat_reduced$vessel <- as.factor(dat_reduced$vessel_registration_number)
  dat_reduced$month <- factor(dat_reduced$month)
  # dplyr::filter(dat_reduced, hours_fished > 2000) # 8762.05!
  dat_reduced <- dplyr::filter(dat_reduced, hours_fished < 2000)
  dat_reduced$depth_scaled <- (dat_reduced$log_depth - mean(dat_reduced$log_depth)) / sd(dat_reduced$log_depth)

  ret <- dat_reduced |>
    filter(!is.na(spp_catch), !is.na(hours_fished)) |>
    group_by(year) |>
    summarise(est_unstandardized = sum(spp_catch) / sum(hours_fished)) |>
    mutate(est_unstandardized = est_unstandardized /
      exp(mean(log(est_unstandardized))))
  ret$region <- paste(survey_grids_fit, collapse = "; ")
  ret$species <- params$species
  if (return_raw_cpue) {
    return(ret)
  }
  saveRDS(ret, file = raw_cpue_caching_file, compress = FALSE)

  if (plots) {
    g <- ggplot(dat_reduced, aes(as.factor(year), (spp_catch + 1) / hours_fished)) +
      geom_boxplot() +
      scale_y_log10()
    print(g)
  }

  get_most_common_level <- function(x) {
    rev(names(sort(table(x))))[[1]]
  }
  base_month <- get_most_common_level(dat_reduced$month)

  dat_reduced$month <- stats::relevel(factor(dat_reduced$month), ref = base_month)
  dat_reduced$month_num <- as.numeric(dat_reduced$month)
  gg$month <- factor(base_month, levels = levels(dat_reduced$month))
  gg$month_num <- as.numeric(base_month)

  if (length(unique(dat_reduced$month)) >= 9) { # use month smoother
    f <- spp_catch ~ 0 + as.factor(year) +
      depth_scaled + I(depth_scaled^2) +
      (1 | vessel) + s(month_num, bs = "cc")
    mm <- stats::model.matrix(spp_catch ~ 0 + as.factor(year) +
      depth_scaled + I(depth_scaled^2), data = dat_reduced)
  } else {
    f <- spp_catch ~ 0 + as.factor(year) +
      depth_scaled + I(depth_scaled^2) +
      (1 | vessel) + as.factor(month)
    mm <- stats::model.matrix(spp_catch ~ 0 + as.factor(year) +
      depth_scaled + I(depth_scaled^2) + as.factor(month), data = dat_reduced)
  }

  if (nrow(dat_reduced) < 200) {
    return(NA_return)
  }

  .cutoff <- if (length(survey_grids_fit) > 3) 25 else 10

  ## make a more even mesh using the grid
  mesh_from_grid <- make_mesh(gg, c("X", "Y"), cutoff = .cutoff)
  mesh <- make_mesh(dat_reduced, c("X", "Y"), mesh = mesh_from_grid$mesh)

  if (plots) plot(mesh$mesh)

  fit <- tryCatch(
    sdmTMB::sdmTMB(
      f,
      knots = list(month_num = c(0.5, 12.5)),
      family = delta_lognormal(type = "poisson-link"),
      control = sdmTMBcontrol(profile = c("b_j", "b_j2"), multiphase = FALSE),
      priors = sdmTMBpriors(b = normal(rep(0, ncol(mm)), rep(20, ncol(mm)))),
      mesh = mesh,
      offset = log(dat_reduced$hours_fished),
      spatial = "on",
      spatiotemporal = "iid",
      data = dat_reduced,
      time = "year",
      anisotropy = TRUE,
      # predict_args = list(newdata = gg, re_form_iid = NA, offset = rep(0, nrow(gg))),
      # index_args = list(area = rep(4, nrow(gg))),
      # do_index = TRUE,
      silent = silent
    ),
    error = function(e) NA
  )

  if (length(fit) == 1L) {
    if (is.na(fit)) {
      return(NA_return)
    }
  }

  s <- sanity(fit)
  if (!all(unlist(s))) {
    fit <- tryCatch(update(fit, anisotropy = FALSE), error = function(e) NA)
    s <- sanity(fit)
  }
  if (length(fit) == 1L) {
    if (is.na(fit)) {
      return(NA_return)
    }
  }

  ## can't turn these off if predicting to subregions or trends will all be the same:
  # if (!all(unlist(s))) {
  #   fit <- tryCatch(update(fit, spatiotemporal = "off"), error = function(e) NA)
  #   s <- sanity(fit)
  # }
  # if (length(fit) == 1L) {
  #   if (is.na(fit)) {
  #     return(NA_return)
  #   }
  # }

  if (!all(unlist(s))) {
    return(NA_return)
  }

  ## need this to iterate over the survey_grids_predict elements
  do_expanion <- function(model, surveys_to_predict) {
    cli::cli_inform(paste("Predicting on", paste(surveys_to_predict, collapse = ", ")))
    this_grid <- filter(gg, survey_abbrev %in% surveys_to_predict)
    pred <- predict(
      model,
      newdata = this_grid,
      return_tmb_object = TRUE,
      re_form_iid = NA,
      offset = rep(0, nrow(this_grid))
    )
    ind <- get_index(pred, bias_correct = TRUE, area = 4)
    gc()
    ind$region <- paste(surveys_to_predict, collapse = "; ")
    ind$species <- params$species
    ind
  }
  gc()
  ind <- purrr::map_dfr(survey_grids_predict, \(s) do_expanion(fit, s))
  gc()

  if (plots) {
    g <- ind |> ggplot(aes(year, est, ymin = lwr, ymax = upr)) +
      geom_ribbon() +
      facet_wrap(~region, scales = "free_y") +
      ylim(0, NA)
    print(g)
  }

  ind
}

message("Fitting sdmTMB CPUE models")
cpue_cache <- file.path("report", paste0("cache-", tag), "cpue-sdmTMB-cache")
raw_cpue_cache <- file.path("report", paste0("cache-", tag), "raw-cpue-cache")
dir.create(cpue_cache, showWarnings = FALSE)
dir.create(raw_cpue_cache, showWarnings = FALSE)
xx <- spp$species_common_name
# set.seed(123)
# xx <- sample(xx, length(xx), replace = FALSE)
# xx[!xx %in% tolower(unique(d_cpue$species_common_name))]
furrr::future_walk(xx, function(.sp) {
  # shapefile <- NULL
# purrr::walk(xx, \(.sp) {
  spp_file <- gfsynopsis:::clean_name(.sp)
  cpue_cache_spp <- paste0(file.path(cpue_cache, spp_file), ".rds")
  regions <- list(
    c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG"),
    c("SYN HS", "SYN WCHG"),
    c("SYN QCS"),
    c("SYN WCVI")
  )
  if (!is.null(shapefile)) {
    regions[[1]] <- c("SYN QCS", "SYN HS", "SYN WCHG") # remove WCVI for Haida shapefile
    regions <- regions[1]
  }
  if (!file.exists(cpue_cache_spp)) {
    cat(.sp, "\n")
    .r <- gsub(" ", "-", paste(regions[[1]], collapse = "-"))
    .f <- paste0(file.path(raw_cpue_cache, paste0(spp_file, "-", .r)), ".rds")
    cpue_index <- fit_sdmTMB_cpue(
      cpue_data_file = file.path(dc, "cpue-index-dat.rds"),
      raw_cpue_caching_file = here::here(.f),
      survey_grids_fit = regions[[1]],
      survey_grids_predict = regions,
      final_year = 2024,
      plots = TRUE,
      species = .sp,
      shapefile = shapefile,
      silent = FALSE
    )
    saveRDS(cpue_index, file = cpue_cache_spp, compress = TRUE)
  }
})

# Old non-spatial models:

# if (parallel_processing) future::plan(future::multisession, workers = 4L)
# message("Fit CPUE models")
# cpue_cache <- file.path("report", "cpue-cache")
# dir.create(cpue_cache, showWarnings = FALSE)
# xx <- spp$species_common_name
# xx <- sample(xx, length(xx), replace = FALSE)
# furrr::future_walk(xx, function(.sp) {
# # purrr::walk(xx, function(.sp) {
#   spp_file <- gfsynopsis:::clean_name(.sp)
#   cpue_cache_spp <- paste0(file.path(cpue_cache, spp_file), ".rds")
#   if (!file.exists(cpue_cache_spp)) {
#     cat(.sp, "\n")
#     cpue_index <- gfsynopsis::fit_cpue_indices(
#       dat = d_cpue,
#       species = .sp,
#       save_model = .sp %in% example_spp,
#       parallel = FALSE
#     )
#     saveRDS(cpue_index, file = cpue_cache_spp, compress = FALSE)
#   }
# })
# future::plan(sequential)

# f <- list.files(raw_cpue_cache, full.names = TRUE)
# raw_cpue <- purrr::map_dfr(f, \(x) {
#   a <- readRDS(x)
#   if (!all(is.na(a[[1L]]))) a
# })
# raw_cpue <- filter(raw_cpue, is.finite(est_unstandardized))
# raw_cpue <- filter(raw_cpue, !is.na(est_unstandardized))
# raw_cpue <- NULL
