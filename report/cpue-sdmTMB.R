fit_sdmTMB_cpue <- function(
    cpue_data_file, # e.g. here::here("report/data-cache-2024-05/cpue-index-dat.rds"),
    species,
    survey_grids = c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG"),
    final_year = as.numeric(format(Sys.Date(), "%Y")) - 1L,
    min_positive_tows = 100L,
    min_positive_trips = 5L,
    min_yrs_with_trips = 5L,
    raw_cpue_caching_file = NULL,
    index_shape_file = NULL, #needs to be read in as sf object
    plots = FALSE, silent = TRUE, return_raw_cpue = FALSE) {
  # library(dplyr)
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
      region = paste(survey_grids, collapse = "; "),
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

  # if (plots) {
  #   gfdata::survey_blocks |>
  #     filter(active_block) |>
  #     dplyr::filter(grepl("^SYN", survey_abbrev)) |>
  #     ggplot(aes(colour = survey_abbrev)) +
  #     geom_sf() +
  #     theme_minimal() +
  #     scale_colour_brewer(palette = "Dark2")
  # }

  grid <- gfdata::survey_blocks |>
    filter(active_block) |>
    dplyr::filter(grepl("^SYN", survey_abbrev))

  if (plots) {
    grid |>
      ggplot(aes(colour = survey_abbrev, fill = survey_abbrev)) +
      geom_sf() +
      theme_minimal() +
      scale_colour_brewer(palette = "Dark2")+
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

  grid_region <- dplyr::filter(grid, survey_abbrev %in% survey_grids)

  if (plots) {
    g <- grid_region |>
      ggplot(aes(colour = survey_abbrev)) +
      geom_sf() +
      theme_minimal() +
      scale_colour_brewer(palette = "Dark2")
    print(g)
  }

  intersected <- sf::st_intersects(dat_sf, grid_region)
  intersected_i <- which(lengths(intersected) > 0)
  dat_sf_reduced <- dat_sf[intersected_i, ]

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
  # grid_region_reduced <- grid_region # !!

  if (plots) {
    g <- grid_region_reduced |>
      ggplot() +
      geom_sf() +
      # geom_sf(data = dat_sf_reduced, alpha = 0.6, pch = ".", colour = "red") +
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
  gg$log_depth <- log(gg$depth_m)
  gg$vessel <- factor(NA)

  dat_reduced <- dat[intersected_i, ]

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

  dat <- dat_reduced # !!

  dat$area <- params$area_name

  dat$log_depth <- log(dat$depth_marmap)
  gg$log_depth <- log(gg$depth_marmap)
  dat$vessel <- as.factor(dat$vessel_registration_number)
  dat$month <- factor(dat$month)
  # dplyr::filter(dat, hours_fished > 2000) # 8762.05!
  dat <- dplyr::filter(dat, hours_fished < 2000)
  dat$depth_scaled <- (dat$log_depth - mean(dat$log_depth)) / sd(dat$log_depth)

  ret <- dat |>
    filter(!is.na(spp_catch), !is.na(hours_fished)) |>
    group_by(year) |>
    summarise(est_unstandardized = sum(spp_catch) / sum(hours_fished)) |>
    mutate(est_unstandardized = est_unstandardized /
      exp(mean(log(est_unstandardized))))
  ret$region <- paste(survey_grids, collapse = "; ")
  ret$species <- params$species
  if (return_raw_cpue) {
    return(ret)
  }
  saveRDS(ret, file = raw_cpue_caching_file, compress = FALSE)

  if (plots) {
    g <- ggplot(dat, aes(as.factor(year), (spp_catch + 1) / hours_fished)) +
      geom_boxplot() +
      scale_y_log10()
    print(g)
  }

  get_most_common_level <- function(x) {
    rev(names(sort(table(x))))[[1]]
  }
  base_month <- get_most_common_level(dat$month)

  dat$month <- stats::relevel(factor(dat$month), ref = base_month)
  dat$month_num <- as.numeric(dat$month)

  if (length(unique(dat$month)) >= 9) { # use month smoother
    f <- spp_catch ~ 0 + as.factor(year) +
      depth_scaled + I(depth_scaled^2) +
      (1 | vessel) + s(month_num, bs = "cc")
    mm <- stats::model.matrix(spp_catch ~ 0 + as.factor(year) +
      depth_scaled + I(depth_scaled^2), data = dat)
  } else {
    f <- spp_catch ~ 0 + as.factor(year) +
      depth_scaled + I(depth_scaled^2) +
      (1 | vessel) + as.factor(month)
    mm <- stats::model.matrix(spp_catch ~ 0 + as.factor(year) +
      depth_scaled + I(depth_scaled^2) + as.factor(month), data = dat)
  }

  if (nrow(dat) < 200) {
    return(NA_return)
  }

  .cutoff <- if (length(survey_grids) > 2) 25 else 10

  ## original mesh approach
  # mesh <- make_mesh(dat, c("X", "Y"), cutoff = if (length(survey_grids) > 2) 25 else 10)

  ## make a more even mesh using the grid
  mesh_from_grid <- make_mesh(gg, c("X", "Y"), cutoff = .cutoff)
  mesh <- make_mesh(dat, c("X", "Y"), mesh = mesh_from_grid$mesh)

  plot(mesh$mesh)
  mesh$mesh$n


  ## Further subseting of grid to new area of interest ----

  if (!is.null(index_shape_file)) {

  index_shape_file_sf <- sf::st_transform(index_shape_file, crs = sf::st_crs(grid))

  new_grid <- sf::st_intersects(grid_region_reduced, index_shape_file_sf)
  new_grid_i <- which(lengths(new_grid) > 0)
  index_grid <- grid_region_reduced[new_grid_i, ]

  if (nrow(index_grid) == 0L) {
      return(NA_return)
  } else {

  suppressWarnings(
    co <- sf::st_centroid(index_grid)
  )
  co <- sf::st_coordinates(co)
  gg <- data.frame(X = co[, 1] / 1000, Y = co[, 2] / 1000)
  gg$depth_m <- index_grid$depth_m
  gg$depth_marmap <- index_grid$depth_marmap
  gg$log_depth <- log(gg$depth_m)
  gg$vessel <- factor(NA)

  if (plots) {
    g <- ggplot(gg, aes(X, Y)) +
      geom_tile(fill = "grey60", width = 2, height = 2) +
      coord_fixed() +
      theme_light()
    print(g)
  }
  }
  }

  gg <- sdmTMB::replicate_df(gg, "year", time_values = sort(unique(dat$year)))
  gg$depth_scaled <- (gg$log_depth - mean(dat$log_depth)) / sd(dat$log_depth)
  gg$month <- factor(base_month, levels = levels(dat$month))
  gg$month_num <- as.numeric(base_month)

  tictoc::tic()
  fit <- tryCatch(sdmTMB::sdmTMB(
    f,
    knots = list(month_num = c(0.5, 12.5)),
    family = sdmTMB::delta_lognormal(),
    # family = sdmTMB::delta_lognormal(type = "poisson-link"),
    # family = sdmTMB::delta_gamma(),
    control = sdmTMBcontrol(profile = c("b_j", "b_j2"), multiphase = FALSE),
    # control = sdmTMBcontrol(multiphase = FALSE),
    priors = sdmTMBpriors(b = normal(rep(0, ncol(mm)), rep(20, ncol(mm)))),
    mesh = mesh,
    offset = log(dat$hours_fished),
    spatial = "on",
    spatiotemporal = "iid",
    data = dat,
    time = "year",
    anisotropy = TRUE,
    predict_args = list(newdata = gg, re_form_iid = NA),
    index_args = list(area = rep(4, nrow(gg))),
    do_index = TRUE,
    silent = silent
  ), error = function(e) NA)
  tictoc::toc()

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

  if (!all(unlist(s))) {
    fit <- tryCatch(update(fit, spatiotemporal = "off"), error = function(e) NA)
    s <- sanity(fit)
  }
  if (length(fit) == 1L) {
    if (is.na(fit)) {
      return(NA_return)
    }
  }

  if (!all(unlist(s))) {
    return(NA_return)
  }
  fit

  do_expanion <- function(model) {
    ind <- get_index(model, bias_correct = TRUE, area = 4)
    ind$region <- paste(survey_grids, collapse = "; ")
    ind$species <- params$species
    ind
  }
  ind <- do_expanion(fit)

  if (plots) {
    g <- ind |> ggplot(aes(year, est, ymin = lwr, ymax = upr)) +
      geom_ribbon() +
      facet_wrap(~region, scales = "free_y") +
      ylim(0, NA)
    print(g)
  }

  ind
}
