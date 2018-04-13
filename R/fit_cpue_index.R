# # library(gfplot)
# load_all("../gfplot/")
# library(ggplot2)
# library(dplyr)
#
# d_cpue_index <- readRDS("../gfplot/inst/pcod-cache/pbs-cpue-index.rds")
#
# # test:
# # pbs_areas[grep("5[ABCDE]+|3[CD]+", pbs_areas$major_stat_area_description), ]
#
# areas <- c("3[CD]+", "5[CD]+", "5[AB]+")
#
# cpue_models <- lapply(areas, function(area) {
#   message("Determining qualified fleet for area ", area, ".")
#   fleet <- tidy_cpue_index(d_cpue_index,
#     year_range = c(1996, 2017),
#     species_common = "pacific cod",
#     area_grep_pattern = area,
#     min_positive_tows = 100,
#     min_positive_trips = 4,
#     min_yrs_with_trips = 4,
#     lat_band_width = 0.2,
#     depth_band_width = 50,
#     clean_bins = TRUE,
#     depth_bin_quantiles = c(0.02, 0.98),
#     lat_bin_quantiles = c(0.01, 0.99)
#   )
#   message("Fitting standardization model for area ", area, ".")
#   m_cpue <- fit_cpue_index(fleet)
#   list(model = m_cpue, fleet = fleet, area = area)
# })
#
# jks <- lapply(cpue_models, function(x) {
#   plot_cpue_index_jk(x$model) + labs(title = gsub("\\[|\\]|\\+", "", x$area))
# })
#
# lapply(jks, print)
#
# out <- purrr::map_df(cpue_models, function(x) {
#   p <- predict_cpue_index(x$model, center = FALSE)
#   p$area <- gsub("\\[|\\]|\\+", "", x$area)
#   p
# })
#
# out_centered <- purrr::map_df(cpue_models, function(x) {
#   p <- predict_cpue_index(x$model, center = TRUE)
#   p$area <- gsub("\\[|\\]|\\+", "", x$area)
#   p
# })
#
# plot_cpue_facet <- function(dat, scales = "free_y") {
#   dat %>%
#     ggplot(aes(year, est, ymin = lwr, ymax = upr, fill = model)) +
#     geom_ribbon(alpha = 0.3) +
#     geom_line() +
#     facet_grid(model~area, scales = scales) +
#     theme_pbs() +
#     ylab("Estimate") + xlab("Year") +
#     guides(fill = FALSE)
# }
#
# plot_cpue_facet(out) +
#   scale_fill_brewer(palette = "Set2")
#
# plot_cpue_facet(filter(out_centered, model == "Combined")) +
#   facet_wrap(~area, scales = "free", ncol = 1) +
#   scale_fill_manual(values = "black")
#
#
#
# areas <- c("3[CD]+", "5[CDE]+", "5[AB]+")
# fleet <- tidy_cpue_index(d_cpue_index,
#   year_range = c(1996, 2017),
#   species_common = "pacific cod",
#   area_grep_pattern = areas[1],
#   min_positive_tows = 100,
#   min_positive_trips = 4,
#   min_yrs_with_trips = 4,
#   lat_band_width = 0.2,
#   depth_band_width = 50,
#   clean_bins = TRUE,
#   depth_bin_quantiles = c(0.02, 0.98),
#   lat_bin_quantiles = c(0.01, 0.99)
# )
#
# # system.time({
# #   m_cpue <- fit_cpue_index(fleet)
# # })
# #
# fleet$month <- as.numeric(as.character(fleet$month))
# fleet$latitude <- as.numeric(as.character(fleet$latitude))
# fleet$depth <- as.numeric(as.character(fleet$depth))
#
# library(mgcv)
# system.time({
#   m <- gamm(spp_catch/hours_fished ~ year_factor + s(month, bs = "cc") +
#       s(depth) + s(latitude), family = Tweedie(p = 1.606, link = "log"),
#     random = list(vessel=~1),
#     data = fleet)
# })
#
#
# system.time({
#   m <- bam(spp_catch/hours_fished ~ year_factor + s(month, bs = "cc") +
#       f(vessel) +
#       f(locality) + s(depth) + s(latitude), family = tw(link = "log"),
#     data = fleet)
# })
#
# # predict_cpue_index(m_cpue, center = TRUE) %>% plot_cpue_index()
# #
# # p <- predict(m, newdata = data.frame(
# #   year_factor = unique(fleet$year_factor),
# #   month = median(fleet$month),
# #   vessel = names(rev(sort(table(fleet$vessel))))[[1]],
# #   locality = names(rev(sort(table(fleet$locality))))[[1]],
# #   depth = median(fleet$depth),
# #   latitude = median(fleet$latitude)
# #   ),
# #   type = "link",
# #   se.fit = TRUE)
# #
# # p <- predict(m, newdata = data.frame(
# #   year_factor = unique(fleet$year_factor),
# #   month = fleet$month[1],
# #   vessel = fleet$vessel[1],
# #   locality = fleet$locality[1],
# #   depth = fleet$depth[1],
# #   latitude = fleet$latitude[1]
# # ),
# #   type = "link",
# #   se.fit = TRUE)
# #
# # fit <- p$fit
# # se <- p$se.fit
# # fit <- fit - mean(fit)
# #
# # plot(seq_along(fit), exp(fit), type = "l")
# # polygon(c(seq_along(fit), rev(seq_along(fit))),
# #   exp(c(fit - 2 * se, rev(fit + 2 * se))), col = "#00000020",
# #   border = NA)
# #
# # library(dplyr)
# # pp <- predict_cpue_index(m_cpue, center = TRUE) %>% filter(model == "Combined")
# # lines(seq_along(pp$year), pp$est, col = "red")
