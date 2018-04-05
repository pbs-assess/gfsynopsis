plot_catches <- function(dat) {

  catch_areas <- gfplot::tidy_catch(dat, areas = c("5[CDE]+", "5[AB]+", "3[CD]+"))
  catch_all <- gfplot::tidy_catch(dat, areas = NULL)
  catch <- bind_rows(catch_all, catch_areas)
  catch$area <- factor(catch$area,
    levels = c("Coastwide", "5AB", "5CDE", "3CD"))

  yrs <- range(catch$year)

  g <- gfplot::plot_catch(catch) +
    # ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2)) +
    theme(panel.spacing = unit(-0.1, "lines")) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
    ) +
    scale_x_continuous(breaks = seq(0, yrs[2], 10))


  gdat <- ggplot2::ggplot_build(g)$data[[1]]
  max_val <- max(gdat$ymax)

  labs <- unique(select(catch, area))
  g <- g + geom_text(
      data = labs, x = yrs[1] + 0.5, y = max_val * 0.9,
      aes_string(label = "area"),
      inherit.aes = FALSE, colour = "grey30", size = 3, hjust = 0
    )
  g

}

# dat <- readRDS("report/data-cache/pbs-catch.rds")
# dat <- dplyr::filter(dat, species_common_name == "pacific cod")
# gfsynopsis::plot_catches(dat)
