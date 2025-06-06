#' Plot catches
#'
#' @param dat The data
#' @param blank_plot Whether or not the plot should be blank.
#' @param xlim x limit
#' @param french Logical
#' @param area_labels Vector of panel labels for areas.
#' @param blank_non_coastwide Logical. If `TRUE`, all non-'coastwide' panels are
#'   left blank.
#' @param ... Other arguments to pass to [gfplot::plot_catch()].
#'
#' @export
plot_catches <- function(dat, blank_plot = FALSE, xlim = c(1955, 2020),
  french = FALSE, area_labels = c("Coastwide", "5CDE", "5AB", "3CD"), 
  blank_non_coastwide = FALSE, ...) {

  if (!blank_plot) {
    catch_areas <- gfplot::tidy_catch(dat, areas = c("5[CDE]+", "5[AB]+", "3[CD]+"))
    catch_all <- gfplot::tidy_catch(dat, areas = NULL)
    catch <- bind_rows(catch_all, catch_areas)

    if (blank_non_coastwide) {
      catch$value[catch$area != "Coastwide"] <- 0
    } 

    if (french) {
      catch$area <- as.character(catch$area)
        catch$area <- gsub("Coastwide", rosettafish::en2fr(area_labels[1]), catch$area)
        catch$area <- factor(catch$area,
          levels = c(rosettafish::en2fr(area_labels[1]), area_labels[2:length(area_labels)]))
    } else {
      catch$area <- gsub("Coastwide", area_labels[1], catch$area)
      catch$area <- factor(catch$area, levels = area_labels)
    }
  } else {
    catch <- dat
  }
  yrs <- xlim

  g <- gfplot::plot_catch(catch, xlim = xlim, french = french, units = NULL, ...) +
    # ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2)) +
    theme(panel.spacing = unit(-0.1, "lines")) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
    ) +
    scale_x_continuous(breaks = seq(0, yrs[2], 10))

  if (blank_plot)
    suppressMessages(g <- g + ylim(0, 1))

  gdat <- ggplot2::ggplot_build(g)$data
  # data_element <- which(unlist(lapply(lapply(gdat, names),
  #   function(x) "ymax" %in% x)))[[1]]
  # gdat <- gdat[[data_element]]
  # max_val <- max(gdat$ymax)
  max_val <- if (!blank_plot) max(gdat[[5]]$ymax) else 1

  labs <- unique(select(catch, area))

  if (isTRUE(french)) .mult <- 0.91 else .mult <- 0.905 # vertical area label
  g <- g + geom_text(
      data = labs, x = yrs[1] + 1.1, y = max_val * .mult,
      aes_string(label = "area"),
      inherit.aes = FALSE, colour = "grey30", size = 3, hjust = 0
    )
  g <- g + ggplot2::theme(legend.justification = c(0, 1),
    legend.position = c(0, 0.215),
    legend.background = element_blank(),
    legend.direction = "horizontal") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::ggtitle("Commercial catch")

  if (french) {
    g <- g + theme(legend.spacing.x = ggplot2::unit(0.5, "mm"))
    g <- g +
    scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) # e.g. 1 000
  }

  g
}

# dat <- readRDS("report/data-cache/pbs-catch.rds")
# dat <- dplyr::filter(dat, species_common_name == "pacific cod")
# gfsynopsis::plot_catches(dat)
