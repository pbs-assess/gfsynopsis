devtools::load_all("../gfplot/")
devtools::load_all(".")
library("dplyr")
library("ggplot2")

# ------------------------------------------------------------
dc <- file.path("report", "data-cache3-uncompressed")
spp <- get_spp_names() %>% filter(type == "A")
spp <- filter(spp, species_common_name != "pacific hake")

out_dat <- list()
plots <- list()
for (i in seq_along(spp$species_common_name)) {
# for (i in 3:7) {
  cat(crayon::green(clisymbols::symbol$tick),
    "Building figure for", spp$species_common_name[i], "\n")
  dat <- readRDS(paste0(file.path(dc, spp$spp_w_hyphens[i]), ".rds"))
  if (nrow(dat$age_precision) > 1) {
    out_dat[[i]] <- gfplot::tidy_age_precision(dat$age_precision)
    out_dat[[i]]$species_common_name <-
      gfsynopsis:::first_cap(spp$species_common_name[[i]])
    plots[[i]] <- out_dat[[i]] %>%
      gfplot::plot_age_precision() +
      ggtitle(gfsynopsis:::first_cap(spp$species_common_name[[i]]))
  }
}

out <- bind_rows(out_dat)
set.seed(42)
jitter <- 0.2
jit <- stats::runif(nrow(out), -jitter, jitter)
out$prec_age <- out$prec_age + jit
out$prim_age <- out$prim_age + jit
out$prim_min_age <- out$prim_min_age + jit
out$prim_max_age <- out$prim_max_age + jit
out$prec_min_age <- out$prec_min_age + jit
out$prec_max_age <- out$prec_max_age + jit

g <- out %>% group_by(species_common_name) %>%
  do(if (nrow(.) > 300) sample_n(., 300) else .) %>% # downsample for clarity
  # fake data points to get 1-1 aspect ratio (they are invisible; coloured NA):
  do(data.frame(
    type = c(rep("real", length(.$prim_max_age)), "fake"),
    prim_max_age = c(.$prim_max_age, max(c(.$prim_max_age, .$prec_max_age))),
    prec_max_age = c(.$prec_max_age, max(c(.$prim_max_age, .$prec_max_age))),
    prim_min_age = c(.$prim_min_age, 0),
    prec_min_age = c(.$prec_min_age, 0),
    prec_age = c(.$prec_age, 0),
    prim_age = c(.$prim_age, 0),
    species_code = c(.$species_code, unique(.$species_code)),
    species_common_name = c(.$species_common_name, unique(.$species_common_name)),
    stringsAsFactors = FALSE
    )) %>%
  ungroup() %>%
  mutate(species_common_name = gsub("Rougheye\\/blackspotted Rockfish Complex",
    "Rougheye/\nblackspotted Rockfish", species_common_name)) %>%
  mutate(species_common_name =
      forcats::fct_reorder(species_common_name, as.numeric(as.factor(species_code)))) %>%
  ggplot(aes_string("prim_age", "prec_age", colour = "type")) +
  geom_point(pch = 21, alpha = 0.6) +
  ggplot2::geom_abline(intercept = 0, slope = 1, col = "grey50", lty = 2) +
  ggplot2::geom_segment(aes_string(
    x = "prim_min_age", xend = "prim_max_age",
    y = "prec_age", yend = "prec_age"),
    alpha = 0.4) +
  facet_wrap(~species_common_name, scales = "free") +
  ggplot2::geom_segment(aes_string(
    x = "prim_age", xend = "prim_age",
    y = "prec_min_age", yend = "prec_max_age"), alpha = 0.4) +
  labs(title = "Ageing precision", x = "Primary age (years)",
    y = "Secondary age (years)") +
  theme_pbs() +
  ggtitle("") +
  scale_colour_manual(values = c("real" = "grey25", "fake" = NA)) +
  guides(colour = FALSE)
ggplot2::ggsave("report/report/age-precision.png", width = 9, height = 8.5, dpi = 300)
