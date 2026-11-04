# Temporary demo: impact of fixing the legacy HBLL index bug in calc.biomass()
# (report/Norm-SurveyIndices.r), where this line filled strata by position
# rather than by stratum code:
#   mu[sa$GROUPING_CODE %in% names(mb)] <- mb
#
# old: legacy values, cached $survey_index in data-cache-2026-07/<species>.rds
# new: gfdata::design_indexes (gfdata >= 0.1.15)
# The set data are also used to replay the buggy and fixed point-estimate
# calculations, to show which old-vs-new differences the bug accounts for.
#
# Run: Rscript report/hbll-bug-impact.R

# Settings --------------------------------------------------------------------
species <- c("quillback rockfish", "yelloweye rockfish") # add species here
repo <- "/Users/seananderson/src/gfsynopsis-2025"
out_dir <- file.path(repo, "report")
cache <- file.path(repo, "report/data-cache-2026-07")

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})
options(width = 150)

frames <- list(
  "HBLL OUT N" = 321:323, "HBLL OUT S" = 324:326,
  "HBLL INS N" = 317:318, "HBLL INS S" = 319:320
)
surveys <- names(frames)
tol <- 1e-3 # relative tolerance for calling two estimates the same

# Old and new indexes ---------------------------------------------------------
old <- lapply(species, \(sp) {
  readRDS(file.path(cache, paste0(gsub(" ", "-", sp), ".rds")))$survey_index
}) |>
  bind_rows() |>
  filter(survey_abbrev %in% surveys) |>
  transmute(species_common_name, survey_abbrev, year,
    old = biomass, old_lo = lowerci, old_hi = upperci, old_cv = re,
    old_sets = num_sets
  )

new <- gfdata::design_indexes |>
  filter(species_common_name %in% species, survey_abbrev %in% surveys) |>
  transmute(species_common_name, survey_abbrev, year,
    new = biomass, new_lo = lowerci, new_hi = upperci, new_cv = cv_boot
  )

# Replay the point estimates --------------------------------------------------
sets <- readRDS(file.path(cache, "survey-sets.rds")) |>
  filter(
    species_common_name %in% species, survey_abbrev %in% surveys,
    !is.na(grouping_code), usability_code %in% c(0, 1, 2, 6),
    year < 2025 # no legacy values, and 2025 uses temporary stratum codes
  )

areas <- sets |>
  filter(!is.na(grouping_area_km2)) |>
  distinct(grouping_code, grouping_area_km2)
stopifnot(!anyDuplicated(areas$grouping_code))
area_of <- setNames(areas$grouping_area_km2, areas$grouping_code)

replay_one <- function(d, frame) {
  a <- area_of[as.character(frame)]
  mb <- tapply(d$density_ppkm2, list(d$grouping_code), mean)

  # As in calc.biomass(); R warns when mb is longer than the slots it fills
  buggy <- numeric(length(frame))
  suppressWarnings(buggy[frame %in% as.numeric(names(mb))] <- mb)

  # The fix: look up by stratum code; surveyed strata with no sets contribute 0
  fixed <- mb[as.character(frame)]
  fixed[is.na(fixed)] <- 0

  # Same positional fill, but using only the means of out-of-frame strata
  mb_out <- mb[!as.numeric(names(mb)) %in% frame]
  out_only <- rep(NA_real_, length(frame))
  if (length(mb_out) >= length(frame)) out_only <- mb_out[seq_along(frame)]

  tibble(
    buggy = sum(buggy * a), fixed = sum(fixed * a), out_only = sum(out_only * a),
    n_sets = nrow(d), n_out_of_frame = sum(!d$grouping_code %in% frame),
    out_of_frame_codes = paste(sort(unique(d$grouping_code[!d$grouping_code %in% frame])), collapse = " ")
  )
}

replay <- sets |>
  group_by(species_common_name, survey_abbrev, year) |>
  group_modify(\(d, k) replay_one(d, frames[[k$survey_abbrev]])) |>
  ungroup()

same <- function(a, b) !is.na(a) & !is.na(b) & abs(a - b) <= tol * pmax(abs(b), 1)

res <- full_join(old, new, by = c("species_common_name", "survey_abbrev", "year")) |>
  left_join(replay, by = c("species_common_name", "survey_abbrev", "year")) |>
  mutate(
    ratio = new / old,
    cause = case_when(
      is.na(buggy) ~ "new year (no legacy value)",
      same(new, old) ~ "unchanged",
      n_out_of_frame > 0 & same(buggy, old) ~ "bug: wrong strata by position",
      n_out_of_frame > 0 & same(out_only, old) ~ "bug: used only out-of-frame means",
      n_out_of_frame > 0 & is.na(old) & buggy == 0 ~ "bug: legacy estimate 0, index missing",
      TRUE ~ "other (not the bug)"
    ),
    bug = startsWith(cause, "bug")
  ) |>
  arrange(species_common_name, survey_abbrev, year)

# Checks and summaries --------------------------------------------------------
cat("\nFixed replay matches gfdata::design_indexes:",
  sum(same(res$fixed, res$new), na.rm = TRUE), "of", sum(!is.na(res$fixed)), "survey-years\n")
cat("Year classification:\n")
print(as.data.frame(count(res, species_common_name, cause)))

fmt <- res |>
  filter(bug | cause == "other (not the bug)") |>
  transmute(species_common_name, survey_abbrev, year, n_sets, n_out_of_frame,
    out_of_frame_codes,
    old = round(old / 1e6, 2), new = round(new / 1e6, 2), ratio = round(ratio, 2),
    old_cv = round(old_cv, 3), new_cv = round(new_cv, 3), cause
  )
cat("\nSurvey-years that changed (index in millions; ratio = new / old):\n")
print(as.data.frame(fmt))

cat("\nBy species and survey:\n")
print(as.data.frame(res |>
  filter(!is.na(old), !is.na(new)) |>
  group_by(species_common_name, survey_abbrev) |>
  summarise(
    years = n(), years_changed_by_bug = sum(bug),
    min_ratio = round(min(ratio), 2), max_ratio = round(max(ratio), 2),
    cor_log_old_new = round(cor(log(old), log(new)), 3), .groups = "drop"
  )))

write.csv(res, file.path(out_dir, "hbll-bug-impact.csv"), row.names = FALSE)

# Plot ------------------------------------------------------------------------
long <- bind_rows(
  res |> transmute(species_common_name, survey_abbrev, year, version = "Old (legacy)",
    est = old, lo = old_lo, hi = old_hi),
  res |> transmute(species_common_name, survey_abbrev, year, version = "New (corrected)",
    est = new, lo = new_lo, hi = new_hi)
) |>
  filter(!is.na(est)) |>
  mutate(
    version = factor(version, c("New (corrected)", "Old (legacy)")),
    x = year + ifelse(version == "Old (legacy)", -0.22, 0.22),
    across(c(est, lo, hi), \(v) v / 1e6),
    panel = factor(paste0(species_common_name, ": ", survey_abbrev),
      levels = as.vector(t(outer(species, surveys, paste, sep = ": "))))
  )

panel_of <- function(d) {
  factor(paste0(d$species_common_name, ": ", d$survey_abbrev), levels = levels(long$panel))
}
flag <- filter(res, bug)
flag$panel <- panel_of(flag)
labs_df <- long |>
  group_by(panel) |>
  summarise(top = max(hi, est, na.rm = TRUE), .groups = "drop") |>
  right_join(flag, by = "panel") |>
  rowwise() |>
  mutate(
    y = max(c(old_hi, new_hi, old, new) / 1e6, na.rm = TRUE) + 0.04 * top,
    label = if (is.na(ratio)) "no old" else sprintf("×%.2f", ratio)
  ) |>
  ungroup()

ink <- "#0b0b0b"; ink2 <- "#52514e"; surface <- "#fcfcfb"
g <- ggplot(long, aes(x, est, colour = version, shape = version)) +
  geom_rect(
    data = flag,
    aes(xmin = year - 0.65, xmax = year + 0.65, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE, fill = "#f0efec", colour = NA
  ) +
  geom_line(linewidth = 0.5, alpha = 0.7) +
  geom_linerange(aes(ymin = lo, ymax = hi), linewidth = 0.5, na.rm = TRUE) +
  geom_point(size = 1.9, stroke = 0.8, fill = surface) +
  ggrepel::geom_text_repel(
    data = labs_df, aes(year, y, label = label), inherit.aes = FALSE,
    size = 2.7, colour = ink2, direction = "y", nudge_y = 0, box.padding = 0.12,
    point.size = NA, min.segment.length = Inf, seed = 1, max.overlaps = Inf
  ) +
  facet_wrap(~panel, ncol = length(surveys), scales = "free_y") +
  scale_colour_manual(values = c("New (corrected)" = "#2a78d6", "Old (legacy)" = "#eb6834")) +
  scale_shape_manual(values = c("New (corrected)" = 16, "Old (legacy)" = 21)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Shaded years were affected by the bug; labels give new / old. Bars show 95% bootstrap intervals.",
    x = NULL, y = "Design-based index (millions)", colour = NULL, shape = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.background = element_rect(fill = surface, colour = NA),
    panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#e8e7e3", linewidth = 0.3),
    panel.spacing = unit(0.9, "lines"),
    axis.text = element_text(colour = ink2), axis.title = element_text(colour = ink2),
    strip.text = element_text(colour = ink, hjust = 0, size = 8.5),
    plot.title = element_text(colour = ink, face = "bold"),
    legend.position = "top", legend.justification = "left",
    legend.text = element_text(colour = ink2)
  )

fig <- file.path(out_dir, "hbll-bug-impact.png")
ggsave(fig, g, width = 12, height = 1 + 2.8 * length(species), dpi = 170, bg = surface)
cat("\nWrote", fig, "\n")
