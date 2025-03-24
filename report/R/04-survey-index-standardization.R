# Cache stitched indices ----------------------------------------------

message("Cache stitched indexes")
if (is.null(shapefile)) {
  stitch_cache <- file.path("report", paste0("cache-", tag), "stitch-cache") # Stitched outputs
} else {
  stitch_cache <- file.path("report", paste0("cache-", tag), "spatial-stitch-cache") # Stitched outputs
}
dir.create(stitch_cache, showWarnings = FALSE, recursive = TRUE)

# Stitch inputs
model_type <- "st-rw"
spp_vector <- spp$species_common_name[order(spp$species_common_name)]
# randomize to avoid clumps of non-fitting in parallel:
set.seed(92729)
spp_vector <- sample(spp_vector, length(spp_vector))
# Synoptic/HBLL
bait_counts <- readRDS(file.path(dc, "bait-counts.rds"))

# Stitch surveys if not cached
source(here("report", "run-stitching.R"))

