# Read in fresh data or load cached data if available -----------------

message("Loading data")
gfsynopsis::get_data(type = c("A", "B"), path = dc, force = FALSE)
spp <- gfsynopsis::get_spp_names() %>%
  select(
    species_common_name, species_code,
    species_science_name, spp_w_hyphens, type, itis_tsn, worms_id
  ) |>
  arrange(species_common_name)
spp <- join_worms_spp(spp)

# Gather and arrange some metadata ------------------------------------

# UPDATE THE .CSV IN THIS EACH YEAR! see R/get_cosewic_data.R
cos <- get_cosewic_data()
spp <- left_join(spp, cos, by = "species_science_name")

# UPDATE THE .CSV IN THIS EACH YEAR! see R/join_refs_spp.R
spp <- join_refs_spp(spp, french = french)

