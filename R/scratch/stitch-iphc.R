library(gfiphc)
library(tidyverse)
library(mgcv)
devtools::load_all()

source(here::here('R', 'scratch', 'cpois-gam-functions.R'))

theme_set(theme_minimal())
dc <- file.path('report/data-cache-july-2023')
iphc_data <- file.path(dc, 'iphc')
# hook data created in get-iphc-hook-data.R
iphc_hook <- readRDS(file.path(iphc_data, 'iphc-hook-counts_1998-2021.rds')) |>
  select(-usable, -standard)

# @TODO: Get a grid
iphc_grid <- # Choose a complete year like 20217

# Wrangle 2022 data to match all other data ----------
# @QUESTION: Where should this code live????
get_iphc_spp_name() |> names()
data('countData2022')
data('setData2022')
check_iphc_spp_name(countData2022)

iphc_2022_counts <- countData2022 |>
  left_join(get_iphc_spp_name(), by = c('spNameIPHC' = 'iphc_common_name')) |>
  # @QUESTION: Allow us to get zero values for all speices in get_iphc_spp_name() ???
  #full_join(get_iphc_spp_name(), by = c('spNameIPHC' = 'iphc_common_name')) |>
  # @Question: Add this to inst/extdata/iphc-spp-names.csv?
  mutate(species_common_name =
    replace(species_common_name, spNameIPHC == "Blackspotted Rockfish",
      "rougheye/blackspotted rockfish complex")
  ) |>
  mutate(species_common_name = ifelse(is.na(species_common_name), spNameIPHC, species_common_name)) |>
  # Get zero observations for each fishing event
  pivot_wider(id_cols = c(year, station),
      names_from = 'species_common_name', values_from = 'specCount',
      values_fill = 0) |>
  pivot_longer(cols = -c(year, station), names_to = "species_common_name",
    values_to = "N_it20")

bait_2022 <- filter(iphc_2022_counts, species_common_name == 'hook with bait') |>
  select(-species_common_name) |>
  rename(baited_hooks = "N_it20")

iphc_2022 <-
  left_join(iphc_2022_counts, bait_2022) |>
    left_join(setData2022) |>
    select(year, station, lat, lon, species_common_name, E_it20, N_it20,
      baited_hooks, usable, standard, effSkateIPHC, hooksObs) |>
    rename(species = "species_common_name", obsHooksPerSet = "hooksObs")
# ----------

# Use the same species as Joe to compare results
# spp_list <- c("yelloweye rockfish",
#             "arrowtooth flounder",
#             "lingcod",
#             "north pacific spiny dogfish",
#             "pacific cod",
#             "pacific halibut",
#             "redbanded rockfish",
#             "sablefish",
#             "big skate",
#             "longnose skate",
#             "shortspine thornyhead")
spp_list <- gfsynopsis::get_spp_names()$species_common_name |> sort()
#spp_list <- "alaska skate"
spp_files <- paste0(gfsynopsis:::clean_name(spp_list), '.rds')

iphc_2022_sp_subset <- iphc_2022 |> filter(species %in% spp_list)

dat <- lapply(spp_list, FUN = function(sp) {
  sp_file <- paste0(gfsynopsis:::clean_name(sp), '.rds')
  readRDS(file.path(dc, 'iphc', sp_file))$set_counts |>
  mutate(species = sp)
}) |>
  enframe() |>
  unnest(cols = value)

clean_dat <-
  left_join(dat, iphc_hook) |> # get observed hook counts
  bind_rows(iphc_2022_sp_subset) |> # add 2022 data
  # @QUESTION: Should we calculate hook values from effective skate for 1995?
  # For rows without hook counts (and just have effective skates), as a proxy you
  # could multiply effective skate number by 100, as an effective skate of 1 is
  # meant to represent 100 hooks (with a few other caveats).
  mutate(obsHooksPerSet = ifelse(year == 1995 & is.na(obsHooksPerSet), E_it * 100, obsHooksPerSet)) |>
  mutate(catch = ifelse(!is.na(N_it), N_it, N_it20),
         sample_n = ifelse(!is.na(N_it), 'whole_haul', '20_hook'),
         effSkate = ifelse(!is.na(N_it), E_it, E_it20),
         hook_removed = obsHooksPerSet - baited_hooks) |>
  mutate(prop_removed = hook_removed / obsHooksPerSet) |>
  mutate(fyear = factor(year), log_eff_skate = log(effSkate),
         fstation = factor(station)) # mgcv needs factor inputs

# @QUESTION: do we need to include sample type as factor in model?
# I.e., WHOLEHAUL vs 20HOOK?

# @FIXME/@NOTE: If species were not observed at a station in 2020 - 2022, they
# Will have an NA value rather than 0. This may not matter given these are the
# rare species and probably will not be stitched anyway.
clean_dat |>
  filter(is.na(catch) | is.na(obsHooksPerSet) | is.na(prop_removed) | is.na(fyear), ) |>
  distinct(year, station, lat, lon, .keep_all = TRUE) #%>% view()

# @QUESTION: Can we use these data? "SOME/ALL SKATES LOST - DATA UNUSABLE FOR CPUE EST"
# Looks like we have effective skate information?
clean_dat |> filter(usable == "N")

test_dat <- clean_dat |> filter(usable == "Y")

# Get GAM outputs
dat_list <- split(test_dat, test_dat$species)

mods <- dat_list |>
  map(\(x) fit_gam(x))
beepr::beep()

saveRDS(mods, 'scratch-out/pstar-gam-fits2.rds')

preds <- mods |>
  map(\(x) pred_gam(x, by = 0.0005)) |>
  bind_rows() |>
  mutate(exp_fit = exp(fit), exp_upr = exp(upr), exp_lwr = exp(lwr))

newdata <- preds |>
  select(species, prop_removed, log_eff_skate, fyear, fstation) %>%
  split(x = ., f = .$species)

# ---------
# Use significant 10% difference to get pstar
fit_at_100 <- preds |>
  filter(prop_removed == 1) |>
  distinct(species, fit, se) |>
  rename(fit_at_100 = "fit", se_100 = "se")

diff_10_check <- left_join(preds, fit_at_100) |>
  mutate(log_ratio =  fit_at_100 - fit,
         percent_diff = 1 - exp(log_ratio),
         sum_of_se = 1.96 * sqrt(se_100^2 + se^2)
       ) |>
  mutate(sig_diff_0 = ifelse(sum_of_se <= abs(log_ratio), 1, 0),  # check ~2SE less than diff of means
         sig_diff_10 = ifelse(sum_of_se <= abs(log_ratio - log(0.9)), 1, 0), # check ~2SE less than diff of means plus 10% diff
         sign_diff = ifelse(log_ratio > 0, 1, -1))
diff_10_check <-
  diff_10_check |>
    group_by(species) |>
    filter(sig_diff_10 == 1) |>
    arrange(species, desc(prop_removed)) |>
    filter(abs(percent_diff) > 0.1) |>
    filter(sign_diff < 0) |>
    slice(1)

diff_10_pstar <- tibble(species = spp_list) |>
  left_join(diff_10_check |> select(species, prop_removed)) |>
  mutate(prop_removed = ifelse(is.na(prop_removed), 1, prop_removed))

# ---------
# Look at derivatives
d1 <- mods |>
  map(\(x) gratia::derivatives(x, term = 's(prop_removed)', type = "backward",
    interval = "simultaneous", n = length(seq(from = 0.15, to = 1, by = 0.0005)))) |>
  bind_rows(.id = "species")
beepr::beep()

# Use custom function to force using the same x-values as predict is done one
d1_custom <- as.list(seq_along(spp_list)) |>
  purrr::set_names(nm = spp_list) |>
  map(\(i) get_fderiv(gam_fit = mods[[i]], newdata = newdata[[i]], terms = "prop_removed", term_columns = "prop_removed",
  x = prop_removed, h =0.0005, nsim = 100, exclude_re = "fstation") |>
  left_join(newdata[[i]])) |>
  bind_rows()

# Use derivative to get p_star
d1_pstar <-
  d1_custom |>
    group_by(species) |>
    #filter(species == "lingcod") |>
    arrange(desc(prop_removed)) |>
    mutate(sign_change = ifelse(lag(derivative) * derivative > 0, "no", "yes"),
           slope_xh = ifelse(lag(derivative) > 0, "positive", "negative")
    ) |>
    filter(sign_change == "yes" & slope_xh == "negative") |>
    select(species, prop_removed, derivative, lower, upper, sign_change, slope_xh) |>
    slice(1)

# Get pstar tibble for plotting
d1_pstar <- tibble(species = spp_list) |>
  left_join(d1_pstar |> select(species, prop_removed)) |>
  mutate(prop_removed = ifelse(is.na(prop_removed), 1, prop_removed))


# Plot derivatives
dev.set(2)
ggplot(data = d1, aes(x = data, y = derivative, ymin = lower, ymax = upper)) +
  geom_line(aes(colour = species)) +
  geom_ribbon(aes(fill = species), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  xlim(c(0.4, 1)) +
  ylim(c(-10, 10)) +
  facet_wrap(~ species, ncol = 3) +
  guides(fill = "none", colour = "none")

dev.set(3)
ggplot(data = d1_custom, aes(x = prop_removed, y = derivative, ymin = lower, ymax = upper)) +
  geom_line(aes(colour = species)) +
  geom_ribbon(aes(fill = species), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  xlim(c(0.4, 1)) +
  facet_wrap(~ species, ncol = 3, scales = "free_y") +
  guides(fill = "none", colour = "none")


# Plots ---------
# Joe values
joe_pstar <- tibble(species = spp_list) |>
  mutate(pstar = case_when(
    species %in% c('north pacific spiny dogfish', 'shortspine thornyhead') ~ 1,
    species %in% c('redbanded rockfish', 'lingcod', 'yelloweye rockfish') ~ 0.85,
    species %in% c('big skate') ~ 0.7,
    TRUE ~ 0.95
    )
  )

# Joe figure
preds |>
  filter(prop_removed > 0.6) |>
  #ggplot(aes(x = prop_removed, y = exp_fit, ymin = exp_lwr, ymax = exp_upr, colour = species, fill = species)) +
ggplot(aes(x = prop_removed, y = exp_fit, ymin = exp_lwr, ymax = exp_upr, fill = species)) +
  geom_line(aes(colour = species)) +
  geom_ribbon(alpha = 0.2) +
  geom_vline(data = joe_pstar, aes(xintercept = pstar), colour = "grey30") +
  geom_vline(data = d1_pstar, aes(xintercept = prop_removed), linetype = "dashed", colour = "darkred") +
  geom_vline(data = diff_10_pstar, aes(xintercept = prop_removed), linetype = "dashed", colour = "dodgerblue") +
  facet_wrap(~ species, ncol = 3) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey95")) +
        #panel.border = element_rect(colour = "grey98", fill = NA)) +
  guides(fill = "none", colour = "none")

