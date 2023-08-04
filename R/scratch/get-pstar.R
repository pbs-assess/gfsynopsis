library(tidyverse)
library(mgcv)
devtools::load_all()

source(here::here('R', 'scratch', 'cpois-gam-functions.R'))
source(here::here('R', 'scratch', 'stitch-iphc-functions.R'))

iphc_data_cache <- file.path('report', 'data-cache-aug-2023', 'iphc')
iphc_hook <- readRDS(file.path(iphc_data_cache, 'iphc-hook-counts_1998-2022.rds')) |>
  select(-usable, -standard)

spp_list <- gfsynopsis::get_spp_names()$species_common_name |> sort()
spp_list <- "lingcod"

sp_dat <- spp_list |>
  map(\(species) load_iphc_sp_dat(species = species, iphc_data_cache = iphc_data))

clean_dat <- sp_dat |>
  map(\(dat) prep_iphc_stitch_dat(sp_dat = dat, hook_dat = iphc_hook))


iphc_pos_sets <- clean_dat |>
  map(\(x) get_iphc_pos_sets(x)) |>
  enframe() |>
  unnest(col = 'value')

# Use only species with proportion of positive sets >= 5%
iphc_pos_sets_0.05 <- filter(iphc_pos_sets, prop_pos >= 0.05)

test_dat <-
  clean_dat |>
  enframe() |>
  unnest(col = 'value') |>
  filter(usable == "Y") |>
  filter(species %in% iphc_pos_sets_0.05$species)

# Get p_star values ------------------------
# Get GAM outputs
dat_list <- split(test_dat, test_dat$species)

out_dir <- file.path('report', 'pstar-cache', 'iphc')

mods <- dat_list |>
  map(\(x) fit_gam(x)) |>
  map(\(x) saveRDS(x, file.path(out_dir, paste0(gfsynopsis:::clean_name(unique(x$data$species)), '_gam.rds'))))
beepr::beep()

#saveRDS(mods, 'scratch-out/pstar-gam-fits.rds')
mods <- readRDS('scratch-out/pstar-gam-fits.rds')

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
# d1 <- mods |>
#   map(\(x) gratia::derivatives(x, term = 's(prop_removed)', type = "backward",
#     interval = "simultaneous", n = length(seq(from = 0.15, to = 1, by = 0.0005)))) |>
#   bind_rows(.id = "species")
# beepr::beep()

# Use custom function to force using the same x-values as predict is done one
d1_custom <- as.list(seq_along(mods)) |>
  #purrr::set_names(nm = mods) |>
  map(\(i) get_fderiv(gam_object = mods[[i]], newdata = newdata[[i]], terms = "prop_removed", term_columns = "prop_removed",
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
  mutate(pstar = ifelse(is.na(prop_removed), 1, prop_removed)) |>
  select(-prop_removed)
saveRDS(d1_pstar, file.path('report', 'pstar-cache', 'iphc', 'derivative-pstar-df.rds'))

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

