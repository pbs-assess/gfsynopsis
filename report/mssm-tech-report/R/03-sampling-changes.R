if (!('mssm_loaded' %in% ls())) {
  source(here::here('report', 'mssm-tech-report', 'R', '00-load.R'))
}

# --- Gear change ----
net_comp_df <- mssm_dat |>
  filter(fishing_event_id %in% comp_trawls$fishing_event_id) |>
  left_join(comp_trawls) |>
  group_by(net, species_common_name) |>
  summarise(mean_catch = mean(catch)) |>
  ungroup() |>
  mutate(net = factor(net, levels = c('NMFS', 'American'))) |>
  group_by(species_common_name) |>
  filter(sum(mean_catch) > 0) |>
  ungroup()

tow_plot <-
  ggplot(data = net_comp_df, aes(x = net, y = mean_catch, colour = species_common_name, group = species_common_name)) +
    geom_point() +
    geom_line() +
    scale_y_continuous(trans = 'log10', labels = scales::label_number(accuracy = 0.01), limits = c(0.001, NA)) +
    scale_x_discrete() +
    guides(colour = 'none') +
    coord_cartesian(clip = "off", xlim = c(1, 1.7)) +
    ggrepel::geom_text_repel(
      data = net_comp_df %>% filter(net == 'NMFS'),
      aes(label = species_common_name, x = net, y = mean_catch, colour = species_common_name),
      size = 3.5, hjust = 0, segment.color = 'grey85',
      nudge_x = -0.55, box.padding = 0.1, point.padding = 0.8,
      direction = "y"
    ) +
    labs(y = 'Catch (kg)', x = 'Net')
tow_plot

ggsave(filename = file.path(mssm_figs, 'net-comp.png'), width = 6.5, height = 6)


# ------------------------------------------------------------------------------
# --- Look at effect of 2003 sampling protocol on mean annual catch ------------
yearbin_catch <- mssm_dat |>
  filter(year < 2003) |>
  group_by(species_common_name) |>
  summarise(mean_catch = mean(catch, na.rm = TRUE)) |>
  mutate(yearbin_catch = ifelse(mean_catch == 0, 0, 1)) |>
  distinct(species_common_name, yearbin_catch)

post_2003_spp <- filter(yearbin_catch, yearbin_catch == 0)$species_common_name
saveRDS(post_2003_spp, file.path(mssm_dir, 'data-outputs', 'post-2003-spp.rds'))

sampling_2003 <-
  mssm_dat |>
    group_by(species_common_name, species_code, year) |>
    summarise(mean_catch = mean(catch, na.rm = TRUE), .groups = 'drop') |>
    left_join(yearbin_catch) |>
    mutate(mean_catch = ifelse((species_common_name %in% post_2003_spp & year < 2003), NA, mean_catch))

pre_2003_spp_plot <- sampling_2003 |>
  filter(!(species_common_name %in% post_2003_spp)) |>
  mutate(species_common_name = stringr::str_to_title(species_common_name)) |>
  mutate(species_common_name = forcats::fct_reorder(species_common_name, species_code)) |>
  ggplot(data = _, aes(x = year, y = mean_catch)) +
    geom_rect(aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf),
              fill = "gray85", alpha = 0.2) +
    geom_point() +
    geom_line(alpha = 0.5) +
    geom_vline(xintercept = 2001, colour = 'grey50') +
    theme(axis.text = element_blank()) +
    facet_wrap(~ species_common_name, scales = 'free_y', ncol = 4) +
    labs(x =  "Year", y = "Mean annual catch (kg)")
pre_2003_spp_plot

ggsave(file.path(mssm_figs, 'sampling-2003.png'), plot = pre_2003_spp_plot,
  width = 17, height = 9)

# Comparison of pcod, pollock, tomcod and possible misidentification
cod_comparison <-
  mssm_dat |>
    filter(species_common_name %in% c('pacific cod', 'walleye pollock', 'pacific tomcod')) |>
    group_by(species_common_name, species_code, year) |>
    summarise(mean_catch = mean(catch, na.rm = TRUE), .groups = 'drop') |>
    left_join(yearbin_catch) |>
    mutate(species_common_name = stringr::str_to_title(species_common_name)) |>
    mutate(species_common_name = forcats::fct_reorder(species_common_name, species_code)) |>
  ggplot(data = _, aes(x = year, y = mean_catch)) +
    geom_rect(aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = "grey90", colour = NA) +
    geom_point() +
    geom_line() +
    scale_colour_brewer(palette = "Dark2", type = 'qual') +
    guides(colour = "none") +
    #facet_wrap(~ species_common_name, scales = 'free_y', nrow = 3) +
    facet_wrap(~ species_common_name, nrow = 3) +
    labs(x =  "Year", y = "Mean annual catch (kg)")
cod_comparison

ggsave(file.path(mssm_figs, 'sampling-cod.png'), plot = cod_comparison,
  width = 5.5, height = 5.5)

# ---- Fish aggregates ---------------------
# How I got to more-mssm-spp.rds:
#fish <- gfdata::run_sql('GFBioSQL', query = "SELECT * FROM SPECIES")
# saveRDS(fish, file.path(mssm_appendix, 'gfbio_fish_list.rds'))
# fish <- readRDS(file.path(mssm_appendix, 'gfbio_fish_list.rds') |> as_tibble()

# # Look at higher order species classifications and choose species aggregations
# # to look for in the MSSM data
# distinct(fish, SPECIES_DESC, .keep_all = TRUE) |>
#   filter(TAXONOMIC_RANK != 'Species') |>
#   arrange(SPECIES_DESC) |>
#   select(SPECIES_CODE, SPECIES_DESC, SPECIES_COMMON_NAME, SPECIES_SCIENCE_NAME)

# spp_string <- textConnection("01P\tSANDDABS
# 042\tDOGFISH SHARKS
# 051\tSKATES
# 060\tCARTILAGINOUS FISH (SHARKS, SKATES, RAYS, RATFISH)
# 065\tRATFISHES
# 221\tCODFISHES
# 227\tCODS/HAKES/GRENADIERS
# 231\tEELPOUTS
# 234\tEELPOUT
# 388\tSCORPIONFISHES
# 389\tROCKFISHES
# 402\tROCKFISHES
# 465\tLINGCOD
# 472\tSCULPINS
# 477\tSCULPINS/POACHERS/SCORPIONFISH
# 595\tLEFTEYE FLOUNDERS
# 597\tFLATFISHES
# 599\tRIGHTEYE FLOUNDERS
# 618\tPLEURONECTES"
# )

# more_spp_codes <- as_tibble(read.table(spp_string, col.names = c("species_code", "species_common_name"), header = FALSE, sep = "\t"))

# GFBIO query to get additional species
# more_spp <- get_survey_sets2(species = more_spp_codes$species_code, ssid = 7)
# saveRDS(more_spp, file.path(mssm_data, 'higher-taxonomic-mssm-spp.rds')
more_spp <- readRDS(file.path(mssm_data, 'higher-taxonomic-mssm-spp.rds')) |>
  filter(grouping_desc %in% c('WCVI Shrimp Survey Area 124', 'WCVI Shrimp Survey Area 125'))

# Check non-zero catches
more_spp %>% filter(catch_weight > 0 | catch_count > 0) |>
  #filter(species_common_name == 'cartilaginous fish (sharks, skates, rays, ratfish)') |>
  count(species_common_name, year) |>
  arrange(species_common_name, year) |>
  distinct(species_common_name)

more_spp %>% filter(catch_weight == 0 & catch_count == 0) |>
  #filter(species_common_name == 'cartilaginous fish (sharks, skates, rays, ratfish)') |>
  count(species_common_name, year) |>
  arrange(species_common_name, year) |>
  distinct(species_common_name)

more_spp_summ <- more_spp |>
  group_by(species_common_name, species_science_name, year) |>
  summarise(mean_catch = mean(catch_weight, na.rm = TRUE), .groups = 'drop') |>
  mutate(parent_taxonomic_unit = species_science_name, parent_taxonomic_unit)


spp_group_df <- mssm_dat |>
  group_by(species_common_name, species_science_name, year) |>
  summarise(mean_catch = mean(catch_weight, na.rm = TRUE), .groups = 'drop') |>
  left_join(gfsynopsis::get_spp_names()) |>
  mutate(parent_taxonomic_unit = ifelse(species_common_name == "pacific halibut", "pleuronectidae(righteye flounders)", parent_taxonomic_unit)) |>
  mutate(parent_taxonomic_unit = ifelse(species_common_name == "pacific hake", "merlucciidae", parent_taxonomic_unit)) |>
  mutate(parent_taxonomic_unit = ifelse(is.na(parent_taxonomic_unit), species_science_name, parent_taxonomic_unit))

# Baseplot
agg_plot <- function(df, ncol = 1, scales = 'free_y') {
  ggplot(data = df) +
    geom_rect(aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf),
            fill = "gray85", alpha = 0.2) +
    geom_vline(xintercept = 2001, colour = 'grey50') +
    geom_point(aes(x = year, y = mean_catch)) +
    labs(x =  "Year", y = "Mean annual catch (kg)") +
    facet_wrap(~ species_common_name, scales = scales, ncol = ncol)
  }

# All higher order species aggregations f families caught in MSSM
spp_group_plot <-
  more_spp_summ |>
  filter(species_common_name %in% c('eelpouts', 'flatfishes', 'rockfishes', 'sculpins', 'skates')) |>
  mutate(species_common_name = str_to_title(species_common_name)) |>
agg_plot(ncol = 3)
spp_group_plot

ggsave(spp_group_plot, filename = file.path(mssm_figs, 'aggregated-spp-plot.png'), width = 8, height = 4)

#
plot_combined_spp <- function(family, common_name) {
  df1 <- spp_group_df |>
    filter(str_detect(parent_taxonomic_unit, family)) |>
    group_by(year) |>
    summarise(mean_catch = mean(mean_catch), .groups = 'drop') |>
    mutate(species_common_name = 'species combined')

  df2 <- more_spp_summ |>
    filter(str_detect(parent_taxonomic_unit, family)) |>
    group_by(year) |>
    summarise(mean_catch = mean(mean_catch), .groups = 'drop') |>
    mutate(species_common_name = common_name)

  dat <- bind_rows(df1, df2)

  ggplot(data = dat) +
    geom_rect(aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf),
            fill = "gray90", alpha = 0.1) +
    geom_vline(xintercept = 2001, colour = 'grey50') +
    geom_point(aes(x = year, y = mean_catch, colour = species_common_name)) +
    scale_colour_brewer(palette = "Dark2", type = 'qual') +
    labs(x =  "Year", y = "Mean annual catch (kg)")
}

add_group_common_name <- function(df) {
  df |>
  mutate(group_common_name = case_when(
      str_detect(parent_taxonomic_unit, "cottidae") ~ 'sculpins',
      str_detect(parent_taxonomic_unit, "pleuronect|paralich") ~ 'flatfishes',
      str_detect(parent_taxonomic_unit, "rajidae") ~ 'skates',
      str_detect(parent_taxonomic_unit, "sebastes") ~ 'rockfishes',
      str_detect(parent_taxonomic_unit, "zoarcidae") ~ 'eelpouts'))
}

plot_combined_spp('cottidae', "'sculpins'")
plot_combined_spp('pleuronect|paralich', "'flatfishes'")
plot_combined_spp('rajidae', "'skates'")
plot_combined_spp('sebastes', "'rockfishes'")
plot_combined_spp('zoarcidae', "'eelpouts'")

df1 <- spp_group_df |>
  filter(str_detect(parent_taxonomic_unit, c("cottidae|pleuronect|paralich|rajidae|sebastes|zoarcidae"))) |>
  add_group_common_name() |>
  group_by(group_common_name, year) |>
  summarise(mean_catch = mean(mean_catch), .groups = 'drop') |>
  mutate(species_common_name = 'Species')

df2 <- more_spp_summ |>
  filter(str_detect(parent_taxonomic_unit, c("cottidae|pleuronect|paralich|rajidae|sebastes|zoarcidae"))) |>
  add_group_common_name() |>
  group_by(group_common_name, year) |>
  summarise(mean_catch = mean(mean_catch), .groups = 'drop') |>
  mutate(species_common_name = 'Order/Family')

dat <- bind_rows(df1, df2) |>
  mutate(group_common_name = str_to_title(group_common_name))

spp_group_plot2 <-
  ggplot(data = dat, aes(x = year, y = mean_catch, colour = species_common_name, shape = species_common_name)) +
    geom_rect(aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf),
            fill = "gray90", colour = NA, alpha = 0.1) +
    geom_vline(xintercept = 2001, colour = 'grey50') +
    geom_point() +
    geom_line(linewidth = 0.3) +
    scale_colour_brewer(palette = "Dark2", type = 'qual') +
    labs(x =  "Year", y = "Mean annual catch (kg)", shape = "Identification level", colour = 'Identification level') +
    facet_wrap(~ group_common_name, scales = "free_y", ncol = 2) +
    theme(legend.position = c(0.7, 0.15))
spp_group_plot2

ggsave(filename = file.path(mssm_figs, 'aggregated-id-level-plot.png'), width = 7.7, height = 5.2)

# --- Mystery of the eelpout spike in 2002 ---
# Eelpout spike is likely due to baby sablefish misidentification
sablefish <- filter(mssm_dat, species_common_name == "sablefish") |>
  group_by(species_common_name, year) |>
  summarise(mean_catch = mean(catch_weight)) |>
  mutate(species_common_name = 'Sablefish', group_common_name = 'Eelpouts')
sablepouts <- dat |> filter(group_common_name == 'Eelpouts') |>
  mutate(species_common_name = paste0("Eelpouts - ", species_common_name)) |>
  bind_rows(sablefish)

sablepouts_plot <-
  ggplot(data = sablepouts,
    aes(x = year, y = mean_catch, colour = species_common_name,
        shape = species_common_name, group = species_common_name)) +
    geom_rect(aes(xmin = -Inf, xmax = 2003, ymin = -Inf, ymax = Inf),
            fill = "gray90", colour = NA, alpha = 0.1) +
    geom_vline(xintercept = 2001, colour = 'grey50') +
    geom_point() +
    geom_line(linewidth = 0.3) +
    scale_colour_brewer(palette = "Dark2", type = 'qual') +
    labs(x =  "Year", y = "Mean annual catch (kg)", shape = "Identification level", colour = 'Identification level') +
    theme(legend.position = c(0.2, 0.83))
sablepouts_plot

ggsave(filename = file.path(mssm_figs, 'aggregated-sablepouts-plot.png'),
  width = 7, height = 3.3)

# --------

# Skates 2002
# - Does not look like there are any duplicate fishing events
select_simple <- function(df) {
  select(df, fishing_event_id, year, month, day, time_deployed, latitude, longitude,
         depth_m, catch_weight, catch_count, species_common_name)
}

skates_group <- more_spp |> filter(species_common_name == 'skates') |>
  select_simple() |>
  filter(year < 2023) |>
  mutate(id_level = 'Family')
skates_spp <- mssm_dat |>
  left_join(gfsynopsis::get_spp_names()) |>
  filter(str_detect(parent_taxonomic_unit, 'rajidae')) |>
  group_by(year, month, day, time_deployed, latitude, longitude, depth_m) |>
  summarise(catch_weight = sum(catch_weight)) |>
  mutate(id_level = 'Species')

extreme_catches <- skates_group |>
  arrange(-catch_weight) |>
  slice(1:3)

bind_rows(skates_group, skates_spp) |>
ggplot(aes(x = year, y = catch_weight)) +
  geom_point(aes(colour = id_level)) +
  scale_colour_brewer(palette = "Dark2", type = 'qual') +
    labs(x =  "Year", y = "Catch (kg)", colour = 'Identification level') +
  ggrepel::geom_text_repel(
    data = extreme_catches,
    aes(x = year, y = catch_weight, label = fishing_event_id),
      size = 3.5, segment.color = 'grey85',
      nudge_x = -1.5, box.padding = 0.2,
      direction = "y", hjust = 1
    ) +
  theme(legend.position = c(0.15, 0.83))

ggsave(filename = file.path(mssm_figs, 'aggregated-skates-plot.png'),
  width = 7.3, height = 3.8)

# # Sculpins ---
# sculpins <- filter(spp_group_df, str_detect(parent_taxonomic_unit, 'cottidae'))
# sculpin_levels <- c('sculpins', unique(sculpins$species_common_name)[unique(sculpins$species_common_name) != 'sculpins'], 'scuplins combined')
# sculpins_all <- sculpins |>
#   group_by(year) |>
#   summarise(mean_catch = sum(mean_catch)) |>
#   mutate(species_common_name = factor('scuplins combined', levels = sculpin_levels))
# #sculpins <- bind_rows(sculpins, sculpins_all)

# p_sculpin <- sculpins |>
#   mutate(species_common_name = factor(species_common_name, levels = sculpin_levels)) |>
#   agg_plot(ncol = 1, scales = 'fixed')
# p_sculpin

# # Flatfish ---
# flatfish <- filter(spp_group_df, str_detect(parent_taxonomic_unit, 'pleuronect|paralich'))
# flatfish_levels <- c('flatfishes', unique(flatfish$species_common_name)[unique(flatfish$species_common_name) != 'flatfishes'], 'flatfishes combined')
# flatfish_all <- flatfish |>
#   group_by(year) |>
#   summarise(mean_catch = sum(mean_catch)) |>
#   mutate(species_common_name = factor('flatfishes combined', levels = flatfish_levels))
# #flatfish <- bind_rows(flatfish, flatfish_all)

# p_flatfish <- flatfish |>
#   mutate(species_common_name = factor(species_common_name, levels = flatfish_levels)) |>
#   agg_plot(ncol = 2)
# p_flatfish

# # Skates ---
# skates <- filter(spp_group_df, str_detect(parent_taxonomic_unit, 'rajidae'))
# skates_levels <- c('skates', unique(skates$species_common_name)[unique(skates$species_common_name) != 'skates'], 'skates combined')
# skates_all <- skates |>
#   group_by(year) |>
#   summarise(mean_catch = sum(mean_catch)) |>
#   mutate(species_common_name = factor('skates combined', levels = skates_levels))
# #skates <- bind_rows(skates, skates_all)

# p_skate <- skates |>
#   mutate(species_common_name = factor(species_common_name, levels = skates_levels)) |>
#   agg_plot(ncol = 1, scales = 'fixed') +
#   theme(axis.title.y = element_blank())

# # Rockfish ---
# rockfish <- filter(spp_group_df, str_detect(parent_taxonomic_unit, 'sebastes'))
# rockfish_levels <- c('rockfishes', unique(rockfish$species_common_name)[unique(rockfish$species_common_name) != 'rockfishes'], 'rockfishes combined')
# rockfish_all <- rockfish |>
#   group_by(year) |>
#   summarise(mean_catch = sum(mean_catch)) |>
#   mutate(species_common_name = factor('rockfishes combined', levels = rockfish_levels))
# #rockfish <- bind_rows(rockfish, rockfish_all)

# p_rockfish <- rockfish |>
#   mutate(species_common_name = factor(species_common_name, levels = rockfish_levels)) |>
#   agg_plot(ncol = 2)

# # Eelpouts ---
# eelpouts <- filter(spp_group_df, str_detect(parent_taxonomic_unit, 'zoarcidae'))
# eelpouts_levels <- c('all eelpouts', 'eelpouts', unique(eelpouts$species_common_name)[unique(eelpouts$species_common_name) != 'eelpouts'])
# eelpouts_all <- eelpouts |>
#   group_by(year) |>
#   summarise(mean_catch = sum(mean_catch)) |>
#   mutate(species_common_name = factor('all eelpouts', levels = eelpouts_levels))
# eelpouts <- bind_rows(eelpouts, eelpouts_all)

# p_eelpout <- eelpouts |>
#   mutate(species_common_name = factor(species_common_name, levels = eelpouts_levels)) |>
#   agg_plot(ncol = 1)

# design <- "
#   12
#   12
#   12
#   1#
#   1#
# "
# p_sculpin + p_skate + plot_layout(design = design)
# ggsave(filename = file.path(mssm_figs, 'agg-sculpin-skate.png'), width = 7, height = 7)