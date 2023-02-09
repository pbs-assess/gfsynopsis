library(gfdata)
library(dplyr)

all_spp <- gfsynopsis::get_spp_names()

# test <- all_spp[1:2,]
# i <- 1

# all_spp <- filter(all_spp, species_common_name %in% c("lingcod", "quillback rockfish", "yelloweye rockfish"))

d <- list()
for (i in seq_along(all_spp$species_common_name)){
  d[[i]] <- get_survey_sets(all_spp$species_common_name[i], ssid = c(39,40))
}

d <- do.call("rbind", d)

saveRDS(d, "all-inside-hbll-2022.rds")

