# get skates

library(gfdata)
library(dplyr)
library(gfplot)


spp <- c("big skate", "longnose skate")
dat <- get_catch(spp)
glimpse(dat)

# readr::write_csv(dat, "inst/requests/skates-all-catch.csv")
# dat <- readr::read_csv("inst/requests/skates-all-catch.csv")
# unique(dat$trip_category)
# unique(dat$fishery_sector)


tdat <- tidy_catch(dat, area = c("5[AB]+","5[CDE]", "4[B]", "3[CD]"))
plot_catch(tdat)
tdat2 <- tdat %>% rename(kg = value) %>% filter(year < 2021)

readr::write_csv(tdat2, "inst/requests/skates-catch-by-area.csv")
