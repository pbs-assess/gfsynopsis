library(tidyverse)
library(gfplot)
rsr <- readRDS("report/data-cache3/redstripe-rockfish.rds")

x <- rsr$survey_samples
x <- x[!duplicated(x$specimen_id), ]
x <- x %>% filter(!is.na(weight))
x <- filter(x, major_stat_area_name != "4B: STRAIT OF GEORGIA")
ggplot(x, aes(major_stat_area_name, weight)) +
  geom_violin(fill = "grey70", col = "grey70") +
  geom_point(position = position_jitter(width = 0.15, height = 0), aes(colour = year),
    size = 1.5, alpha = 0.1) +
  xlab("") +
  coord_flip() +
  viridis::scale_color_viridis() +
  theme_pbs()
ggsave("rsr-weight.pdf", width = 7, height = 5)

x <- rsr$survey_samples
x <- x[!duplicated(x$specimen_id), ]
x <- x %>% filter(!is.na(age))
x <- filter(x, major_stat_area_name != "4B: STRAIT OF GEORGIA")
ggplot(x, aes(major_stat_area_name, age)) +
  geom_violin(fill = "grey70", col = "grey70") +
  geom_point(position = position_jitter(width = 0.15, height = 0), aes(colour = year),
    size = 1.5, alpha = 0.1) +
  xlab("") +
  coord_flip() +
  viridis::scale_color_viridis() +
  theme_pbs()
ggsave("rsr-age.pdf", width = 7, height = 5)

areas <- c("5[DE]+", "3[CD]+|5[ABC]+")
x$area <- NA
for (i in seq_along(areas)) {
  x[grepl(areas[[i]], x$major_stat_area_name), "area"] <-
    gsub("\\[|\\]|\\+", "", areas[[i]])
}

filter(x, !is.na(area)) %>%
  ggplot(aes(area, weight)) +
  geom_violin() +
  geom_point(position = position_jitter(width = 0.2, height = 0),
    aes(colour = year), size = 3) +
  coord_flip() +
  viridis::scale_color_viridis() +
  theme_pbs()

library(rstanarm)
m <- stan_glm(weight ~ as.factor(area),
  data = x, family = Gamma(link = log), iter = 500, cores = 1)

e <- as.data.frame(m)
e2 <- data.frame(b = exp(e$`as.factor(area)5DE`))
ggplot(e2, aes(b)) + geom_density(fill = "grey70", col = "grey70") + theme_pbs() +
  coord_cartesian(expand = FALSE, ylim = c(0, 24)) +
  xlab("Multiplicative difference in weight: North vs. South") +
  ggtitle("stan_glm(weight ~ as.factor(area), family = Gamma(link = log))") +
  ylab("Probability density") +
  xlim(0.8, 2.2) +
  geom_vline(xintercept = 1.0, lty = 2)
ggsave("rsr-weight-diff.pdf", width = 5.5, height = 3.5)

round(quantile(exp(e$`as.factor(area)5DE`), probs = c(0.025, 0.5, 0.975)), 1)
# sum(exp(e$`as.factor(area)5DE`) < 1)

##################

x <- rsr$survey_samples
x <- x[!duplicated(x$specimen_id), ]
x <- filter(x, major_stat_area_name != "4B: STRAIT OF GEORGIA")
x <- filter(x, major_stat_area_name != "5D: NORTHERN HECATE STRAIT")

o <- plyr::dlply(x, "major_stat_area_name", function(xx) {
  mf <- fit_vb(xx, "female")
  mm <- fit_vb(xx, "male")
  plot_vb(object_female = mf, object_male = mm) +
    ggtitle(unique(xx$major_stat_area_name)) +
    coord_cartesian(xlim = c(0, 50), ylim = c(0, 45))
})

library(gridExtra)
n <- length(o)
nCol <- floor(sqrt(n))
pdf("vb-rsr.pdf", width = 8, height = 8)
do.call("grid.arrange", c(o, ncol=nCol))
dev.off()

##################

x <- rsr$survey_samples
x <- bind_samples(dat_survey = rsr$survey_samples, dat_comm = rsr$commercial_samples)
x <- x[!duplicated(x$specimen_id), ]
x <- filter(x, major_stat_area_name != "4B: STRAIT OF GEORGIA")
x <- filter(x, major_stat_area_name != "5D: NORTHERN HECATE STRAIT")

o <- plyr::dlply(x, "major_stat_area_name", function(xx) {
  mf <- fit_vb(xx, "female")
  mm <- fit_vb(xx, "male")
  plot_vb(object_female = mf, object_male = mm) +
    ggtitle(unique(xx$major_stat_area_name)) +
    coord_cartesian(xlim = c(0, 50), ylim = c(0, 45))
})

library(gridExtra)
n <- length(o)
nCol <- floor(sqrt(n))
pdf("vb-rsr-suv-comm.pdf", width = 8, height = 8)
do.call("grid.arrange", c(o, ncol=nCol))
dev.off()

##################

x <- rsr$survey_samples
# x <- bind_samples(dat_survey = rsr$survey_samples, dat_comm = rsr$commercial_samples)
x <- x[!duplicated(x$specimen_id), ]
x <- filter(x, major_stat_area_name != "4B: STRAIT OF GEORGIA")
x <- filter(x, major_stat_area_name != "5D: NORTHERN HECATE STRAIT")
x <- filter(x, major_stat_area_name != "5D: NORTHERN HECATE STRAIT")
# x <- filter(x, survey_abbrev %in% c("HS MSA", "SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI"))

o <- plyr::dlply(x, "major_stat_area_name", function(xx) {
  fit_vb(xx, "female", method = "mcmc", iter = 2000, chains = 1, chains = 4, cores = 4)
})

oo <- plyr::ldply(o, function(xxx) stanhelpers::extract_df(xxx$model, output = "long_df"))
oo %>% filter(!variable %in% c("sigma", "lp__")) %>%
  ggplot(aes(x = major_stat_area_name, y = value)) +
  facet_wrap(~variable, scales = "free_x") + geom_violin(fill = "grey60", colour = "grey60") +
  theme_pbs() +
  coord_flip() +
  ylab("Parameter posterior distribution") +
  xlab("") + ggtitle("von Bertalanffy growth parmaters") +
  labs(subtitle = "Females only; survey samples")
ggsave("vb-params.pdf", width = 8, height = 4)

##################

x <- rsr$survey_samples

x <- filter(x, survey_abbrev %in% c("SYN QCS", "SYN WCHG", "SYN WCVI"))
of <- plyr::dlply(x, "survey_abbrev", function(xx) {
  fit_vb(xx, "female", method = "mcmc", iter = 2000, chains = 4, cores = 4)
})
om <- plyr::dlply(x, "survey_abbrev", function(xx) {
  fit_vb(xx, "male", method = "mcmc", iter = 2000, chains = 4, cores = 4)
})

oof <- plyr::ldply(of, function(xxx) {
  if (!is.na(xxx$model)) {
    stanhelpers::extract_df(xxx$model, output = "long_df")
  }
})
oom <- plyr::ldply(om, function(xxx) {
  if (!is.na(xxx$model)) {
    stanhelpers::extract_df(xxx$model, output = "long_df")
  }
})
oo <- bind_rows(data.frame(oof, sex = "F"), data.frame(oom, sex = "M"))

oo %>% filter(!variable %in% c("sigma", "lp__")) %>%
  mutate(north = ifelse(survey_abbrev %in% "SYN WCHG", TRUE, FALSE)) %>%
  mutate(lab = ifelse(survey_abbrev %in% "SYN WCHG", paste("N", survey_abbrev),
    paste("S", survey_abbrev))) %>%
  mutate(lab = forcats::fct_rev(lab)) %>%
  ggplot(aes(x = lab, y = value, fill = sex, colour = sex)) +
  facet_wrap(~variable, scales = "free_x") + geom_violin() +
  theme_pbs() +
  coord_flip() +
  ylab("Parameter posterior distribution") +
  xlab("") + ggtitle("von Bertalanffy growth parmaters") +
  labs(subtitle = "Synoptic survey samples only")
ggsave("vb-params-surveys-males-and-females.pdf", width = 7, height = 3)
