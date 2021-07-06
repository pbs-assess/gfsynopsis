d <- readRDS("report/data-cache/lingcod.rds")

library(gfplot)

survs <- c(
  # "4B: STRAIT OF GEORGIA",
  "5C: SOUTHERN HECATE STRAIT",
  "5E: WEST COAST Q.C. ISLANDS",
  "5D: NORTHERN HECATE STRAIT",
  "5B: NORTHERN Q.C. SOUND",
  "3D: N.W. VANCOUVER ISLAND",
  "3C: S.W. VANCOUVER ISLAND",
  "5A: SOUTHERN Q.C. SOUND"
  # "2B:CAPE BLANCO TO CAPE PERPETUA (42 50' TO 44 18')"
)

surv_samp <- d$survey_samples
surv_samp <- dplyr::filter(surv_samp, major_stat_area_name %in% survs)
mf <- fit_vb(surv_samp, sex = "female")
mm <- fit_vb(surv_samp, sex = "male")
plot_vb(object_female = mf, object_male = mm)

TMB::sdreport(mf$model)
TMB::sdreport(mm$model)

mf_stan <- fit_vb(surv_samp, sex = "female", method = "mcmc")
mm_stan <- fit_vb(surv_samp, sex = "male", method = "mcmc")
plot_vb(object_female = mf_stan, object_male = mm_stan)

