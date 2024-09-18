extra_mssm_data <- run_sql("GFBioSQL",
"SELECT
      C.SPECIES_CODE,
      S.SURVEY_SERIES_ID,
      S.SURVEY_ID,
      T.TRIP_ID,
      V.VESSEL_NAME,
      CV.CATCH_VERIFICATION_DESC,
      ECM.EST_CATCH_METHOD_DESC,
      ISNULL(FE.FE_PARENT_EVENT_ID, FE.FISHING_EVENT_ID) AS FISHING_EVENT_ID,
      FE.FE_MAJOR_LEVEL_ID,
      SUM(ISNULL(CATCH_WEIGHT,0)) AS CATCH_WEIGHT,
      SUM(ISNULL(CATCH_COUNT,0)) AS CATCH_COUNT
   FROM SURVEY S
      INNER JOIN SURVEY_SERIES ss ON
      S.SURVEY_SERIES_ID = SS.SURVEY_SERIES_ID
      INNER JOIN TRIP_SURVEY TRS ON
      S.SURVEY_ID = TRS.SURVEY_ID
      INNER JOIN TRIP T ON
      TRS.TRIP_ID = T.TRIP_ID
      INNER JOIN FISHING_EVENT FE ON
      T.TRIP_ID = FE.TRIP_ID
      INNER JOIN FISHING_EVENT_CATCH FEC ON
      FEC.FISHING_EVENT_ID = FE.FISHING_EVENT_ID
      INNER JOIN CATCH C ON
      FEC.CATCH_ID = C.CATCH_ID
      INNER JOIN SPECIES SP ON
      SP.SPECIES_CODE = C.SPECIES_CODE
      LEFT JOIN TRIP_ACTIVITY TA ON
      TA.TRIP_ID = FE.TRIP_ID
      LEFT JOIN ACTIVITY A ON
      A.ACTIVITY_CODE = TA.ACTIVITY_CODE
      LEFT JOIN CATCH_VERIFICATION CV ON
      CV.CATCH_VERIFICATION_CODE = C.CATCH_VERIFICATION_CODE
      LEFT JOIN VESSEL V ON
      V.VESSEL_ID = T.VESSEL_ID
      LEFT JOIN EST_CATCH_METHOD ECM ON
      FE.EST_CATCH_METHOD_CODE = ECM.EST_CATCH_METHOD_CODE
   WHERE S.SURVEY_SERIES_ID = 7
      AND FE.FE_MAJOR_LEVEL_ID < 700
      AND FE.FE_MINOR_LEVEL_ID IS NULL
      -- insert species here
      -- insert ssid here
      -- insert fe_vector here
      -- insert major here
   GROUP BY C.SPECIES_CODE, FE.GEAR_CODE, A.ACTIVITY_DESC, TA.ACTIVITY_CODE,
      S.SURVEY_SERIES_ID, S.SURVEY_ID, T.TRIP_ID, V.VESSEL_NAME, FE.FISHING_EVENT_ID,
      FE.FE_PARENT_EVENT_ID, FE.FE_MAJOR_LEVEL_ID,
      T.TRIP_START_DATE, S.ORIGINAL_IND, CV.CATCH_VERIFICATION_DESC,
      ECM.EST_CATCH_METHOD_DESC
")
dplyr::glimpse(extra_mssm_data)
saveRDS(as_tibble(extra_mssm_data), file = "data/vessel-catch-verification-estimation-mssm.rds")


ed <- readRDS(file.path(mssm_data, "vessel-catch-verification-estimation-mssm.rds")) |>
  janitor::clean_names() |>
  as_tibble() |>
  mutate(cw2 = catch_weight, cc2 = catch_count)

ed

distinct(ed, catch_verification_desc, est_catch_method_desc)


anti_join(select(ed,species_code, fishing_event_id, catch_weight),
  mssm_dat |> select(species_code, fishing_event_id, catch_weight))

dat <- left_join(mssm_dat, ed)

filter(dat, !is.na(catch_verification_desc) | !is.na(est_catch_method_desc)) |>
filter(year %in% 2001:2022) |>
distinct(year, catch_verification_desc, est_catch_method_desc) |>
arrange(year) |>
print(n = 39)

glimpse(dat)

dat |> distinct(year, vessel_name)
