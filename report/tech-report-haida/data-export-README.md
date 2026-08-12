## Overview

This directory contains species-specific data exports from *A data synopsis for Haida Gwaii Groundfish: 2024*. Each Excel file contains standardized fisheries data for one species, with each worksheet containing data for a different figure or set of figures in the synopsis report.

---

## File Structure

### Excel Files
**Name:** `{species-name}.xlsx`
**Format:** Multi-sheet workbook with:
- **`data_summary` sheet** (first sheet): Inventory of all data types with availability status
- **Data sheets** (one per available data type): Named as indicated below (and matching figures in the synopsis report). If one a data element had no data, the worksheet is not included, and this is noted in the first sheet containing the data inventory.

**Survey Abbreviations:**
| Code | Full Name |
|------|-----------|
| SYN WCHG | Synoptic Bottom Trawl: West Coast Haida Gwaii |
| SYN HS | Synoptic Bottom Trawl: Hecate Strait |
| SYN QCS | Synoptic Bottom Trawl: Queen Charlotte Sound |
| HBLL OUT N | Hard Bottom Longline: Outside North |
| HBLL OUT S | Hard Bottom Longline: Outside South |
| IPHC FISS | International Pacific Halibut Commission Fishery-Independent Setline Survey |
---

## Data Worksheets

Each species export contains up to **12 data sheets**. Not all elements are present for every species, this is noted on the first metadata worksheet that inventories the datasets available.

### 1. survey_index
**Description:** Relative biomass or abundance indices from research surveys
**Source:** Spatiotemporal model-based indices (coastwide models subset to Haida Territory)

**Columns:**
- `survey_abbrev`: Survey code (SYN WCHG, SYN HS, SYN QCS, HBLL OUT N/S, IPHC FISS, or a combination of these if stitched: e.g., SYN WCHG/HS/QCS/WCVI)
- `year`: Survey year
- `biomass`: Relative biomass (trawl) or abundance (longline) estimate (raw scale)
- `lowerci`: Lower 95% confidence interval (raw scale)
- `upperci`: Upper 95% confidence interval (raw scale)
- `mean_cv`: Mean coefficient of variation across years for survey
- `num_sets`: Mean number of sets across years for survey
- `num_pos_sets`: Mean number of positive sets (with catch) across years for survey
- `spatial`: Spatial random field structure ("on" = included)
- `spatiotemporal`: Spatiotemporal random field structure ("iid" = independent and identically distributed by year)
- `family`: Error distribution family ("delta-lognormal")
- `method`: Index method ("geostat" = geostatistical/model-based, "design")
- `biomass_scaled`: Biomass scaled for visualization (see scaling details below)
- `lowerci_scaled`: Lower CI scaled (same scaling factor as biomass)
- `upperci_scaled`: Upper CI scaled (same scaling factor as biomass)

**Units:** Synoptic = biomass, HBLL/IPHC = fish counts/abundance
**Scaling:**
  - If both design-based and geostat indices exist for a survey: (1) both scaled to same geometric mean for overlapping years, (2) then both divided by max geostat upperci → max upperci_scaled = 1.0
  - If only geostat exists: scaled by max(upperci) → max upperci_scaled = 1.0
**Reference:** See Appendix A in Anderson et al. (2024) synopsis coastwide report for full spatiotemporal model specification.

Anderson, S.C., Dunic, J.C., Keppel, E.A., and Edwards, A.M. 2024. A data synopsis for British Columbia groundfish: 2023 data update. Can.
   Tech. Rep. Fish. Aquat. Sci. 3641: viii + 262 p.

---

### 2. catch_totals
**Description:** Aggregated commercial catch by year, area, and gear type

**Columns:**
- `year`: Year
- `area`: Management area
- `gear`: Gear type (Bottom trawl, Midwater trawl, Hook and line, Trap, etc.)
- `species_common_name`: Species common name
- `value`: Total catch in tonnes (landed + discarded)

---

### 3. trawl_cpue_index
**Description:** Standardized commercial bottom trawl catch per unit effort from spatiotemporal models

**Columns:**
- `year` (int): Year
- `area` (chr): Spatial area ("Whole area" only)
- `est` (num): CPUE estimate (Relative index (kg/hour trawled, positive tows only, scaled))
- `lwr`, `upr` (num): 95% confidence interval bounds
- `se_link` (num): Standard error in link space
- `est_link` (num): Estimate in link space
- `geo_mean` (num): Geometric mean
- `max_se` (num): Maximum standard error

**Reference:** See Appendix A in Anderson et al. (2025) synopsis coastwide report for spatiotemporal CPUE model specification.

Anderson, S.C. and Dunic, J.C. 2025. A data synopsis for British Columbia groundfish: 2024 data update. Can. Tech. Rep. Fish. Aquat. Sci. 3718: viii + 263 p.

---

### 4. maps_synoptic_biomass
**Description:** Synoptic survey biomass density predictions on 2km × 2km spatial grid

**Columns:**
- `lon`, `lat`: WGS84 coordinates (EPSG:4326)
- `X`, `Y`: UTM Zone 9 coordinates (meters)
- `depth`: Depth (meters)
- `depth_scaled`, `depth_scaled2`: Scaled depth and depth-squared covariates
- `combined`: Predicted biomass density (kg/km²)
- `survey`: Survey abbreviation
- `year`: Most recent survey year

**Reference:** See Appendix E in Anderson et al. (2019) for model specifications.

Anderson, S.C., Keppel, E.A., and Edwards, A.M. 2019. A reproducible data synopsis for over 100 species of British Columbia groundfish. DFO Can. Sci. Advis. Sec. Res. Doc. 2019/041: vii + 321 p.

---

### 5. maps_hbll_catch
**Description:** Outside Hard Bottom Longline (HBLL OUT) survey catch predictions on 2km × 2km spatial grid

**Columns:**
- `lon`, `lat`: WGS84 coordinates
- `X`, `Y`: UTM Zone 9 coordinates
- `depth`: Depth (meters)
- `depth_scaled`, `depth_scaled2`: Scaled depth and depth-squared covariates
- `combined`: Predicted catch density (fish/km²)
- `survey`: Survey abbreviation
- `year`: Most recent survey year

**Reference:** See Appendix E in Anderson et al. (2019) for model specifications.

Anderson, S.C., Keppel, E.A., and Edwards, A.M. 2019. A reproducible data synopsis for over 100 species of British Columbia groundfish. DFO Can. Sci. Advis. Sec. Res. Doc. 2019/041: vii + 321 p.
---

### 6. maps_iphc_catch
**Description:** IPHC Fishery-Independent Setline Survey catch at fixed station locations (raw unmodeled data)

**Columns:**
- `survey`: "IPHC FISS"
- `year`: Survey year (most recent year only)
- `station`: IPHC station identifier
- `station_key`: Unique station key
- `lon`, `lat`: WGS84 coordinates (station locations)
- `catch`: Number of fish observed
- `effective_skates`: Effective skates fished
  
**Reference:** See Appendix A in Anderson et al. (2024) for more details on obtaining the full IPHC FISS dataset.

Anderson, S.C., Dunic, J.C., Keppel, E.A., and Edwards, A.M. 2024. A data synopsis for British Columbia groundfish: 2023 data update. Can.
   Tech. Rep. Fish. Aquat. Sci. 3641: viii + 262 p.

---

### 7. maps_commercial_trawl_cpue
**Description:** Commercial bottom trawl CPUE aggregated to 7km hexagon grid (privacy-filtered to >= 3 vessels per cell)

**Columns:**
- `lon`, `lat`: Hexagon center coordinates (WGS84)
- `cpue_kg_hr`: Geometric mean CPUE of positive tows (kg/hour)
- `hex_width_km`, `hex_height_km`: Hexagon dimensions (7 km)

---

### 8. maps_commercial_longline_cpue
**Description:** Commercial hook-and-line CPUE aggregated to 7km hexagon grid (privacy-filtered to >= 3 vessels per cell)

**Columns:**
- `lon`, `lat`: Hexagon center coordinates (WGS84)
- `cpue_fish_set`: Geometric mean CPUE (fish/set)
- `hex_width_km`, `hex_height_km`: Hexagon dimensions (7 km)

---

### 9. length_compositions
**Description:** Length-frequency distributions by survey/year/sex

**Columns:**
- `species_common_name`: Species name
- `survey_abbrev`: Survey abbreviation (or "Commercial" for fishery samples)
- `year`: Year
- `sex`: Sex (Female, Male, or combined for commercial)
- `length_bin`: Length bin midpoint (cm)
- `proportion`: Proportion in bin (0-1)
- `total`: Total sample size in year (n fish measured)
- `survey_abbrev2`: Column title in gfsynopsis plot

---

### 10. age_compositions
**Description:** Age-frequency distributions by survey/year/sex

**Columns:**
- `species_common_name`: Species name
- `survey_abbrev`: Survey abbreviation
- `year`: Year
- `sex`: Sex (Female, Male)
- `age`: Age (years)
- `proportion`: Proportion at age (0-1)
- `total`: Total sample size in year (n fish aged)

---

### 11. survey_specimen_counts
**Description:** Counts of biological specimens collected from research surveys

**Columns:**
- `species_common_name`: Species name
- `year`: Year
- `type`: Sample type ("length", "weight", "age", "maturity", "ageing_structure")
- `n`: Number of specimens

**Sample Types:**
- `length`: Fish measured for length
- `weight`: Fish weighed
- `age`: Fish aged (otoliths/structures read)
- `maturity`: Fish with maturity assessment
- `ageing_structure`: Otoliths/fins/structures


---

### 12. commercial_specimen_counts
**Description:** Counts of biological specimens from commercial fisheries

**Columns:**
- `species_common_name`: Species name
- `year`: Year
- `type`: Sample type (same as survey specimens)
- `n`: Number of specimens

**Sample Types:**
- `length`: Fish measured for length
- `weight`: Fish weighed
- `age`: Fish aged (otoliths/structures read)
- `maturity`: Fish with maturity assessment
- `ageing_structure`: Otoliths/fins/structures

---


Anderson, S.C., and Dunic, J.C. 2025. A data synopsis for British Columbia groundfish: 2024 data update. Can. Tech. Rep. Fish. Aquat. Sci. 3718: viii + 263 p.

Anderson, S.C., Dunic, J.C., Keppel, E.A., and Edwards, A.M. 2024. A data synopsis for British Columbia groundfish: 2023 data update. Can. Tech. Rep. Fish. Aquat. Sci. 3641: viii + 262 p.

Anderson, S.C., Dunic, J.C., Keppel, E.A., and Edwards, A.M. 2024. A data synopsis for British Columbia groundfish: 2022 data update. Can. Tech. Rep. Fish. Aquat. Sci. 3624: viii + 267 p.

Anderson, S.C., Keppel, E.A., and Edwards, A.M. 2019. A reproducible data synopsis for over 100 species of British Columbia groundfish. DFO Can. Sci. Advis. Sec. Res. Doc. 2019/041: vii + 321 p.