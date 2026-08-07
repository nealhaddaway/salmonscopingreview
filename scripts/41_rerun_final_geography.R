# =============================================================================
# File: scripts/41_rerun_final_geography.R
# Purpose: Re-run the final corrected geography layer and primary-country
#          assignment after the existing v2 global gazetteer has been built.
# =============================================================================

source("scripts/25_add_continent_region_layer_FINAL.R")
source("scripts/26_run_global_geography_detection_v3_FINAL.R")
source("scripts/34_assign_primary_study_country_v2_FINAL.R")

message("")
message("Final corrected geography pipeline completed.")
message(
  "Primary-country output: ",
  here::here(
    "outputs",
    "stage_5_geography",
    "primary_study_country_v2",
    "primary_country_summary_v2.csv"
  )
)
