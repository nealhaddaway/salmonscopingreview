# =============================================================================
# File: 40_clean_geography_mentions.R
# Purpose: Final deterministic cleaning of geography mentions.
# =============================================================================

source("scripts/00_setup.R")

input_file <- here::here(
  "outputs",
  "stage_5_geography",
  "global_detection_v3",
  "global_geography_mentions_v3.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_5_geography",
  "global_detection_v3_clean"
)

fs::dir_create(output_dir)

geo <- readr::read_csv(input_file, show_col_types = FALSE)

geo <- geo |>
  dplyr::mutate(
    context_lower = stringr::str_to_lower(dplyr::coalesce(context, ""))
  )

patterns <- list(
  publisher = paste(
    c("copyright","all rights reserved","creative commons",
      "published by","publisher","springer","elsevier",
      "wiley","taylor & francis","crc press",
      "oxford university press","cambridge university press",
      "isbn","issn","edition","editor","editors"),
    collapse="|"),
  translation = paste(
    c("translate with","translation","language selector",
      "select language"),
    collapse="|"),
  software = paste(
    c("software","package","version","plugin","toolbox","r package"),
    collapse="|"),
  manufacturer = paste(
    c("manufacturer","manufactured by","supplied by",
      "provided by","purchased from","equipment",
      "instrument","microscope","camera","reader",
      "incubator","analyser","analyzer"),
    collapse="|"),
  convention = paste(
    c("convention","protocol","treaty",
      "international maritime organization","imo",
      "helcom","ospar",
      "barcelona convention","paris convention",
      "helsinki convention"),
    collapse="|"),
  institution = paste(
    c("university","institute","department",
      "faculty","academy of sciences",
      "research centre","research center",
      "centre","center","laboratory",
      "laboratoire","school of","hospital"),
    collapse="|")
)

geo2 <- geo |>
  dplyr::mutate(
    remove_publisher = stringr::str_detect(context_lower, patterns$publisher),
    remove_translation = stringr::str_detect(context_lower, patterns$translation),
    remove_software = stringr::str_detect(context_lower, patterns$software),
    remove_manufacturer = stringr::str_detect(context_lower, patterns$manufacturer),
    remove_convention = stringr::str_detect(context_lower, patterns$convention),
    remove_institution = stringr::str_detect(context_lower, patterns$institution),
    remove = remove_publisher |
             remove_translation |
             remove_software |
             remove_manufacturer |
             remove_convention |
             remove_institution
  )

clean <- geo2 |>
  dplyr::filter(!remove)

record_annotations <- clean |>
  dplyr::group_by(record_sequence, record_id) |>
  dplyr::summarise(
    countries = paste(sort(unique(country_name[!is.na(country_name)])),
                      collapse="; "),
    iso3c = paste(sort(unique(iso3c[!is.na(iso3c)])),
                  collapse="; "),
    country_count = dplyr::n_distinct(iso3c[!is.na(iso3c)]),
    .groups="drop"
  )

record_summary <- record_annotations |>
  dplyr::count(country_count, name="records")

readr::write_csv(clean,
  fs::path(output_dir,"global_geography_mentions_v3_clean.csv"), na="")
readr::write_csv(record_annotations,
  fs::path(output_dir,"record_country_annotations_v3_clean.csv"), na="")
readr::write_csv(record_summary,
  fs::path(output_dir,"global_geography_record_summary_v3_clean.csv"), na="")

cat("\nMentions before:", nrow(geo),
    "\nMentions after:", nrow(clean),
    "\nPublisher rows removed:", sum(geo2$remove_publisher),
    "\nTranslation rows removed:", sum(geo2$remove_translation),
    "\nSoftware rows removed:", sum(geo2$remove_software),
    "\nManufacturer rows removed:", sum(geo2$remove_manufacturer),
    "\nConvention rows removed:", sum(geo2$remove_convention),
    "\nInstitution rows removed:", sum(geo2$remove_institution),
    "\nRecords with geography:", nrow(record_annotations), "\n")
