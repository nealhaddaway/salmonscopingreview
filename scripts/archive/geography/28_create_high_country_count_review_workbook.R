# =============================================================================
# File: 28_create_high_country_count_review_workbook.R
# Project: salmonscopingreview
# Purpose: Create a review workbook for records assigned five or more countries
# =============================================================================

source("scripts/00_setup.R")
source("R/read_corpus.R")

# Change this value if you want a different threshold.
# 4 means: include records with 5 or more detected countries.
max_countries_allowed <- 4L

input_records <- here::here(
  "data_raw",
  "INCLUDES fixed abstracts.txt"
)

input_mentions <- here::here(
  "outputs",
  "stage_5_geography",
  "global_detection_v3",
  "global_geography_mentions_v3.csv"
)

input_summary <- here::here(
  "outputs",
  "stage_5_geography",
  "global_detection_v3",
  "global_geography_record_summary_v3.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_5_geography",
  "validation"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(input_records),
  file.exists(input_mentions),
  file.exists(input_summary)
)

records <- read_corpus(input_records) |>
  dplyr::mutate(
    record_id = as.character(record_id)
  )

mentions <- readr::read_csv(
  input_mentions,
  show_col_types = FALSE
) |>
  dplyr::mutate(
    record_id = as.character(record_id)
  )

record_summary <- readr::read_csv(
  input_summary,
  show_col_types = FALSE
) |>
  dplyr::mutate(
    record_id = as.character(record_id)
  )

high_count_records <- record_summary |>
  dplyr::filter(
    country_count > max_countries_allowed
  ) |>
  dplyr::arrange(
    dplyr::desc(country_count),
    record_sequence
  ) |>
  dplyr::select(
    record_sequence,
    record_id,
    title,
    abstract,
    matched_places,
    countries_mentioned,
    iso3c,
    country_count,
    regions_mentioned,
    region_count,
    ambiguous_mention,
    geography_review_required
  ) |>
  dplyr::mutate(
    countries_correct = NA_character_,
    corrected_countries = NA_character_,
    false_positive_places = NA_character_,
    validation_notes = NA_character_
  )

high_count_mentions <- mentions |>
  dplyr::semi_join(
    high_count_records |>
      dplyr::select(
        record_sequence
      ),
    by = "record_sequence"
  ) |>
  dplyr::left_join(
    records |>
      dplyr::select(
        record_sequence,
        title
      ),
    by = "record_sequence"
  ) |>
  dplyr::select(
    record_sequence,
    record_id,
    title,
    source,
    matched_text,
    matched_place,
    country_name,
    iso3c,
    region_name,
    match_type,
    context,
    ambiguous
  ) |>
  dplyr::arrange(
    dplyr::desc(
      record_sequence %in%
        high_count_records$record_sequence
    ),
    record_sequence,
    source,
    matched_place
  )

matched_place_frequency <- high_count_mentions |>
  dplyr::distinct(
    record_sequence,
    matched_place,
    country_name,
    iso3c,
    match_type
  ) |>
  dplyr::count(
    matched_place,
    country_name,
    iso3c,
    match_type,
    name = "records",
    sort = TRUE
  ) |>
  dplyr::mutate(
    decision = NA_character_,
    notes = NA_character_
  )

summary_sheet <- tibble::tibble(
  measure = c(
    "Country-count threshold",
    "Records included",
    "Maximum countries in one record",
    "Mention rows included",
    "Unique matched-place mappings"
  ),
  value = c(
    paste0(
      max_countries_allowed + 1L,
      " or more countries"
    ),
    nrow(high_count_records),
    if (
      nrow(high_count_records) == 0L
    ) {
      0L
    } else {
      max(
        high_count_records$country_count,
        na.rm = TRUE
      )
    },
    nrow(high_count_mentions),
    nrow(matched_place_frequency)
  )
)

output_path <- fs::path(
  output_dir,
  paste0(
    "geography_records_",
    max_countries_allowed + 1L,
    "_plus_countries.xlsx"
  )
)

wb <- openxlsx2::wb_workbook()

wb$add_worksheet("Summary")
wb$add_data(
  "Summary",
  summary_sheet
)

wb$add_worksheet("Records to review")
wb$add_data(
  "Records to review",
  high_count_records
)

wb$add_worksheet("Matched mentions")
wb$add_data(
  "Matched mentions",
  high_count_mentions
)

wb$add_worksheet("Matched-place frequency")
wb$add_data(
  "Matched-place frequency",
  matched_place_frequency
)

wb$add_worksheet("Instructions")
wb$add_data(
  "Instructions",
  tibble::tribble(
    ~sheet, ~field, ~instruction,
    "Records to review",
    "countries_correct",
    "Enter Yes, Partial or No for the complete set of detected countries.",
    "Records to review",
    "corrected_countries",
    "For Partial or No, enter the correct countries separated by semicolons.",
    "Records to review",
    "false_positive_places",
    "List matched place names that caused incorrect country assignments.",
    "Records to review",
    "validation_notes",
    "Add a brief explanation where useful.",
    "Matched-place frequency",
    "decision",
    "Optionally enter keep, remove or review for systematic place-name errors."
  )
)

# Basic workbook formatting ----------------------------------------------------

for (sheet_name in wb$get_sheet_names()) {
  
  wb$freeze_pane(
    sheet = sheet_name,
    first_row = TRUE
  )
  
  wb$set_col_widths(
    sheet = sheet_name,
    cols = 1:50,
    widths = "auto"
  )
}

# Cap text-heavy columns so the workbook remains readable.
wb$set_col_widths(
  "Records to review",
  cols = c(3, 4, 5, 6, 13, 14, 15, 16),
  widths = c(45, 70, 35, 45, 18, 30, 30, 45)
)

wb$set_col_widths(
  "Matched mentions",
  cols = c(3, 11),
  widths = c(45, 80)
)

wb$set_col_widths(
  "Matched-place frequency",
  cols = c(1, 2, 4, 6),
  widths = c(28, 24, 24, 35)
)

wb$save(
  output_path,
  overwrite = TRUE
)

message("High-country-count review workbook created.")
message(
  "Threshold: ",
  max_countries_allowed + 1L,
  " or more countries"
)
message(
  "Records included: ",
  nrow(high_count_records)
)
message(
  "Maximum countries in one record: ",
  if (
    nrow(high_count_records) == 0L
  ) {
    0L
  } else {
    max(
      high_count_records$country_count,
      na.rm = TRUE
    )
  }
)
message("Workbook: ", output_path)
