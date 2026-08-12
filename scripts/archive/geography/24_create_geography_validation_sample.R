# =============================================================================
# File: 24_create_geography_validation_sample.R
# Project: salmonscopingreview
# Purpose: Create a small stratified validation sample for country annotation
# =============================================================================

source("scripts/00_setup.R")

input_summary <- here::here(
  "outputs",
  "stage_5_geography",
  "global_detection_v2",
  "global_geography_record_summary_v2.csv"
)

input_mentions <- here::here(
  "outputs",
  "stage_5_geography",
  "global_detection_v2",
  "global_geography_mentions_v2.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_5_geography",
  "validation"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(input_summary),
  file.exists(input_mentions)
)

record_summary <- readr::read_csv(
  input_summary,
  show_col_types = FALSE
)

mentions <- readr::read_csv(
  input_mentions,
  show_col_types = FALSE
)

set.seed(20260802)

# Three validation strata:
# 1. One country, no ambiguity: checks ordinary precision.
# 2. Multiple countries or ambiguity: checks difficult records.
# 3. No detected geography: checks false negatives / recall.
single_country <- record_summary |>
  dplyr::filter(
    country_count == 1L,
    !ambiguous_mention,
    !multiple_countries
  ) |>
  (\(x) if (nrow(x) <= 10L) x else dplyr::slice_sample(x, n = 10L))() |>
  dplyr::mutate(
    validation_stratum = "single country"
  )

difficult_records <- record_summary |>
  dplyr::filter(
    geography_review_required
  ) |>
  (\(x) if (nrow(x) <= 10L) x else dplyr::slice_sample(x, n = 10L))() |>
  dplyr::mutate(
    validation_stratum = "multiple or ambiguous"
  )

no_geography <- record_summary |>
  dplyr::filter(
    mention_count == 0L
  ) |>
  (\(x) if (nrow(x) <= 10L) x else dplyr::slice_sample(x, n = 10L))() |>
  dplyr::mutate(
    validation_stratum = "no geography detected"
  )

validation_sample <- dplyr::bind_rows(
  single_country,
  difficult_records,
  no_geography
) |>
  dplyr::arrange(
    validation_stratum,
    record_sequence
  ) |>
  dplyr::select(
    validation_stratum,
    record_sequence,
    record_id,
    title,
    abstract,
    matched_places,
    countries_mentioned,
    iso3c,
    country_count,
    ambiguous_mention,
    multiple_countries,
    geography_review_required
  ) |>
  dplyr::mutate(
    geography_correct = NA_character_,
    corrected_countries = NA_character_,
    validation_notes = NA_character_
  )

validation_mentions <- mentions |>
  dplyr::semi_join(
    validation_sample |>
      dplyr::select(
        record_sequence
      ),
    by = "record_sequence"
  ) |>
  dplyr::select(
    record_sequence,
    source,
    matched_text,
    matched_place,
    country_name,
    iso3c,
    match_type,
    context,
    ambiguous
  ) |>
  dplyr::arrange(
    record_sequence,
    source
  )

csv_path <- fs::path(
  output_dir,
  "geography_validation_30.csv"
)

xlsx_path <- fs::path(
  output_dir,
  "geography_validation_30.xlsx"
)

readr::write_csv(
  validation_sample,
  csv_path,
  na = ""
)

wb <- openxlsx2::wb_workbook()

wb$add_worksheet("Validation 30")
wb$add_data(
  "Validation 30",
  validation_sample
)

wb$add_worksheet("Matched mentions")
wb$add_data(
  "Matched mentions",
  validation_mentions
)

wb$add_worksheet("Instructions")
wb$add_data(
  "Instructions",
  tibble::tribble(
    ~field, ~instruction,
    "geography_correct",
    "Enter Yes, Partial or No for the complete set of country annotations.",
    "corrected_countries",
    "For Partial or No, enter the correct country names separated by semicolons. Leave blank if no country should be assigned.",
    "validation_notes",
    "Briefly explain false positives, false negatives or ambiguity where useful.",
    "no geography detected",
    "Check whether any country-level geography is present but was missed."
  )
)

wb$save(
  xlsx_path,
  overwrite = TRUE
)

message("Geography validation sample created.")
message("Validation records: ", nrow(validation_sample))
message(
  "Single-country records: ",
  sum(validation_sample$validation_stratum == "single country")
)
message(
  "Multiple/ambiguous records: ",
  sum(validation_sample$validation_stratum == "multiple or ambiguous")
)
message(
  "No-geography records: ",
  sum(validation_sample$validation_stratum == "no geography detected")
)
message("Workbook: ", xlsx_path)
