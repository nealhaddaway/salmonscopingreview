# =============================================================================
# File: 37_create_clean_primary_country_review.R
# Project: salmonscopingreview
# Purpose: Create a clean, minimal workbook containing only the v1/v2 records
#          that genuinely require manual review
# =============================================================================

source("scripts/00_setup.R")

input_comparison <- here::here(
  "outputs",
  "stage_5_geography",
  "primary_study_country_v2",
  "validation",
  "primary_country_v1_v2_comparison.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_5_geography",
  "primary_study_country_v2",
  "validation"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(input_comparison)
)

comparison <- readr::read_csv(
  input_comparison,
  show_col_types = FALSE
)

manual_review <- comparison |>
  dplyr::filter(
    manual_review_needed
  ) |>
  dplyr::transmute(
    record_sequence,
    stratum,
    title,
    abstract,

    previous_assignment = primary_countries_v1,
    revised_assignment = primary_countries_v2,
    previous_judgement = correct,
    previous_corrected_country = corrected_country,
    previous_notes = notes,

    comparison_outcome,
    revised_review_required = review_required_v2,
    revised_review_reason = review_reason_v2,

    revised_assignment_correct = NA_character_,
    correct_primary_country = NA_character_,
    review_notes = NA_character_
  ) |>
  dplyr::arrange(
    comparison_outcome,
    stratum,
    record_sequence
  )

outcome_summary <- manual_review |>
  dplyr::count(
    comparison_outcome,
    name = "records",
    sort = TRUE
  )

instructions <- tibble::tribble(
  ~column, ~what_to_do,
  "revised_assignment_correct",
  "Enter Yes if the revised assignment is correct. Enter No if it is wrong.",
  "correct_primary_country",
  "Only for No: enter the correct primary country or countries, separated by semicolons. Leave blank if no country can be determined.",
  "review_notes",
  "Only where useful: briefly explain why the revised assignment is wrong or uncertain.",
  "previous_assignment",
  "The country assignment from classifier v1.",
  "revised_assignment",
  "The country assignment from classifier v2.",
  "previous_judgement",
  "Your earlier Yes/No judgement of v1.",
  "previous_corrected_country",
  "Your earlier correction where v1 was wrong.",
  "comparison_outcome",
  "Why this record still needs manual checking."
)

output_path <- fs::path(
  output_dir,
  "primary_country_manual_review_clean.xlsx"
)

wb <- openxlsx2::wb_workbook()

wb$add_worksheet("REVIEW THESE")
wb$add_data(
  "REVIEW THESE",
  manual_review
)
wb$freeze_pane(
  sheet = "REVIEW THESE",
  first_row = TRUE
)

wb$add_worksheet("Instructions")
wb$add_data(
  "Instructions",
  instructions
)

wb$add_worksheet("Summary")
wb$add_data(
  "Summary",
  outcome_summary
)

# Set readable widths without relying on unsupported filter arguments.
wb$set_col_widths(
  sheet = "REVIEW THESE",
  cols = 1,
  widths = 12
)
wb$set_col_widths(
  sheet = "REVIEW THESE",
  cols = 2,
  widths = 18
)
wb$set_col_widths(
  sheet = "REVIEW THESE",
  cols = 3,
  widths = 45
)
wb$set_col_widths(
  sheet = "REVIEW THESE",
  cols = 4,
  widths = 80
)
wb$set_col_widths(
  sheet = "REVIEW THESE",
  cols = 5:13,
  widths = 24
)
wb$set_col_widths(
  sheet = "REVIEW THESE",
  cols = 14:16,
  widths = 28
)

wb$save(
  output_path,
  overwrite = TRUE
)

message("Clean primary-country review workbook created.")
message("Records to review: ", nrow(manual_review))
message("Workbook: ", output_path)
