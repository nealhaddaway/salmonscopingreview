# =============================================================================
# File: 30_audit_study_location_annotations.R
# Project: salmonscopingreview
# Purpose: Audit study-location assignments and review burden before any
#          further rule changes
# =============================================================================

source("scripts/00_setup.R")

input_summary <- here::here(
  "outputs",
  "stage_5_geography",
  "study_locations",
  "study_location_record_summary.csv"
)

input_annotations <- here::here(
  "outputs",
  "stage_5_geography",
  "study_locations",
  "study_country_annotations.csv"
)

input_review <- here::here(
  "outputs",
  "stage_5_geography",
  "study_locations",
  "study_location_review_queue.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_5_geography",
  "study_locations",
  "audit"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(input_summary),
  file.exists(input_annotations),
  file.exists(input_review)
)

summary_tbl <- readr::read_csv(
  input_summary,
  show_col_types = FALSE
)

annotations <- readr::read_csv(
  input_annotations,
  show_col_types = FALSE
)

review_queue <- readr::read_csv(
  input_review,
  show_col_types = FALSE
)

assignment_reason_counts <- annotations |>
  dplyr::count(
    selection_reason,
    name = "assignment_rows",
    sort = TRUE
  )

record_status_counts <- summary_tbl |>
  dplyr::mutate(
    status = dplyr::case_when(
      study_country_count == 0L &
        unresolved_country_count == 0L ~
        "No country detected",
      study_country_count == 0L &
        unresolved_country_count > 0L ~
        "Unresolved only",
      study_country_count == 1L &
        unresolved_country_count == 0L ~
        "One assigned, no unresolved",
      study_country_count == 1L &
        unresolved_country_count > 0L ~
        "One assigned plus unresolved",
      study_country_count > 1L &
        unresolved_country_count == 0L ~
        "Multiple assigned, no unresolved",
      study_country_count > 1L &
        unresolved_country_count > 0L ~
        "Multiple assigned plus unresolved",
      TRUE ~
        "Other"
    )
  ) |>
  dplyr::count(
    status,
    name = "records",
    sort = TRUE
  )

unresolved_distribution <- summary_tbl |>
  dplyr::count(
    unresolved_country_count,
    name = "records",
    sort = FALSE
  )

assigned_distribution <- summary_tbl |>
  dplyr::count(
    study_country_count,
    name = "records",
    sort = FALSE
  )

top_review_country_sets <- review_queue |>
  dplyr::count(
    study_countries,
    all_detected_countries,
    name = "records",
    sort = TRUE
  )

# Small targeted sample for manual inspection:
# 10 title-country records, 10 abstract-derived records, 10 unresolved-only.
set.seed(20260803)

sample_up_to <- function(data, n) {
  if (nrow(data) <= n) {
    data
  } else {
    dplyr::slice_sample(data, n = n)
  }
}

title_sample <- summary_tbl |>
  dplyr::filter(
    has_title_country,
    study_country_count > 0L
  ) |>
  sample_up_to(10L) |>
  dplyr::mutate(
    validation_stratum = "title-derived"
  )

abstract_sample <- summary_tbl |>
  dplyr::filter(
    !has_title_country,
    study_country_count > 0L
  ) |>
  sample_up_to(10L) |>
  dplyr::mutate(
    validation_stratum = "abstract-derived"
  )

unresolved_sample <- summary_tbl |>
  dplyr::filter(
    study_country_count == 0L,
    unresolved_country_count > 0L
  ) |>
  sample_up_to(10L) |>
  dplyr::mutate(
    validation_stratum = "unresolved-only"
  )

validation_sample <- dplyr::bind_rows(
  title_sample,
  abstract_sample,
  unresolved_sample
) |>
  dplyr::arrange(
    validation_stratum,
    record_sequence
  ) |>
  dplyr::mutate(
    study_country_correct = NA_character_,
    corrected_study_countries = NA_character_,
    validation_notes = NA_character_
  )

csv_path <- fs::path(
  output_dir,
  "study_location_audit_sample_30.csv"
)

xlsx_path <- fs::path(
  output_dir,
  "study_location_audit_sample_30.xlsx"
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

wb$add_worksheet("Assignment reasons")
wb$add_data(
  "Assignment reasons",
  assignment_reason_counts
)

wb$add_worksheet("Record statuses")
wb$add_data(
  "Record statuses",
  record_status_counts
)

wb$add_worksheet("Unresolved distribution")
wb$add_data(
  "Unresolved distribution",
  unresolved_distribution
)

wb$add_worksheet("Assigned distribution")
wb$add_data(
  "Assigned distribution",
  assigned_distribution
)

wb$add_worksheet("Top review country sets")
wb$add_data(
  "Top review country sets",
  top_review_country_sets
)

wb$add_worksheet("Instructions")
wb$add_data(
  "Instructions",
  tibble::tribble(
    ~field, ~instruction,
    "study_country_correct",
    "Enter Yes, Partial or No for the assigned study-country set.",
    "corrected_study_countries",
    "For Partial or No, enter the correct study countries separated by semicolons. Leave blank where no study country can be determined.",
    "validation_notes",
    "Briefly explain false positives, false negatives or unresolved background mentions."
  )
)

wb$save(
  xlsx_path,
  overwrite = TRUE
)

message("Study-location audit created.")
message("Assignment reasons:")
print(assignment_reason_counts, n = Inf)
message("Record statuses:")
print(record_status_counts, n = Inf)
message("Validation records: ", nrow(validation_sample))
message("Workbook: ", xlsx_path)
