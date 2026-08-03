# =============================================================================
# File: 36_compare_primary_country_v1_v2.R
# Project: salmonscopingreview
# Purpose: Compare v1 and v2 primary-country outputs against the user's prior
#          validation judgements, requiring manual review only where necessary
# =============================================================================

source("scripts/00_setup.R")

input_validation <- here::here(
  "outputs",
  "stage_5_geography",
  "primary_study_country",
  "validation",
  "primary_country_validation.xlsx"
)

input_v1_summary <- here::here(
  "outputs",
  "stage_5_geography",
  "primary_study_country",
  "primary_country_summary.csv"
)

input_v2_summary <- here::here(
  "outputs",
  "stage_5_geography",
  "primary_study_country_v2",
  "primary_country_summary_v2.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_5_geography",
  "primary_study_country_v2",
  "validation"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(input_validation),
  file.exists(input_v1_summary),
  file.exists(input_v2_summary)
)

validation <- openxlsx2::read_xlsx(
  input_validation,
  sheet = "Validation"
) |>
  dplyr::mutate(
    record_id = as.character(record_id),
    correct = stringr::str_to_lower(
      stringr::str_squish(
        dplyr::coalesce(correct, "")
      )
    ),
    corrected_country = stringr::str_squish(
      dplyr::coalesce(corrected_country, "")
    )
  )

v1 <- readr::read_csv(
  input_v1_summary,
  show_col_types = FALSE
) |>
  dplyr::mutate(
    record_id = as.character(record_id)
  ) |>
  dplyr::select(
    record_sequence,
    record_id,
    primary_countries_v1 = primary_countries,
    primary_iso3c_v1 = primary_iso3c,
    primary_country_count_v1 = primary_country_count,
    review_required_v1 = review_required
  )

v2 <- readr::read_csv(
  input_v2_summary,
  show_col_types = FALSE
) |>
  dplyr::mutate(
    record_id = as.character(record_id)
  ) |>
  dplyr::select(
    record_sequence,
    record_id,
    primary_countries_v2 = primary_countries,
    primary_iso3c_v2 = primary_iso3c,
    primary_country_count_v2 = primary_country_count,
    review_required_v2 = review_required,
    review_reason_v2 = review_reason
  )

normalise_country_set <- function(x) {

  if (
    length(x) == 0L ||
      is.na(x) ||
      !nzchar(stringr::str_squish(x))
  ) {
    return("")
  }

  x |>
    stringr::str_split("\\s*;\\s*") |>
    unlist(use.names = FALSE) |>
    stringr::str_squish() |>
    stringr::str_to_lower() |>
    unique() |>
    sort() |>
    paste(collapse = "; ")
}

comparison <- validation |>
  dplyr::left_join(
    v1,
    by = c(
      "record_sequence",
      "record_id"
    )
  ) |>
  dplyr::left_join(
    v2,
    by = c(
      "record_sequence",
      "record_id"
    )
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    v1_set = normalise_country_set(
      primary_countries_v1
    ),
    v2_set = normalise_country_set(
      primary_countries_v2
    ),

    # Construct the user's gold-standard country set from the previous review:
    # - If v1 was Yes, the v1 assignment is the accepted gold standard.
    # - If v1 was No and a correction was supplied, use that correction.
    # - Otherwise the gold standard is unresolved.
    gold_standard = dplyr::case_when(
      correct == "yes" ~ v1_set,
      correct == "no" &
        nzchar(corrected_country) ~
        normalise_country_set(corrected_country),
      TRUE ~ ""
    ),

    v1_matches_gold = (
      nzchar(gold_standard) &
        v1_set == gold_standard
    ),
    v2_matches_gold = (
      nzchar(gold_standard) &
        v2_set == gold_standard
    ),

    comparison_outcome = dplyr::case_when(
      !nzchar(gold_standard) ~
        "Needs manual inspection: no usable gold standard",

      v1_matches_gold &
        v2_matches_gold ~
        "Unchanged correct",

      !v1_matches_gold &
        v2_matches_gold ~
        "Improved",

      v1_matches_gold &
        !v2_matches_gold ~
        "Regressed",

      !v1_matches_gold &
        !v2_matches_gold &
        v1_set == v2_set ~
        "Unchanged incorrect",

      !v1_matches_gold &
        !v2_matches_gold &
        v1_set != v2_set ~
        "Changed but still incorrect",

      TRUE ~
        "Needs manual inspection"
    ),

    manual_review_needed = comparison_outcome %in% c(
      "Regressed",
      "Changed but still incorrect",
      "Needs manual inspection",
      "Needs manual inspection: no usable gold standard"
    ),

    manual_decision = NA_character_,
    manual_corrected_country = NA_character_,
    manual_notes = NA_character_
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(
    dplyr::desc(manual_review_needed),
    comparison_outcome,
    stratum,
    record_sequence
  )

outcome_summary <- comparison |>
  dplyr::count(
    comparison_outcome,
    name = "records",
    sort = TRUE
  )

stratum_summary <- comparison |>
  dplyr::count(
    stratum,
    comparison_outcome,
    name = "records"
  ) |>
  dplyr::arrange(
    stratum,
    dplyr::desc(records)
  )

manual_review <- comparison |>
  dplyr::filter(
    manual_review_needed
  )

csv_path <- fs::path(
  output_dir,
  "primary_country_v1_v2_comparison.csv"
)

xlsx_path <- fs::path(
  output_dir,
  "primary_country_v1_v2_comparison.xlsx"
)

readr::write_csv(
  comparison,
  csv_path,
  na = ""
)

wb <- openxlsx2::wb_workbook()

wb$add_worksheet("Comparison")
wb$add_data(
  "Comparison",
  comparison
)
wb$freeze_pane(
  sheet = "Comparison",
  first_row = TRUE
)

wb$add_worksheet("Manual review only")
wb$add_data(
  "Manual review only",
  manual_review
)
wb$freeze_pane(
  sheet = "Manual review only",
  first_row = TRUE
)

wb$add_worksheet("Outcome summary")
wb$add_data(
  "Outcome summary",
  outcome_summary
)

wb$add_worksheet("Stratum summary")
wb$add_data(
  "Stratum summary",
  stratum_summary
)

wb$add_worksheet("Instructions")
wb$add_data(
  "Instructions",
  tibble::tribble(
    ~field, ~instruction,
    "comparison_outcome",
    "Automatically compares v1 and v2 against your previous judgement.",
    "Manual review only",
    "Review only the records on this sheet.",
    "manual_decision",
    "Enter Yes if v2 is correct, otherwise No.",
    "manual_corrected_country",
    "For No, enter the correct primary country or countries.",
    "manual_notes",
    "Add a brief explanation where useful."
  )
)

wb$save(
  xlsx_path,
  overwrite = TRUE
)

message("Primary-country v1/v2 comparison completed.")
message("Validation records: ", nrow(comparison))
message(
  "Improved: ",
  sum(comparison$comparison_outcome == "Improved")
)
message(
  "Unchanged correct: ",
  sum(comparison$comparison_outcome == "Unchanged correct")
)
message(
  "Regressed: ",
  sum(comparison$comparison_outcome == "Regressed")
)
message(
  "Unchanged incorrect: ",
  sum(comparison$comparison_outcome == "Unchanged incorrect")
)
message(
  "Changed but still incorrect: ",
  sum(comparison$comparison_outcome == "Changed but still incorrect")
)
message(
  "Manual review records: ",
  nrow(manual_review)
)
message("Workbook: ", xlsx_path)
