# =============================================================================
# File: 39_compare_primary_country_v1_1_to_validation.R
# Project: salmonscopingreview
# Purpose: Compare classifier v1.1 against the user's existing validation
#          judgements without requesting any new manual coding
# =============================================================================

source("scripts/00_setup.R")

input_validation <- here::here(
  "outputs",
  "stage_5_geography",
  "primary_study_country",
  "validation",
  "primary_country_validation.xlsx"
)

input_v1_1_summary <- here::here(
  "outputs",
  "stage_5_geography",
  "primary_study_country_v1_1",
  "primary_country_summary_v1_1.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_5_geography",
  "primary_study_country_v1_1",
  "validation"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(input_validation),
  file.exists(input_v1_1_summary)
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

v1_1 <- readr::read_csv(
  input_v1_1_summary,
  show_col_types = FALSE
) |>
  dplyr::mutate(
    record_id = as.character(record_id)
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
    v1_1 |>
      dplyr::select(
        record_sequence,
        record_id,
        primary_countries_v1_1 = primary_countries,
        primary_iso3c_v1_1 = primary_iso3c,
        primary_country_count_v1_1 = primary_country_count,
        review_required_v1_1 = review_required,
        assignment_reasons_v1_1 = assignment_reasons
      ),
    by = c(
      "record_sequence",
      "record_id"
    )
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    original_assignment_set = normalise_country_set(
      primary_countries
    ),
    revised_assignment_set = normalise_country_set(
      primary_countries_v1_1
    ),
    gold_standard = dplyr::case_when(
      correct == "yes" ~ original_assignment_set,
      correct == "no" &
        nzchar(corrected_country) ~
        normalise_country_set(corrected_country),
      TRUE ~ ""
    ),
    revised_matches_gold = (
      nzchar(gold_standard) &
        revised_assignment_set == gold_standard
    ),
    comparison_result = dplyr::case_when(
      !nzchar(gold_standard) ~ "No usable previous gold standard",
      revised_matches_gold ~ "Correct",
      TRUE ~ "Incorrect"
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(
    comparison_result,
    stratum,
    record_sequence
  )

summary_tbl <- comparison |>
  dplyr::count(
    comparison_result,
    name = "records",
    sort = TRUE
  )

stratum_summary <- comparison |>
  dplyr::filter(
    comparison_result %in% c(
      "Correct",
      "Incorrect"
    )
  ) |>
  dplyr::count(
    stratum,
    comparison_result,
    name = "records"
  ) |>
  tidyr::pivot_wider(
    names_from = comparison_result,
    values_from = records,
    values_fill = 0
  ) |>
  dplyr::mutate(
    total = Correct + Incorrect,
    accuracy = Correct / total
  )

incorrect_only <- comparison |>
  dplyr::filter(
    comparison_result == "Incorrect"
  ) |>
  dplyr::select(
    record_sequence,
    stratum,
    title,
    abstract,
    original_assignment = primary_countries,
    revised_assignment = primary_countries_v1_1,
    gold_standard,
    previous_notes = notes,
    review_required_v1_1,
    assignment_reasons_v1_1
  )

xlsx_path <- fs::path(
  output_dir,
  "primary_country_v1_1_validation_comparison.xlsx"
)

csv_path <- fs::path(
  output_dir,
  "primary_country_v1_1_validation_comparison.csv"
)

readr::write_csv(
  comparison,
  csv_path,
  na = ""
)

wb <- openxlsx2::wb_workbook()

wb$add_worksheet("Summary")
wb$add_data(
  "Summary",
  summary_tbl
)

wb$add_worksheet("By stratum")
wb$add_data(
  "By stratum",
  stratum_summary
)

wb$add_worksheet("Incorrect only")
wb$add_data(
  "Incorrect only",
  incorrect_only
)
wb$freeze_pane(
  sheet = "Incorrect only",
  first_row = TRUE
)

wb$save(
  xlsx_path,
  overwrite = TRUE
)

message("Primary-country v1.1 validation comparison completed.")
message(
  "Correct: ",
  sum(comparison$comparison_result == "Correct")
)
message(
  "Incorrect: ",
  sum(comparison$comparison_result == "Incorrect")
)
message(
  "No usable previous gold standard: ",
  sum(
    comparison$comparison_result ==
      "No usable previous gold standard"
  )
)
message(
  "Validated accuracy: ",
  round(
    mean(
      comparison$comparison_result[
        comparison$comparison_result %in%
          c("Correct", "Incorrect")
      ] == "Correct"
    ) * 100,
    1
  ),
  "%"
)
message("Workbook: ", xlsx_path)
