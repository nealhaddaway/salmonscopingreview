# =============================================================================
# File: 13_compare_single_call_classifier.R
# Project: salmonscopingreview
# Purpose: Compare the single-call classifier with the validated staged output
#          on the same 20-record confirmation sample
# =============================================================================

source("scripts/00_setup.R")
source("R/classify_topic_hierarchy_single_call.R")

input_ontology <- here::here(
  "outputs",
  "stage_4_llm",
  "llm_topic_ontology.csv"
)

input_staged_validation <- here::here(
  "outputs",
  "stage_4_llm",
  "validation",
  "llm_validation_20.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_4_llm",
  "single_call_validation"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(input_ontology),
  file.exists(input_staged_validation)
)

ontology <- readr::read_csv(
  input_ontology,
  show_col_types = FALSE
)

staged_validation <- readr::read_csv(
  input_staged_validation,
  show_col_types = FALSE
)

validation_records <- staged_validation |>
  dplyr::select(
    record_sequence,
    record_id,
    title,
    abstract,
    staged_predicted_paths = predicted_paths
  ) |>
  dplyr::distinct(record_sequence, .keep_all = TRUE) |>
  dplyr::arrange(record_sequence)

collapse_unique <- function(x) {
  values <- sort(unique(stats::na.omit(x)))
  values <- values[nzchar(values)]
  if (length(values) == 0L) {
    NA_character_
  } else {
    paste(values, collapse = "; ")
  }
}

classify_one_record <- function(
    record_sequence,
    record_id,
    title,
    abstract,
    staged_predicted_paths
) {

  started_at <- Sys.time()

  result <- tryCatch(
    classify_topic_hierarchy_single_call(
      title = title,
      abstract = abstract,
      ontology = ontology
    ),
    error = function(e) {
      structure(
        list(message = conditionMessage(e)),
        class = "classification_error"
      )
    }
  )

  elapsed_seconds <- as.numeric(
    difftime(Sys.time(), started_at, units = "secs")
  )

  if (inherits(result, "classification_error")) {
    return(
      tibble::tibble(
        record_sequence = record_sequence,
        record_id = record_id,
        title = title,
        abstract = abstract,
        staged_predicted_paths = staged_predicted_paths,
        single_call_predicted_paths = NA_character_,
        paths_identical = FALSE,
        review_required = TRUE,
        classification_failed = TRUE,
        classification_error = result$message,
        elapsed_seconds = elapsed_seconds,
        hierarchy_correct = NA_character_,
        validation_notes = NA_character_
      )
    )
  }

  single_call_paths <- result |>
    dplyr::transmute(
      path = paste(
        broad_topic,
        subtopic,
        feature,
        component,
        sep = " > "
      )
    ) |>
    dplyr::pull(path) |>
    collapse_unique()

  tibble::tibble(
    record_sequence = record_sequence,
    record_id = record_id,
    title = title,
    abstract = abstract,
    staged_predicted_paths = staged_predicted_paths,
    single_call_predicted_paths = single_call_paths,
    paths_identical = identical(
      staged_predicted_paths,
      single_call_paths
    ),
    review_required = any(result$review_required %in% TRUE),
    classification_failed = FALSE,
    classification_error = NA_character_,
    elapsed_seconds = elapsed_seconds,
    hierarchy_correct = NA_character_,
    validation_notes = NA_character_
  )
}

comparison_results <- purrr::pmap_dfr(
  validation_records,
  classify_one_record,
  .progress = TRUE
)

csv_path <- fs::path(
  output_dir,
  "single_call_comparison_20.csv"
)

xlsx_path <- fs::path(
  output_dir,
  "single_call_comparison_20.xlsx"
)

readr::write_csv(comparison_results, csv_path, na = "")

wb <- openxlsx2::wb_workbook()
wb$add_worksheet("Comparison 20")
wb$add_data("Comparison 20", comparison_results)
wb$add_worksheet("Instructions")
wb$add_data(
  "Instructions",
  tibble::tibble(
    field = c(
      "staged_predicted_paths",
      "single_call_predicted_paths",
      "paths_identical",
      "hierarchy_correct",
      "validation_notes"
    ),
    meaning = c(
      "Previously validated four-stage output",
      "New one-call output",
      "TRUE only when the collapsed path strings match exactly",
      "Judge the new one-call output: Yes / Partial / No",
      "Correct four-level path(s) only where Partial or No"
    )
  )
)
wb$save(xlsx_path, overwrite = TRUE)

message("Single-call comparison completed.")
message("Comparison records: ", nrow(comparison_results))
message(
  "Successful classifications: ",
  sum(!comparison_results$classification_failed)
)
message(
  "Classification failures: ",
  sum(comparison_results$classification_failed)
)
message(
  "Exactly identical path sets: ",
  sum(comparison_results$paths_identical)
)
message(
  "Median seconds per record: ",
  round(stats::median(comparison_results$elapsed_seconds), 1)
)
message("Workbook: ", xlsx_path)
