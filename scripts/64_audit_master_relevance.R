# =============================================================================
# File: scripts/64_audit_master_relevance.R
# Purpose: Read-only QA audit of the existing included master corpus using the
#          validated lightweight relevance model. Does not alter the master.
# =============================================================================

source("scripts/00_setup.R")
source("R/read_corpus.R")
source("R/relevance_screening.R")

ensure_relevance_packages()

include_file <- here::here(
  "data_raw",
  "INCLUDES fixed abstracts.txt"
)

model_file <- here::here(
  "outputs",
  "stage_5_relevance_screening",
  "model",
  "salmon_farming_relevance_model.rds"
)

output_dir <- here::here(
  "outputs",
  "master_relevance_audit"
)

fs::dir_create(output_dir)

# Do not attempt an expensive model run when a required input is absent.
required_files <- c(
  include_corpus = include_file,
  relevance_model = model_file
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  diagnostic <- tibble::tibble(
    item = names(missing_files),
    expected_path = unname(missing_files),
    exists = file.exists(missing_files),
    working_directory = getwd(),
    repository_root = here::here(),
    repository_commit = system2("git", c("rev-parse", "HEAD"), stdout = TRUE)
  )
  readr::write_csv(
    diagnostic,
    fs::path(output_dir, "preflight_missing_files.csv"),
    na = ""
  )
  stop(
    paste0(
      "Master relevance audit preflight failed. Missing required file(s): ",
      paste(unname(missing_files), collapse = "; "),
      ". The validated inputs were not supplied to the workflow runner."
    ),
    call. = FALSE
  )
}

message("Master relevance audit: loading included master corpus.")
master <- read_corpus(include_file) |>
  dplyr::mutate(master_decision = "include") |>
  add_screening_keys()

message(sprintf("Master relevance audit: loaded %d included records.", nrow(master)))
message("Master relevance audit: loading validated relevance model.")
fitted <- readRDS(model_file)
message(sprintf("Master relevance audit: model loaded with %d features.", length(fitted$model$features)))
message("Master relevance audit: scoring master corpus.")
master$probability_relevant <- predict_relevance_probability(fitted$model, master)
master$model_decision <- assign_screening_decision(master$probability_relevant, fitted$thresholds)

# A disagreement is an included master record that the validated model would
# automatically exclude. Keep this as an explicit master-level flag so it is
# available both to the review queue and to the exported audit tables.
master$model_disagreement <- master$model_decision == "automatic_exclude"
master$model_uncertain <- master$model_decision == "review"
master$review_priority <- dplyr::case_when(
  master$model_disagreement ~ "HIGH: model recommends exclusion",
  master$model_uncertain ~ "MEDIUM: model is uncertain",
  TRUE ~ "LOW: model supports inclusion"
)
master$distance_to_exclusion_threshold <- master$probability_relevant - fitted$thresholds$exclude_threshold

message("Master relevance audit: ranking records for manual review.")
# Important: after filtering, use column names (not master$column) inside the
# dplyr pipeline. master$column refers to the full 12,074-row vector and causes
# a size mismatch when review_queue has fewer rows.
review_queue <- master |>
  dplyr::filter(model_disagreement | model_uncertain) |>
  dplyr::arrange(dplyr::desc(model_disagreement), distance_to_exclusion_threshold, probability_relevant) |>
  dplyr::select(record_id, title, abstract, authors, year, doi, probability_relevant, model_decision, model_disagreement, model_uncertain, review_priority, distance_to_exclusion_threshold)

high_priority <- review_queue |> dplyr::filter(model_disagreement)
uncertain <- review_queue |> dplyr::filter(model_uncertain)

readr::write_csv(master |> dplyr::select(record_id, title, abstract, authors, year, doi, master_decision, probability_relevant, model_decision, model_disagreement, model_uncertain, review_priority, distance_to_exclusion_threshold), fs::path(output_dir, "master_relevance_scores.csv"), na = "")
readr::write_csv(high_priority, fs::path(output_dir, "high_priority_reconsideration.csv"), na = "")
readr::write_csv(uncertain, fs::path(output_dir, "uncertain_reconsideration.csv"), na = "")

summary <- tibble::tibble(
  item = c("Included master records scored", "Automatic-exclude model recommendations", "Model-uncertain records", "Model supports inclusion", "Validated exclusion threshold", "Validated inclusion threshold"),
  value = c(nrow(master), nrow(high_priority), nrow(uncertain), sum(master$model_decision == "automatic_retain"), fitted$thresholds$exclude_threshold, fitted$thresholds$include_threshold)
)
readr::write_csv(summary, fs::path(output_dir, "audit_summary.csv"), na = "")

wb <- openxlsx2::wb_workbook()
wb$add_worksheet("High priority")
wb$add_data("High priority", high_priority)
wb$freeze_pane("High priority", first_row = TRUE)
wb$add_worksheet("Uncertain")
wb$add_data("Uncertain", uncertain)
wb$freeze_pane("Uncertain", first_row = TRUE)
wb$add_worksheet("Summary")
wb$add_data("Summary", summary)
wb$save(fs::path(output_dir, "master_relevance_audit.xlsx"), overwrite = TRUE)

message("")
message("Master relevance audit completed; master corpus was not modified.")
print(summary)
message("")
message("Review workbook: ", fs::path(output_dir, "master_relevance_audit.xlsx"))
