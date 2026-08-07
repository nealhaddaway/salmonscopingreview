# =============================================================================
# File: scripts/60_prepare_relevance_training_data.R
# Purpose: Build the binary relevance training set from the historical
#          12,074 includes and EXCLUDES.ris.
# =============================================================================

source("scripts/00_setup.R")
source("R/read_corpus.R")
source("R/relevance_screening.R")

ensure_relevance_packages()

include_file <- here::here(
  "data_raw",
  "INCLUDES fixed abstracts.txt"
)

exclude_file <- here::here(
  "data_raw",
  "EXCLUDES.ris"
)

output_dir <- here::here(
  "outputs",
  "stage_5_relevance_screening",
  "training"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(include_file),
  file.exists(exclude_file)
)

records <- build_training_records(
  include_file,
  exclude_file
)

conflicts <- find_label_conflicts(records)

if (nrow(conflicts) > 0L) {
  readr::write_csv(
    conflicts,
    fs::path(
      output_dir,
      "relevance_label_conflicts.csv"
    ),
    na = ""
  )

  conflicting_titles <- unique(
    conflicts$title_key[
      conflicts$conflict_basis == "exact normalised title"
    ]
  )

  conflicting_dois <- unique(
    conflicts$doi_key[
      conflicts$conflict_basis == "exact normalised DOI"
    ]
  )

  records <- records |>
    dplyr::filter(
      !title_key %in% conflicting_titles,
      is.na(doi_key) | !doi_key %in% conflicting_dois
    )
}

training <- collapse_training_duplicates(records) |>
  stratified_group_split(
    validation_fraction = 0.20,
    seed = 20260806L
  )

summary <- training |>
  dplyr::count(
    eligibility,
    validation,
    has_abstract,
    name = "records"
  )

readr::write_csv(
  training,
  fs::path(
    output_dir,
    "relevance_training_records.csv"
  ),
  na = ""
)

readr::write_csv(
  summary,
  fs::path(
    output_dir,
    "relevance_training_summary.csv"
  ),
  na = ""
)

message("")
message("Relevance training data prepared.")
message("Records retained: ", nrow(training))
message("Includes: ", sum(training$eligibility == 1L))
message("Excludes: ", sum(training$eligibility == 0L))
message("Validation records: ", sum(training$validation))
message("Label-conflict rows removed: ", nrow(conflicts))
message("")
message(
  "Next run: source(\"scripts/61_train_validate_relevance_classifier.R\")"
)
