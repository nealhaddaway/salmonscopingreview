# =============================================================================
# File: 12_run_full_llm_topic_classification.R
# Project: salmonscopingreview
# Purpose: Run the validated four-level hierarchical LLM topic classifier
#          across the full corpus with resumable checkpoints
# =============================================================================

source("scripts/00_setup.R")
source("R/read_corpus.R")
source("R/llm_prompts.R")
source("R/classify_broad_topics.R")
source("R/classify_subtopics.R")
source("R/classify_features.R")
source("R/classify_components.R")
source("R/classify_topic_hierarchy.R")

# Inputs ----------------------------------------------------------------------

input_records <- here::here(
  "data_raw",
  "INCLUDES fixed abstracts.txt"
)

input_ontology <- here::here(
  "outputs",
  "stage_4_llm",
  "llm_topic_ontology.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_4_llm",
  "full_corpus"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(input_records),
  file.exists(input_ontology)
)

records <- read_corpus(input_records)

ontology <- readr::read_csv(
  input_ontology,
  show_col_types = FALSE
)

# Run settings ----------------------------------------------------------------

model <- "gpt-5-mini"

checkpoint_every <- 100L

checkpoint_path <- fs::path(
  output_dir,
  "llm_topic_checkpoint.rds"
)

long_output_path <- fs::path(
  output_dir,
  "llm_topic_assignments_long.csv"
)

record_output_path <- fs::path(
  output_dir,
  "llm_topic_assignments_record.csv"
)

failure_output_path <- fs::path(
  output_dir,
  "llm_topic_failures.csv"
)

progress_output_path <- fs::path(
  output_dir,
  "llm_topic_progress.csv"
)

# Helpers ---------------------------------------------------------------------

empty_long_result <- function() {
  tibble::tibble(
    record_sequence = integer(),
    record_id = character(),
    broad_topic = character(),
    subtopic = character(),
    feature = character(),
    component = character(),
    broad_review_required = logical(),
    subtopic_review_required = logical(),
    feature_review_required = logical(),
    component_review_required = logical(),
    review_required = logical()
  )
}

empty_failure_result <- function() {
  tibble::tibble(
    record_sequence = integer(),
    record_id = character(),
    title = character(),
    classification_error = character()
  )
}

collapse_unique <- function(x) {

  values <- sort(
    unique(
      stats::na.omit(x)
    )
  )

  values <- values[
    nzchar(values)
  ]

  if (length(values) == 0L) {
    NA_character_
  } else {
    paste(
      values,
      collapse = "; "
    )
  }
}

write_current_outputs <- function(
    assignments_long,
    failures,
    completed_record_sequences
) {

  assignments_record <- records |>
    dplyr::filter(
      record_sequence %in% completed_record_sequences
    ) |>
    dplyr::select(
      record_sequence,
      record_id,
      title,
      abstract
    ) |>
    dplyr::left_join(
      assignments_long |>
        dplyr::group_by(
          record_sequence,
          record_id
        ) |>
        dplyr::summarise(
          assigned_broad_topics = collapse_unique(
            broad_topic
          ),
          assigned_subtopics = collapse_unique(
            paste(
              broad_topic,
              subtopic,
              sep = " > "
            )
          ),
          assigned_features = collapse_unique(
            paste(
              broad_topic,
              subtopic,
              feature,
              sep = " > "
            )
          ),
          assigned_components = collapse_unique(
            component
          ),
          assigned_paths = collapse_unique(
            paste(
              broad_topic,
              subtopic,
              feature,
              component,
              sep = " > "
            )
          ),
          review_required = any(
            review_required %in% TRUE
          ),
          .groups = "drop"
        ),
      by = c(
        "record_sequence",
        "record_id"
      )
    ) |>
    dplyr::left_join(
      failures |>
        dplyr::select(
          record_sequence,
          classification_error
        ) |>
        dplyr::mutate(
          classification_failed = TRUE
        ),
      by = "record_sequence"
    ) |>
    dplyr::mutate(
      classification_failed = dplyr::coalesce(
        classification_failed,
        FALSE
      )
    ) |>
    dplyr::arrange(
      record_sequence
    )

  readr::write_csv(
    assignments_long |>
      dplyr::arrange(
        record_sequence,
        broad_topic,
        subtopic,
        feature,
        component
      ),
    long_output_path,
    na = ""
  )

  readr::write_csv(
    assignments_record,
    record_output_path,
    na = ""
  )

  readr::write_csv(
    failures |>
      dplyr::arrange(
        record_sequence
      ),
    failure_output_path,
    na = ""
  )

  progress_tbl <- tibble::tibble(
    measure = c(
      "Corpus records",
      "Completed records",
      "Successful records",
      "Failed records",
      "Remaining records",
      "Long-form assignment rows"
    ),
    value = c(
      nrow(records),
      length(completed_record_sequences),
      length(completed_record_sequences) - nrow(failures),
      nrow(failures),
      nrow(records) - length(completed_record_sequences),
      nrow(assignments_long)
    )
  )

  readr::write_csv(
    progress_tbl,
    progress_output_path,
    na = ""
  )
}

# Resume or initialise ---------------------------------------------------------

if (file.exists(checkpoint_path)) {

  checkpoint <- readRDS(
    checkpoint_path
  )

  assignments_long <- checkpoint$assignments_long
  failures <- checkpoint$failures
  completed_record_sequences <- checkpoint$completed_record_sequences

  message(
    "Resuming from checkpoint."
  )

  message(
    "Already completed: ",
    length(completed_record_sequences)
  )

} else {

  assignments_long <- empty_long_result()
  failures <- empty_failure_result()
  completed_record_sequences <- integer()

  message(
    "Starting new full-corpus run."
  )
}

remaining_records <- records |>
  dplyr::filter(
    !record_sequence %in% completed_record_sequences
  ) |>
  dplyr::arrange(
    record_sequence
  )

message(
  "Corpus records: ",
  nrow(records)
)

message(
  "Remaining records: ",
  nrow(remaining_records)
)

# Main loop -------------------------------------------------------------------

if (nrow(remaining_records) > 0L) {

  for (i in seq_len(nrow(remaining_records))) {

    current <- remaining_records[i, ]

    result <- tryCatch(
      classify_topic_hierarchy(
        title = current$title[[1]],
        abstract = current$abstract[[1]],
        ontology = ontology,
        model = model
      ),
      error = function(e) {
        structure(
          list(
            message = conditionMessage(e)
          ),
          class = "classification_error"
        )
      }
    )

    if (inherits(result, "classification_error")) {

      failures <- dplyr::bind_rows(
        failures,
        tibble::tibble(
          record_sequence = current$record_sequence[[1]],
          record_id = current$record_id[[1]],
          title = current$title[[1]],
          classification_error = result$message
        )
      )

    } else {

      result <- result |>
        dplyr::mutate(
          record_sequence = current$record_sequence[[1]],
          record_id = current$record_id[[1]],
          .before = 1
        )

      assignments_long <- dplyr::bind_rows(
        assignments_long,
        result
      )
    }

    completed_record_sequences <- c(
      completed_record_sequences,
      current$record_sequence[[1]]
    )

    completed_total <- length(
      completed_record_sequences
    )

    if (
      completed_total %% checkpoint_every == 0L ||
        i == nrow(remaining_records)
    ) {

      checkpoint <- list(
        assignments_long = assignments_long,
        failures = failures,
        completed_record_sequences = unique(
          completed_record_sequences
        ),
        model = model,
        checkpoint_time = Sys.time()
      )

      saveRDS(
        checkpoint,
        checkpoint_path
      )

      write_current_outputs(
        assignments_long = assignments_long,
        failures = failures,
        completed_record_sequences = unique(
          completed_record_sequences
        )
      )

      message(
        "Checkpoint saved: ",
        completed_total,
        " / ",
        nrow(records),
        " records completed; ",
        nrow(failures),
        " failures."
      )
    }
  }
}

# Final outputs ----------------------------------------------------------------

completed_record_sequences <- unique(
  completed_record_sequences
)

write_current_outputs(
  assignments_long = assignments_long,
  failures = failures,
  completed_record_sequences = completed_record_sequences
)

capture.output(
  sessionInfo(),
  file = fs::path(
    output_dir,
    "session_info.txt"
  )
)

message("Full-corpus LLM topic classification finished.")
message("Corpus records: ", nrow(records))
message("Completed records: ", length(completed_record_sequences))
message(
  "Successful records: ",
  length(completed_record_sequences) - nrow(failures)
)
message("Failed records: ", nrow(failures))
message("Long-form assignment rows: ", nrow(assignments_long))
