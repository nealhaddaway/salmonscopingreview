# =============================================================================
# File: 11_validate_llm_classifier.R
# Project: salmonscopingreview
# Purpose: Run and export a 100-record validation sample for the five-stage
#          hierarchical LLM topic classifier
# =============================================================================

source("scripts/00_setup.R")
source("R/read_corpus.R")
source("R/classify_broad_topics.R")
source("R/classify_subtopics.R")
source("R/classify_features.R")
source("R/classify_components.R")
source("R/classify_terms.R")
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

input_topic_dictionary <- here::here(
  "data_raw",
  "Salmon scoping review keywords - FULL dictionary_NO_FARMED_SPECIES.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_4_llm",
  "validation"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(input_records),
  file.exists(input_ontology),
  file.exists(input_topic_dictionary)
)

records <- read_corpus(input_records)

ontology <- readr::read_csv(
  input_ontology,
  show_col_types = FALSE
)

topic_dictionary <- readr::read_csv(
  input_topic_dictionary,
  show_col_types = FALSE
) |>
  janitor::clean_names()

# Fixed validation sample ------------------------------------------------------

set.seed(20260801)

validation_records <- records |>
  dplyr::filter(
    !is.na(title),
    nzchar(title),
    !is.na(abstract),
    nzchar(abstract)
  ) |>
  dplyr::slice_sample(n = 100L) |>
  dplyr::select(
    record_sequence,
    record_id,
    title,
    abstract
  ) |>
  dplyr::arrange(record_sequence)

# Classify one record ----------------------------------------------------------

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

classify_one_record <- function(
    record_sequence,
    record_id,
    title,
    abstract
) {

  result <- tryCatch(
    classify_topic_hierarchy(
      title = title,
      abstract = abstract,
      ontology = ontology,
      topic_dictionary = topic_dictionary
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
    return(
      tibble::tibble(
        record_sequence = record_sequence,
        record_id = record_id,
        title = title,
        abstract = abstract,
        predicted_broad_topics = NA_character_,
        predicted_subtopics = NA_character_,
        predicted_features = NA_character_,
        predicted_components = NA_character_,
        predicted_terms = NA_character_,
        predicted_paths = NA_character_,
        review_required = TRUE,
        classification_failed = TRUE,
        classification_error = result$message
      )
    )
  }

  predicted_subtopics <- result |>
    dplyr::filter(
      !is.na(subtopic)
    ) |>
    dplyr::transmute(
      value = paste(
        broad_topic,
        subtopic,
        sep = " > "
      )
    ) |>
    dplyr::pull(value) |>
    collapse_unique()

  predicted_features <- result |>
    dplyr::filter(
      !is.na(feature)
    ) |>
    dplyr::transmute(
      value = paste(
        broad_topic,
        subtopic,
        feature,
        sep = " > "
      )
    ) |>
    dplyr::pull(value) |>
    collapse_unique()

  predicted_components <- result |>
    dplyr::filter(
      !is.na(component)
    ) |>
    dplyr::transmute(
      value = paste(
        broad_topic,
        subtopic,
        feature,
        component,
        sep = " > "
      )
    ) |>
    dplyr::pull(value) |>
    collapse_unique()

  predicted_paths <- result |>
    dplyr::filter(
      !is.na(term)
    ) |>
    dplyr::transmute(
      value = paste(
        broad_topic,
        subtopic,
        feature,
        component,
        term,
        sep = " > "
      )
    ) |>
    dplyr::pull(value) |>
    collapse_unique()

  tibble::tibble(
    record_sequence = record_sequence,
    record_id = record_id,
    title = title,
    abstract = abstract,
    predicted_broad_topics = collapse_unique(
      result$broad_topic
    ),
    predicted_subtopics = predicted_subtopics,
    predicted_features = predicted_features,
    predicted_components = predicted_components,
    predicted_terms = collapse_unique(
      result$term
    ),
    predicted_paths = predicted_paths,
    review_required = any(
      result$review_required %in% TRUE
    ),
    classification_failed = FALSE,
    classification_error = NA_character_
  )
}

# Run classifier ---------------------------------------------------------------

validation_results <- purrr::pmap_dfr(
  validation_records,
  classify_one_record,
  .progress = TRUE
)

# Add blank validation fields --------------------------------------------------

validation_results <- validation_results |>
  dplyr::mutate(
    broad_correct = NA_character_,
    subtopic_correct = NA_character_,
    feature_correct = NA_character_,
    component_correct = NA_character_,
    term_correct = NA_character_,
    corrected_paths = NA_character_,
    validation_notes = NA_character_
  )

# Write outputs ----------------------------------------------------------------

csv_path <- fs::path(
  output_dir,
  "llm_validation_100.csv"
)

xlsx_path <- fs::path(
  output_dir,
  "llm_validation_100.xlsx"
)

readr::write_csv(
  validation_results,
  csv_path,
  na = ""
)

wb <- openxlsx2::wb_workbook()

wb$add_worksheet("Validation 100")
wb$add_data(
  "Validation 100",
  validation_results
)

wb$add_worksheet("Instructions")
wb$add_data(
  "Instructions",
  tibble::tibble(
    field = c(
      "broad_correct",
      "subtopic_correct",
      "feature_correct",
      "component_correct",
      "term_correct",
      "corrected_paths",
      "validation_notes"
    ),
    permitted_values = c(
      "Yes / Partial / No",
      "Yes / Partial / No",
      "Yes / Partial / No",
      "Yes / Partial / No",
      "Yes / Partial / No",
      "Complete corrected five-level paths, separated by semicolons",
      "Brief explanation of any error"
    )
  )
)

wb$save(
  xlsx_path,
  overwrite = TRUE
)

# Report ----------------------------------------------------------------------

failures <- validation_results |>
  dplyr::filter(
    classification_failed
  )

message("LLM validation sample completed.")
message("Validation records: ", nrow(validation_results))
message(
  "Successful classifications: ",
  sum(!validation_results$classification_failed)
)
message("Classification failures: ", nrow(failures))
message("CSV: ", csv_path)
message("Workbook: ", xlsx_path)
