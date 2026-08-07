# =============================================================================
# File: scripts/63_screen_new_search_results.R
# Purpose: Import all RIS files in data_updates/incoming, conservatively
#          deduplicate them against the historical corpus, and classify new
#          records as automatic retain, automatic exclude or review.
# =============================================================================

source("scripts/00_setup.R")
source("R/read_corpus.R")
source("R/relevance_screening.R")

ensure_relevance_packages()

incoming_dir <- here::here(
  "data_updates",
  "incoming"
)

include_file <- here::here(
  "data_raw",
  "INCLUDES fixed abstracts.txt"
)

exclude_file <- here::here(
  "data_raw",
  "EXCLUDES.ris"
)

model_file <- here::here(
  "outputs",
  "stage_5_relevance_screening",
  "model",
  "salmon_farming_relevance_model.rds"
)

output_dir <- here::here(
  "outputs",
  "living_map_screening"
)

fs::dir_create(incoming_dir)
fs::dir_create(output_dir)

stopifnot(
  file.exists(include_file),
  file.exists(exclude_file),
  file.exists(model_file)
)

ris_files <- fs::dir_ls(
  incoming_dir,
  regexp = "\\.ris$",
  type = "file",
  recurse = FALSE
)

if (length(ris_files) == 0L) {
  stop(
    "No .ris files were found in: ",
    incoming_dir
  )
}

incoming <- read_ris_files(ris_files)

historical <- dplyr::bind_rows(
  read_corpus(include_file) |>
    dplyr::mutate(
      historical_decision = "include",
      source_file = basename(include_file)
    ),
  read_corpus(exclude_file) |>
    dplyr::mutate(
      historical_decision = "exclude",
      source_file = basename(exclude_file)
    )
)

# Deduplicate incoming records against the complete historical include/exclude
# corpus. Only definitive matches are removed automatically. Probable, possible
# and DOI-conflict matches are retained in a separate review file.
incoming_ordered <- incoming |>
  dplyr::mutate(
    incoming_global_row = dplyr::row_number()
  )

against_historical <- deduplicate_new_records(
  new_records = incoming_ordered,
  master_records = historical
)

historical_duplicates <- against_historical |>
  dplyr::filter(
    duplicate_status == "duplicate"
  )

duplicate_review <- against_historical |>
  dplyr::filter(
    duplicate_status %in% c(
      "probable_duplicate",
      "possible_duplicate",
      "doi_conflict_review"
    )
  )

new_candidates <- against_historical |>
  dplyr::filter(
    duplicate_status == "new"
  )

# Exact normalised titles identify duplicates within the incoming batch without
# relying on DOI. Keep the most complete representative.
incoming_representatives <- new_candidates |>
  add_screening_keys() |>
  dplyr::group_by(title_key) |>
  dplyr::arrange(
    dplyr::desc(has_abstract),
    dplyr::desc(nchar(abstract)),
    incoming_global_row
  ) |>
  dplyr::mutate(
    within_batch_rank = dplyr::row_number(),
    within_batch_count = dplyr::n()
  ) |>
  dplyr::ungroup()

within_batch_duplicates <- incoming_representatives |>
  dplyr::filter(
    within_batch_rank > 1L
  ) |>
  dplyr::mutate(
    duplicate_status = "duplicate",
    duplicate_basis = "exact normalised title within incoming batch"
  )

records_to_score <- incoming_representatives |>
  dplyr::filter(
    within_batch_rank == 1L
  )

fitted <- readRDS(model_file)

records_to_score <- records_to_score |>
  dplyr::mutate(
    probability_relevant = predict_relevance_probability(
      fitted$model,
      dplyr::pick(dplyr::everything())
    ),
    screening_decision = assign_screening_decision(
      probability_relevant,
      fitted$thresholds
    ),
    screening_reason = dplyr::case_when(
      screening_decision == "automatic_retain" ~
        "Probability met the validated automatic-retain threshold.",
      screening_decision == "automatic_exclude" ~
        "Probability fell below the validated high-sensitivity exclusion threshold.",
      TRUE ~
        "Probability fell within the review interval."
    )
  )

automatic_retain <- records_to_score |>
  dplyr::filter(
    screening_decision == "automatic_retain"
  )

automatic_exclude <- records_to_score |>
  dplyr::filter(
    screening_decision == "automatic_exclude"
  )

review <- records_to_score |>
  dplyr::filter(
    screening_decision == "review"
  ) |>
  dplyr::transmute(
    source_file,
    record_sequence,
    record_id,
    title,
    abstract,
    authors,
    year,
    doi,
    probability_relevant,
    model_decision = screening_decision,
    human_decision = NA_character_,
    human_notes = NA_character_
  )

audit <- dplyr::bind_rows(
  records_to_score |>
    dplyr::mutate(
      pipeline_status = screening_decision
    ),
  historical_duplicates |>
    dplyr::mutate(
      pipeline_status = duplicate_status
    ),
  duplicate_review |>
    dplyr::mutate(
      pipeline_status = duplicate_status
    ),
  within_batch_duplicates |>
    dplyr::mutate(
      pipeline_status = "duplicate"
    )
)

readr::write_csv(
  automatic_retain,
  fs::path(
    output_dir,
    "automatic_retain.csv"
  ),
  na = ""
)

readr::write_csv(
  automatic_exclude,
  fs::path(
    output_dir,
    "automatic_exclude.csv"
  ),
  na = ""
)

readr::write_csv(
  audit,
  fs::path(
    output_dir,
    "screening_audit.csv"
  ),
  na = ""
)

readr::write_csv(
  historical_duplicates,
  fs::path(
    output_dir,
    "definitive_duplicates.csv"
  ),
  na = ""
)

readr::write_csv(
  duplicate_review,
  fs::path(
    output_dir,
    "possible_duplicates_for_review.csv"
  ),
  na = ""
)

wb <- openxlsx2::wb_workbook()
wb$add_worksheet("Review")
wb$add_data("Review", review)
wb$freeze_pane("Review", first_row = TRUE)
wb$save(
  fs::path(
    output_dir,
    "uncertain_for_review.xlsx"
  ),
  overwrite = TRUE
)

summary <- tibble::tibble(
  item = c(
    "Incoming RIS files",
    "Incoming records",
    "Definitive historical duplicates",
    "Possible duplicates or DOI conflicts for review",
    "Within-batch exact-title duplicates",
    "Automatic retain",
    "Automatic exclude",
    "Review"
  ),
  value = c(
    length(ris_files),
    nrow(incoming),
    nrow(historical_duplicates),
    nrow(duplicate_review),
    nrow(within_batch_duplicates),
    nrow(automatic_retain),
    nrow(automatic_exclude),
    nrow(review)
  )
)

readr::write_csv(
  summary,
  fs::path(
    output_dir,
    "screening_summary.csv"
  ),
  na = ""
)

message("")
message("Living-map relevance screening completed.")
print(summary)
message("")
message("Review workbook: ",
        fs::path(output_dir, "uncertain_for_review.xlsx"))
