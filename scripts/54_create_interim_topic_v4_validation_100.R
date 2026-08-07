# =============================================================================
# File: scripts/54_create_interim_topic_v4_validation_100.R
# Purpose: Create a targeted 100-record validation workbook from the records
#          already completed by the paused V4 full-corpus run.
#
# Sampling:
#   - 25 records with six or more assigned codes
#   - 25 records containing a General / Multiple-or-general pathway
#   - 50 otherwise random completed records
#
# Records are unique across the three strata. The human-coding sheet does not
# show the LLM assignments, allowing blind validation.
# =============================================================================

source("scripts/00_setup.R")

record_file <- here::here(
  "outputs",
  "stage_4_llm",
  "ontology_v4_full_corpus",
  "topic_v4_full_corpus_record.csv"
)

long_file <- here::here(
  "outputs",
  "stage_4_llm",
  "ontology_v4_full_corpus",
  "topic_v4_full_corpus_long.csv"
)

ontology_file <- here::here(
  "data_raw",
  "topic_ontology_v3.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_4_llm",
  "ontology_v4_full_corpus",
  "interim_validation"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(record_file),
  file.exists(long_file),
  file.exists(ontology_file)
)

records <- readr::read_csv(
  record_file,
  show_col_types = FALSE,
  col_types = readr::cols(
    record_id = readr::col_character()
  )
) |>
  dplyr::filter(
    !classification_failed
  ) |>
  dplyr::mutate(
    assignment_count = dplyr::coalesce(
      as.integer(assignment_count),
      0L
    )
  )

assignments <- readr::read_csv(
  long_file,
  show_col_types = FALSE,
  col_types = readr::cols(
    record_id = readr::col_character()
  )
)

ontology <- readr::read_csv(
  ontology_file,
  show_col_types = FALSE
) |>
  dplyr::select(
    path_id,
    hierarchy_path
  ) |>
  dplyr::distinct() |>
  dplyr::arrange(
    hierarchy_path
  )

if (nrow(records) < 100L) {
  stop(
    "Only ",
    nrow(records),
    " successfully classified records are available. ",
    "At least 100 are needed."
  )
}

assignment_summary <- assignments |>
  dplyr::group_by(
    record_id
  ) |>
  dplyr::summarise(
    llm_paths = paste(
      sort(
        unique(
          hierarchy_path
        )
      ),
      collapse = "; "
    ),
    llm_reasons = paste(
      paste0(
        hierarchy_path,
        ": ",
        reason
      ),
      collapse = " | "
    ),
    has_general_pathway = any(
      stringr::str_detect(
        hierarchy_path,
        stringr::regex(
          "General|Multiple or general",
          ignore_case = TRUE
        )
      )
    ),
    .groups = "drop"
  )

eligible <- records |>
  dplyr::left_join(
    assignment_summary,
    by = "record_id"
  ) |>
  dplyr::mutate(
    llm_paths = dplyr::coalesce(
      llm_paths,
      ""
    ),
    llm_reasons = dplyr::coalesce(
      llm_reasons,
      ""
    ),
    has_general_pathway = dplyr::coalesce(
      has_general_pathway,
      FALSE
    )
  )

set.seed(20260806)

# -----------------------------------------------------------------------------
# Stratum 1: high code count
# -----------------------------------------------------------------------------

high_code_pool <- eligible |>
  dplyr::filter(
    assignment_count >= 6L
  )

n_high <- min(
  25L,
  nrow(high_code_pool)
)

high_code_sample <- if (n_high > 0L) {
  high_code_pool |>
    dplyr::slice_sample(
      n = n_high
    ) |>
    dplyr::mutate(
      validation_stratum = "High code count (6+)"
    )
} else {
  high_code_pool |>
    dplyr::mutate(
      validation_stratum = character()
    )
}

selected_ids <- high_code_sample$record_id

# -----------------------------------------------------------------------------
# Stratum 2: General / Multiple-or-general pathways, excluding stratum 1
# -----------------------------------------------------------------------------

general_pool <- eligible |>
  dplyr::filter(
    has_general_pathway,
    !record_id %in% selected_ids
  )

n_general <- min(
  25L,
  nrow(general_pool)
)

general_sample <- if (n_general > 0L) {
  general_pool |>
    dplyr::slice_sample(
      n = n_general
    ) |>
    dplyr::mutate(
      validation_stratum = "General pathway"
    )
} else {
  general_pool |>
    dplyr::mutate(
      validation_stratum = character()
    )
}

selected_ids <- c(
  selected_ids,
  general_sample$record_id
)

# -----------------------------------------------------------------------------
# Stratum 3: random completed records, excluding strata 1 and 2
# -----------------------------------------------------------------------------

random_pool <- eligible |>
  dplyr::filter(
    !record_id %in% selected_ids
  )

n_random_target <- 100L -
  nrow(high_code_sample) -
  nrow(general_sample)

n_random <- min(
  n_random_target,
  nrow(random_pool)
)

random_sample <- random_pool |>
  dplyr::slice_sample(
    n = n_random
  ) |>
  dplyr::mutate(
    validation_stratum = "Random"
  )

sample_100 <- dplyr::bind_rows(
  high_code_sample,
  general_sample,
  random_sample
) |>
  dplyr::arrange(
    validation_stratum,
    record_sequence
  )

if (nrow(sample_100) != 100L) {
  stop(
    "Sampling produced ",
    nrow(sample_100),
    " records rather than 100."
  )
}

if (anyDuplicated(sample_100$record_id) > 0L) {
  stop("Duplicate records were selected across validation strata.")
}

# Randomise row order so stratum membership does not create long blocks.
set.seed(20260806)
sample_100 <- sample_100 |>
  dplyr::slice_sample(
    prop = 1
  ) |>
  dplyr::mutate(
    validation_row = dplyr::row_number()
  ) |>
  dplyr::arrange(
    validation_row
  )

human_validation <- sample_100 |>
  dplyr::transmute(
    validation_row,
    validation_stratum,
    record_sequence,
    record_id,
    title,
    abstract,
    human_code_1 = NA_character_,
    human_code_2 = NA_character_,
    human_code_3 = NA_character_,
    human_code_4 = NA_character_,
    human_code_5 = NA_character_,
    human_code_6 = NA_character_,
    human_notes = NA_character_
  )

llm_reference <- sample_100 |>
  dplyr::transmute(
    validation_row,
    validation_stratum,
    record_sequence,
    record_id,
    title,
    llm_paths,
    llm_reasons,
    assignment_count,
    review_required,
    review_reason
  )

sampling_summary <- tibble::tibble(
  item = c(
    "Successfully classified records available",
    "Highest completed record sequence",
    "High-code records selected",
    "General-pathway records selected",
    "Random records selected",
    "Total validation records",
    "Sampling seed"
  ),
  value = c(
    nrow(eligible),
    max(eligible$record_sequence, na.rm = TRUE),
    nrow(high_code_sample),
    nrow(general_sample),
    nrow(random_sample),
    nrow(sample_100),
    20260806
  )
)

csv_file <- fs::path(
  output_dir,
  "topic_v4_interim_validation_100.csv"
)

xlsx_file <- fs::path(
  output_dir,
  "topic_v4_interim_validation_100.xlsx"
)

readr::write_csv(
  human_validation,
  csv_file,
  na = ""
)

wb <- openxlsx2::wb_workbook()

wb$add_worksheet(
  "Human validation 100"
)
wb$add_data(
  "Human validation 100",
  human_validation
)
wb$freeze_pane(
  "Human validation 100",
  first_row = TRUE
)

wb$add_worksheet(
  "LLM reference"
)
wb$add_data(
  "LLM reference",
  llm_reference
)
wb$freeze_pane(
  "LLM reference",
  first_row = TRUE
)

wb$add_worksheet(
  "Ontology list"
)
wb$add_data(
  "Ontology list",
  ontology
)
wb$freeze_pane(
  "Ontology list",
  first_row = TRUE
)

wb$add_worksheet(
  "Sampling summary"
)
wb$add_data(
  "Sampling summary",
  sampling_summary
)

# Add dropdowns to the six human-code columns. If the installed openxlsx2
# version does not support this method, the workbook is still saved normally.
ontology_end_row <- nrow(ontology) + 1L

for (column in LETTERS[7:12]) {
  try(
    wb$add_data_validation(
      sheet = "Human validation 100",
      dims = paste0(
        column,
        "2:",
        column,
        "101"
      ),
      type = "list",
      value = paste0(
        "'Ontology list'!$B$2:$B$",
        ontology_end_row
      )
    ),
    silent = TRUE
  )
}

wb$save(
  xlsx_file,
  overwrite = TRUE
)

message("")
message("Interim V4 validation workbook created.")
message("Completed records available: ", nrow(eligible))
message("Highest completed sequence: ", max(eligible$record_sequence, na.rm = TRUE))
message("High-code records: ", nrow(high_code_sample))
message("General-pathway records: ", nrow(general_sample))
message("Random records: ", nrow(random_sample))
message("Workbook: ", xlsx_file)
