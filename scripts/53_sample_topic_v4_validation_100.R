# =============================================================================
# File: scripts/53_sample_topic_v4_validation_100.R
# Purpose: Draw a reproducible fresh 100-record sample from the completed
#          full-corpus V4 output for independent human validation.
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

output_dir <- here::here(
  "outputs",
  "stage_4_llm",
  "ontology_v4_full_corpus",
  "validation"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(record_file),
  file.exists(long_file)
)

records <- readr::read_csv(
  record_file,
  show_col_types = FALSE,
  col_types = readr::cols(
    record_id = readr::col_character()
  )
)

assignments <- readr::read_csv(
  long_file,
  show_col_types = FALSE,
  col_types = readr::cols(
    record_id = readr::col_character()
  )
) |>
  dplyr::group_by(record_id) |>
  dplyr::summarise(
    llm_paths = paste(
      sort(unique(hierarchy_path)),
      collapse = "; "
    ),
    llm_reasons = paste(
      paste0(hierarchy_path, ": ", reason),
      collapse = " | "
    ),
    .groups = "drop"
  )

eligible <- records |>
  dplyr::filter(
    !classification_failed
  ) |>
  dplyr::left_join(
    assignments,
    by = "record_id"
  )

set.seed(20260806)

sample_100 <- eligible |>
  dplyr::filter(
    record_sequence <= 401
  ) |>
  dplyr::slice_sample(
    n = min(
      100L,
      n()
    )
  ) |>
  dplyr::arrange(record_sequence) |>
  dplyr::transmute(
    record_sequence,
    record_id,
    title,
    abstract,
    llm_paths,
    llm_reasons,
    review_required,
    review_reason,
    human_code_1 = NA_character_,
    human_code_2 = NA_character_,
    human_code_3 = NA_character_,
    human_code_4 = NA_character_,
    human_code_5 = NA_character_,
    human_code_6 = NA_character_,
    human_notes = NA_character_
  )

csv_file <- fs::path(
  output_dir,
  "topic_v4_fresh_validation_100.csv"
)

xlsx_file <- fs::path(
  output_dir,
  "topic_v4_fresh_validation_100.xlsx"
)

readr::write_csv(
  sample_100,
  csv_file,
  na = ""
)

wb <- openxlsx2::wb_workbook()
wb$add_worksheet("Validation 100")
wb$add_data("Validation 100", sample_100)
wb$freeze_pane("Validation 100", first_row = TRUE)
wb$save(xlsx_file, overwrite = TRUE)

message("")
message("Fresh V4 validation sample created.")
message("Records: ", nrow(sample_100))
message("Workbook: ", xlsx_file)
