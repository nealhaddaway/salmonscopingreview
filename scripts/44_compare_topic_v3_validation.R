# =============================================================================
# File: scripts/44_compare_topic_v3_validation.R
# Purpose: Compare blind LLM topic coding with the 50-record human gold standard.
# =============================================================================

source("scripts/00_setup.R")

gold_file <- here::here(
  "data_raw",
  "topic_validation_gold_standard_50_long.csv"
)

record_file <- here::here(
  "data_raw",
  "topic_validation_gold_standard_50.csv"
)

llm_long_file <- here::here(
  "outputs",
  "stage_4_llm",
  "ontology_v3_validation",
  "topic_v3_validation_llm_long.csv"
)

llm_record_file <- here::here(
  "outputs",
  "stage_4_llm",
  "ontology_v3_validation",
  "topic_v3_validation_llm_record.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_4_llm",
  "ontology_v3_validation",
  "comparison"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(gold_file),
  file.exists(record_file),
  file.exists(llm_long_file),
  file.exists(llm_record_file)
)

gold <- readr::read_csv(
  gold_file,
  show_col_types = FALSE,
  col_types = readr::cols(
    record_id = readr::col_character(),
    gold_path = readr::col_character()
  )
) |>
  dplyr::distinct(
    record_id,
    gold_path
  )

records <- readr::read_csv(
  record_file,
  show_col_types = FALSE
) |>
  dplyr::mutate(
    record_id = as.character(record_id)
  )

llm_long <- readr::read_csv(
  llm_long_file,
  show_col_types = FALSE
) |>
  dplyr::mutate(
    record_id = as.character(record_id)
  ) |>
  dplyr::filter(
    !is.na(hierarchy_path),
    nzchar(hierarchy_path)
  ) |>
  dplyr::distinct(
    record_id,
    hierarchy_path,
    .keep_all = TRUE
  )

llm_record <- readr::read_csv(
  llm_record_file,
  show_col_types = FALSE
) |>
  dplyr::mutate(
    record_id = as.character(record_id)
  )

# -----------------------------------------------------------------------------
# Record-level set comparison
# -----------------------------------------------------------------------------

gold_sets <- gold |>
  dplyr::group_by(
    record_id
  ) |>
  dplyr::summarise(
    gold_codes = list(
      sort(
        unique(
          gold_path
        )
      )
    ),
    .groups = "drop"
  )

llm_sets <- llm_long |>
  dplyr::group_by(
    record_id
  ) |>
  dplyr::summarise(
    llm_codes = list(
      sort(
        unique(
          hierarchy_path
        )
      )
    ),
    .groups = "drop"
  )

comparison <- records |>
  dplyr::select(
    sampling_stratum,
    record_sequence,
    record_id,
    title,
    abstract
  ) |>
  dplyr::left_join(
    gold_sets,
    by = "record_id"
  ) |>
  dplyr::left_join(
    llm_sets,
    by = "record_id"
  ) |>
  dplyr::left_join(
    llm_record |>
      dplyr::select(
        record_id,
        review_required,
        review_reason,
        classification_failed,
        classification_error
      ),
    by = "record_id"
  ) |>
  dplyr::mutate(
    gold_codes = purrr::map(
      gold_codes,
      ~ if (
        is.null(.x)
      ) {
        character()
      } else {
        .x
      }
    ),
    llm_codes = purrr::map(
      llm_codes,
      ~ if (
        is.null(.x)
      ) {
        character()
      } else {
        .x
      }
    ),
    true_positive_codes = purrr::map2(
      gold_codes,
      llm_codes,
      intersect
    ),
    missing_codes = purrr::map2(
      gold_codes,
      llm_codes,
      setdiff
    ),
    extra_codes = purrr::map2(
      llm_codes,
      gold_codes,
      setdiff
    ),
    true_positives = purrr::map_int(
      true_positive_codes,
      length
    ),
    false_negatives = purrr::map_int(
      missing_codes,
      length
    ),
    false_positives = purrr::map_int(
      extra_codes,
      length
    ),
    exact_match = purrr::map2_lgl(
      gold_codes,
      llm_codes,
      identical
    ),
    precision = dplyr::if_else(
      true_positives + false_positives == 0L,
      dplyr::if_else(
        true_positives + false_negatives == 0L,
        1,
        0
      ),
      true_positives /
        (
          true_positives +
            false_positives
        )
    ),
    recall = dplyr::if_else(
      true_positives + false_negatives == 0L,
      1,
      true_positives /
        (
          true_positives +
            false_negatives
        )
    ),
    f1 = dplyr::if_else(
      precision + recall == 0,
      0,
      2 *
        precision *
        recall /
        (
          precision +
            recall
        )
    ),
    gold_paths = purrr::map_chr(
      gold_codes,
      ~ paste(
        .x,
        collapse = "; "
      )
    ),
    llm_paths = purrr::map_chr(
      llm_codes,
      ~ paste(
        .x,
        collapse = "; "
      )
    ),
    missing_paths = purrr::map_chr(
      missing_codes,
      ~ paste(
        .x,
        collapse = "; "
      )
    ),
    extra_paths = purrr::map_chr(
      extra_codes,
      ~ paste(
        .x,
        collapse = "; "
      )
    )
  )

# -----------------------------------------------------------------------------
# Overall metrics
# -----------------------------------------------------------------------------

total_tp <- sum(
  comparison$true_positives
)

total_fp <- sum(
  comparison$false_positives
)

total_fn <- sum(
  comparison$false_negatives
)

micro_precision <- if (
  total_tp + total_fp == 0
) {
  NA_real_
} else {
  total_tp /
    (
      total_tp +
        total_fp
    )
}

micro_recall <- if (
  total_tp + total_fn == 0
) {
  NA_real_
} else {
  total_tp /
    (
      total_tp +
        total_fn
    )
}

micro_f1 <- if (
  is.na(micro_precision) ||
    is.na(micro_recall) ||
    micro_precision + micro_recall == 0
) {
  NA_real_
} else {
  2 *
    micro_precision *
    micro_recall /
    (
      micro_precision +
        micro_recall
    )
}

overall_metrics <- tibble::tibble(
  metric = c(
    "Records",
    "Exact matches",
    "Exact-match proportion",
    "Mean record precision",
    "Mean record recall",
    "Mean record F1",
    "Micro precision",
    "Micro recall",
    "Micro F1",
    "Total missing codes",
    "Total extra codes",
    "Classification failures",
    "Review required"
  ),
  value = c(
    nrow(comparison),
    sum(comparison$exact_match),
    mean(comparison$exact_match),
    mean(comparison$precision),
    mean(comparison$recall),
    mean(comparison$f1),
    micro_precision,
    micro_recall,
    micro_f1,
    total_fn,
    total_fp,
    sum(
      comparison$classification_failed,
      na.rm = TRUE
    ),
    sum(
      comparison$review_required,
      na.rm = TRUE
    )
  )
)

# -----------------------------------------------------------------------------
# Per-code metrics
# -----------------------------------------------------------------------------

all_codes <- sort(
  unique(
    c(
      gold$gold_path,
      llm_long$hierarchy_path
    )
  )
)

per_code <- purrr::map_dfr(
  all_codes,
  function(code) {

    gold_records <- unique(
      gold$record_id[
        gold$gold_path == code
      ]
    )

    llm_records <- unique(
      llm_long$record_id[
        llm_long$hierarchy_path == code
      ]
    )

    tp <- length(
      intersect(
        gold_records,
        llm_records
      )
    )

    fp <- length(
      setdiff(
        llm_records,
        gold_records
      )
    )

    fn <- length(
      setdiff(
        gold_records,
        llm_records
      )
    )

    precision <- if (
      tp + fp == 0
    ) {
      NA_real_
    } else {
      tp /
        (
          tp +
            fp
        )
    }

    recall <- if (
      tp + fn == 0
    ) {
      NA_real_
    } else {
      tp /
        (
          tp +
            fn
        )
    }

    f1 <- if (
      is.na(precision) ||
        is.na(recall) ||
        precision + recall == 0
    ) {
      NA_real_
    } else {
      2 *
        precision *
        recall /
        (
          precision +
            recall
        )
    }

    tibble::tibble(
      hierarchy_path = code,
      gold_records = length(
        gold_records
      ),
      llm_records = length(
        llm_records
      ),
      true_positives = tp,
      false_positives = fp,
      false_negatives = fn,
      precision = precision,
      recall = recall,
      f1 = f1
    )
  }
) |>
  dplyr::arrange(
    dplyr::desc(
      gold_records
    ),
    hierarchy_path
  )

# -----------------------------------------------------------------------------
# Long disagreement table
# -----------------------------------------------------------------------------

missing_long <- comparison |>
  dplyr::select(
    sampling_stratum,
    record_sequence,
    record_id,
    title,
    abstract,
    missing_codes
  ) |>
  tidyr::unnest_longer(
    missing_codes,
    values_to = "hierarchy_path"
  ) |>
  dplyr::filter(
    !is.na(hierarchy_path),
    nzchar(hierarchy_path)
  ) |>
  dplyr::mutate(
    disagreement_type = "Missing from LLM"
  )

extra_long <- comparison |>
  dplyr::select(
    sampling_stratum,
    record_sequence,
    record_id,
    title,
    abstract,
    extra_codes
  ) |>
  tidyr::unnest_longer(
    extra_codes,
    values_to = "hierarchy_path"
  ) |>
  dplyr::filter(
    !is.na(hierarchy_path),
    nzchar(hierarchy_path)
  ) |>
  dplyr::mutate(
    disagreement_type = "Extra from LLM"
  )

disagreements <- dplyr::bind_rows(
  missing_long,
  extra_long
) |>
  dplyr::arrange(
    record_sequence,
    disagreement_type,
    hierarchy_path
  ) |>
  dplyr::mutate(
    reviewer_resolution = NA_character_,
    disagreement_cause = NA_character_,
    resolution_notes = NA_character_
  )

comparison_flat <- comparison |>
  dplyr::select(
    sampling_stratum,
    record_sequence,
    record_id,
    title,
    abstract,
    gold_paths,
    llm_paths,
    missing_paths,
    extra_paths,
    exact_match,
    precision,
    recall,
    f1,
    review_required,
    review_reason,
    classification_failed,
    classification_error
  ) |>
  dplyr::arrange(
    record_sequence
  )

# -----------------------------------------------------------------------------
# Write files
# -----------------------------------------------------------------------------

readr::write_csv(
  overall_metrics,
  fs::path(
    output_dir,
    "topic_v3_validation_overall_metrics.csv"
  ),
  na = ""
)

readr::write_csv(
  comparison_flat,
  fs::path(
    output_dir,
    "topic_v3_validation_record_comparison.csv"
  ),
  na = ""
)

readr::write_csv(
  per_code,
  fs::path(
    output_dir,
    "topic_v3_validation_per_code_metrics.csv"
  ),
  na = ""
)

readr::write_csv(
  disagreements,
  fs::path(
    output_dir,
    "topic_v3_validation_disagreements.csv"
  ),
  na = ""
)

# Human-readable workbook.
xlsx_file <- fs::path(
  output_dir,
  "topic_v3_validation_comparison.xlsx"
)

wb <- openxlsx2::wb_workbook()

wb$add_worksheet(
  "Overall metrics"
)
wb$add_data(
  "Overall metrics",
  overall_metrics
)

wb$add_worksheet(
  "Record comparison"
)
wb$add_data(
  "Record comparison",
  comparison_flat
)
wb$freeze_pane(
  "Record comparison",
  first_row = TRUE
)

wb$add_worksheet(
  "Disagreements"
)
wb$add_data(
  "Disagreements",
  disagreements
)
wb$freeze_pane(
  "Disagreements",
  first_row = TRUE
)

wb$add_worksheet(
  "Per-code metrics"
)
wb$add_data(
  "Per-code metrics",
  per_code
)
wb$freeze_pane(
  "Per-code metrics",
  first_row = TRUE
)

wb$save(
  xlsx_file,
  overwrite = TRUE
)

message("")
message("Topic ontology v3 comparison completed.")
message("")
print(overall_metrics)
message("")
message("Comparison workbook: ", xlsx_file)
