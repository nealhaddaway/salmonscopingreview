# =============================================================================
# File: scripts/51_compare_topic_v4_validation.R
# Purpose: Compare the final V4 classifier against the adjudicated 50-record
#          standard using the record-level V4 output.
# =============================================================================

source("scripts/00_setup.R")

gold_file <- here::here(
  "data_raw",
  "topic_validation_adjudicated_50.csv"
)

v4_record_file <- here::here(
  "outputs",
  "stage_4_llm",
  "ontology_v4_validation",
  "topic_v4_llm_record.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_4_llm",
  "ontology_v4_validation",
  "comparison"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(gold_file),
  file.exists(v4_record_file)
)

split_paths <- function(x) {
  if (
    is.na(x) ||
    !nzchar(trimws(x))
  ) {
    return(character())
  }

  values <- trimws(
    unlist(
      strsplit(
        x,
        ";",
        fixed = TRUE
      )
    )
  )

  sort(
    unique(
      values[
        nzchar(values)
      ]
    )
  )
}

gold <- readr::read_csv(
  gold_file,
  show_col_types = FALSE
) |>
  dplyr::mutate(
    record_id = as.character(record_id),
    gold_codes = purrr::map(
      final_paths,
      split_paths
    )
  )

v4 <- readr::read_csv(
  v4_record_file,
  show_col_types = FALSE
) |>
  dplyr::mutate(
    record_id = as.character(record_id),
    v4_codes = purrr::map(
      assigned_paths,
      split_paths
    )
  )

comparison <- gold |>
  dplyr::select(
    sampling_stratum,
    record_sequence,
    record_id,
    title,
    abstract,
    final_paths,
    adjudication,
    adjudication_reason,
    adjudication_action,
    gold_codes
  ) |>
  dplyr::left_join(
    v4 |>
      dplyr::select(
        record_id,
        assigned_paths,
        assignment_count,
        review_required,
        review_reason,
        classification_failed,
        classification_error,
        v4_codes
      ),
    by = "record_id"
  ) |>
  dplyr::mutate(
    v4_codes = purrr::map(
      v4_codes,
      ~ if (is.null(.x)) character() else .x
    ),
    true_positive_codes = purrr::map2(
      gold_codes,
      v4_codes,
      intersect
    ),
    missing_codes = purrr::map2(
      gold_codes,
      v4_codes,
      setdiff
    ),
    extra_codes = purrr::map2(
      v4_codes,
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
      v4_codes,
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
    v4_paths = purrr::map_chr(
      v4_codes,
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
    sum(
      comparison$exact_match
    ),
    mean(
      comparison$exact_match
    ),
    mean(
      comparison$precision
    ),
    mean(
      comparison$recall
    ),
    mean(
      comparison$f1
    ),
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

all_codes <- sort(
  unique(
    c(
      unlist(
        comparison$gold_codes
      ),
      unlist(
        comparison$v4_codes
      )
    )
  )
)

per_code <- purrr::map_dfr(
  all_codes,
  function(code) {

    gold_records <- comparison$record_id[
      purrr::map_lgl(
        comparison$gold_codes,
        ~ code %in% .x
      )
    ]

    v4_records <- comparison$record_id[
      purrr::map_lgl(
        comparison$v4_codes,
        ~ code %in% .x
      )
    ]

    tp <- length(
      intersect(
        gold_records,
        v4_records
      )
    )

    fp <- length(
      setdiff(
        v4_records,
        gold_records
      )
    )

    fn <- length(
      setdiff(
        gold_records,
        v4_records
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
      v4_records = length(
        v4_records
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
    disagreement_type = "Missing from V4"
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
    disagreement_type = "Extra from V4"
  )

disagreements <- dplyr::bind_rows(
  missing_long,
  extra_long
) |>
  dplyr::arrange(
    record_sequence,
    disagreement_type,
    hierarchy_path
  )

record_comparison <- comparison |>
  dplyr::select(
    sampling_stratum,
    record_sequence,
    record_id,
    title,
    abstract,
    gold_paths,
    v4_paths,
    missing_paths,
    extra_paths,
    exact_match,
    precision,
    recall,
    f1,
    adjudication,
    adjudication_reason,
    adjudication_action,
    review_required,
    review_reason,
    classification_failed,
    classification_error
  ) |>
  dplyr::arrange(
    record_sequence
  )

readr::write_csv(
  overall_metrics,
  fs::path(
    output_dir,
    "topic_v4_overall_metrics.csv"
  ),
  na = ""
)

readr::write_csv(
  record_comparison,
  fs::path(
    output_dir,
    "topic_v4_record_comparison.csv"
  ),
  na = ""
)

readr::write_csv(
  disagreements,
  fs::path(
    output_dir,
    "topic_v4_disagreements.csv"
  ),
  na = ""
)

readr::write_csv(
  per_code,
  fs::path(
    output_dir,
    "topic_v4_per_code_metrics.csv"
  ),
  na = ""
)

xlsx_file <- fs::path(
  output_dir,
  "topic_v4_validation_comparison.xlsx"
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
  record_comparison
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
message("V4 comparison completed.")
message("")
print(
  overall_metrics
)
message("")
message(
  "Comparison workbook: ",
  xlsx_file
)
