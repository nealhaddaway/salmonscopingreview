# =============================================================================
# File: scripts/46_compare_topic_v3_round2.R
# Purpose: Compare validation round 2 against the adjudicated 50-record
#          standard and compare round-2 metrics with round 1.
# =============================================================================

source("scripts/00_setup.R")

gold_long_file <- here::here(
  "data_raw",
  "topic_validation_adjudicated_50_long.csv"
)

gold_record_file <- here::here(
  "data_raw",
  "topic_validation_adjudicated_50.csv"
)

round2_long_file <- here::here(
  "outputs",
  "stage_4_llm",
  "ontology_v3_validation_round2",
  "topic_v3_round2_llm_long.csv"
)

round2_record_file <- here::here(
  "outputs",
  "stage_4_llm",
  "ontology_v3_validation_round2",
  "topic_v3_round2_llm_record.csv"
)

round1_record_file <- here::here(
  "outputs",
  "stage_4_llm",
  "ontology_v3_validation",
  "topic_v3_validation_llm_record.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_4_llm",
  "ontology_v3_validation_round2",
  "comparison"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(gold_long_file),
  file.exists(gold_record_file),
  file.exists(round2_long_file),
  file.exists(round2_record_file)
)

gold <- readr::read_csv(
  gold_long_file,
  show_col_types = FALSE,
  col_types = readr::cols(
    record_id = readr::col_character(),
    gold_path = readr::col_character()
  )
) |>
  dplyr::distinct(record_id, gold_path)

records <- readr::read_csv(
  gold_record_file,
  show_col_types = FALSE
) |>
  dplyr::mutate(record_id = as.character(record_id))

round2_long <- readr::read_csv(
  round2_long_file,
  show_col_types = FALSE
) |>
  dplyr::mutate(record_id = as.character(record_id)) |>
  dplyr::filter(
    !is.na(hierarchy_path),
    nzchar(hierarchy_path)
  ) |>
  dplyr::distinct(
    record_id,
    hierarchy_path,
    .keep_all = TRUE
  )

round2_record <- readr::read_csv(
  round2_record_file,
  show_col_types = FALSE
) |>
  dplyr::mutate(record_id = as.character(record_id))

round1_record <- if (file.exists(round1_record_file)) {
  readr::read_csv(
    round1_record_file,
    show_col_types = FALSE
  ) |>
    dplyr::mutate(record_id = as.character(record_id)) |>
    dplyr::select(
      record_id,
      round1_paths = assigned_paths
    )
} else {
  tibble::tibble(
    record_id = character(),
    round1_paths = character()
  )
}

split_set <- function(x) {
  if (is.na(x) || !nzchar(x)) {
    return(character())
  }
  sort(unique(trimws(unlist(strsplit(x, ";", fixed = TRUE)))))
}

gold_sets <- gold |>
  dplyr::group_by(record_id) |>
  dplyr::summarise(
    gold_codes = list(sort(unique(gold_path))),
    .groups = "drop"
  )

round2_sets <- round2_long |>
  dplyr::group_by(record_id) |>
  dplyr::summarise(
    round2_codes = list(sort(unique(hierarchy_path))),
    .groups = "drop"
  )

comparison <- records |>
  dplyr::select(
    sampling_stratum,
    record_sequence,
    record_id,
    title,
    abstract,
    final_paths,
    adjudication,
    adjudication_reason,
    adjudication_action
  ) |>
  dplyr::left_join(gold_sets, by = "record_id") |>
  dplyr::left_join(round2_sets, by = "record_id") |>
  dplyr::left_join(round1_record, by = "record_id") |>
  dplyr::left_join(
    round2_record |>
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
      ~ if (is.null(.x)) character() else .x
    ),
    round2_codes = purrr::map(
      round2_codes,
      ~ if (is.null(.x)) character() else .x
    ),
    true_positive_codes = purrr::map2(
      gold_codes,
      round2_codes,
      intersect
    ),
    missing_codes = purrr::map2(
      gold_codes,
      round2_codes,
      setdiff
    ),
    extra_codes = purrr::map2(
      round2_codes,
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
      round2_codes,
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
        (true_positives + false_positives)
    ),
    recall = dplyr::if_else(
      true_positives + false_negatives == 0L,
      1,
      true_positives /
        (true_positives + false_negatives)
    ),
    f1 = dplyr::if_else(
      precision + recall == 0,
      0,
      2 * precision * recall / (precision + recall)
    ),
    round2_paths = purrr::map_chr(
      round2_codes,
      ~ paste(.x, collapse = "; ")
    ),
    missing_paths = purrr::map_chr(
      missing_codes,
      ~ paste(.x, collapse = "; ")
    ),
    extra_paths = purrr::map_chr(
      extra_codes,
      ~ paste(.x, collapse = "; ")
    )
  )

total_tp <- sum(comparison$true_positives)
total_fp <- sum(comparison$false_positives)
total_fn <- sum(comparison$false_negatives)

micro_precision <- if (total_tp + total_fp == 0) {
  NA_real_
} else {
  total_tp / (total_tp + total_fp)
}

micro_recall <- if (total_tp + total_fn == 0) {
  NA_real_
} else {
  total_tp / (total_tp + total_fn)
}

micro_f1 <- if (
  is.na(micro_precision) ||
    is.na(micro_recall) ||
    micro_precision + micro_recall == 0
) {
  NA_real_
} else {
  2 * micro_precision * micro_recall /
    (micro_precision + micro_recall)
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
    sum(comparison$classification_failed, na.rm = TRUE),
    sum(comparison$review_required, na.rm = TRUE)
  )
)

all_codes <- sort(unique(c(
  gold$gold_path,
  round2_long$hierarchy_path
)))

per_code <- purrr::map_dfr(
  all_codes,
  function(code) {
    gold_records <- unique(
      gold$record_id[gold$gold_path == code]
    )
    llm_records <- unique(
      round2_long$record_id[
        round2_long$hierarchy_path == code
      ]
    )

    tp <- length(intersect(gold_records, llm_records))
    fp <- length(setdiff(llm_records, gold_records))
    fn <- length(setdiff(gold_records, llm_records))

    precision <- if (tp + fp == 0) NA_real_ else tp / (tp + fp)
    recall <- if (tp + fn == 0) NA_real_ else tp / (tp + fn)
    f1 <- if (
      is.na(precision) ||
        is.na(recall) ||
        precision + recall == 0
    ) {
      NA_real_
    } else {
      2 * precision * recall / (precision + recall)
    }

    tibble::tibble(
      hierarchy_path = code,
      gold_records = length(gold_records),
      round2_records = length(llm_records),
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
    dplyr::desc(gold_records),
    hierarchy_path
  )

comparison_flat <- comparison |>
  dplyr::select(
    sampling_stratum,
    record_sequence,
    record_id,
    title,
    abstract,
    final_paths,
    round1_paths,
    round2_paths,
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
  dplyr::arrange(record_sequence)

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
    disagreement_type = "Missing from round 2"
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
    disagreement_type = "Extra from round 2"
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

readr::write_csv(
  overall_metrics,
  fs::path(output_dir, "topic_v3_round2_overall_metrics.csv"),
  na = ""
)

readr::write_csv(
  comparison_flat,
  fs::path(output_dir, "topic_v3_round2_record_comparison.csv"),
  na = ""
)

readr::write_csv(
  per_code,
  fs::path(output_dir, "topic_v3_round2_per_code_metrics.csv"),
  na = ""
)

readr::write_csv(
  disagreements,
  fs::path(output_dir, "topic_v3_round2_disagreements.csv"),
  na = ""
)

xlsx_file <- fs::path(
  output_dir,
  "topic_v3_round2_comparison.xlsx"
)

wb <- openxlsx2::wb_workbook()

wb$add_worksheet("Overall metrics")
wb$add_data("Overall metrics", overall_metrics)

wb$add_worksheet("Record comparison")
wb$add_data("Record comparison", comparison_flat)
wb$freeze_pane("Record comparison", first_row = TRUE)

wb$add_worksheet("Disagreements")
wb$add_data("Disagreements", disagreements)
wb$freeze_pane("Disagreements", first_row = TRUE)

wb$add_worksheet("Per-code metrics")
wb$add_data("Per-code metrics", per_code)
wb$freeze_pane("Per-code metrics", first_row = TRUE)

wb$save(xlsx_file, overwrite = TRUE)

message("")
message("Round-2 comparison completed.")
message("")
print(overall_metrics)
message("")
message("Comparison workbook: ", xlsx_file)
