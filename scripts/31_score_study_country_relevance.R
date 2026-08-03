# =============================================================================
# File: 31_score_study_country_relevance.R
# Project: salmonscopingreview
# Purpose: Score detected country mentions deterministically and aggregate them
#          to country-level study-location relevance
# =============================================================================

source("scripts/00_setup.R")
source("R/read_corpus.R")

input_records <- here::here(
  "data_raw",
  "INCLUDES fixed abstracts.txt"
)

input_mentions <- here::here(
  "outputs",
  "stage_5_geography",
  "global_detection_v3",
  "global_geography_mentions_v3.csv"
)

input_rules <- here::here(
  "data_raw",
  "geography_scoring_dictionary.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_5_geography",
  "weighted_study_locations"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(input_records),
  file.exists(input_mentions),
  file.exists(input_rules)
)

records <- read_corpus(input_records) |>
  dplyr::mutate(
    record_id = as.character(record_id)
  )

mentions <- readr::read_csv(
  input_mentions,
  show_col_types = FALSE
) |>
  dplyr::mutate(
    record_id = as.character(record_id),
    context_lower = stringr::str_to_lower(
      dplyr::coalesce(context, "")
    ),
    matched_text_lower = stringr::str_to_lower(
      dplyr::coalesce(matched_text, "")
    ),
    source = stringr::str_to_lower(source)
  ) |>
  dplyr::filter(
    !is.na(iso3c),
    nzchar(iso3c)
  )

rules <- readr::read_csv(
  input_rules,
  show_col_types = FALSE
)

required_rule_columns <- c(
  "rule_id",
  "direction",
  "pattern",
  "score",
  "description"
)

missing_rule_columns <- setdiff(
  required_rule_columns,
  names(rules)
)

if (length(missing_rule_columns) > 0L) {
  stop(
    "Scoring dictionary is missing required columns: ",
    paste(missing_rule_columns, collapse = ", ")
  )
}

# ---------------------------------------------------------------------------
# Apply rule scores to each mention
# ---------------------------------------------------------------------------

apply_rule <- function(
    context_text,
    source_value,
    rule_id,
    pattern,
    score
) {

  if (rule_id == "title_country") {
    matched <- source_value == "title"
  } else {
    matched <- stringr::str_detect(
      context_text,
      stringr::regex(
        pattern,
        ignore_case = TRUE
      )
    )
  }

  ifelse(
    matched,
    score,
    0
  )
}

scored_mentions <- mentions

for (i in seq_len(nrow(rules))) {

  rule_column <- paste0(
    "score_",
    rules$rule_id[i]
  )

  scored_mentions[[rule_column]] <- apply_rule(
    context_text = scored_mentions$context_lower,
    source_value = scored_mentions$source,
    rule_id = rules$rule_id[i],
    pattern = rules$pattern[i],
    score = rules$score[i]
  )
}

score_columns <- grep(
  "^score_",
  names(scored_mentions),
  value = TRUE
)

scored_mentions <- scored_mentions |>
  dplyr::rowwise() |>
  dplyr::mutate(
    mention_score = sum(
      dplyr::c_across(
        dplyr::all_of(score_columns)
      ),
      na.rm = TRUE
    ),
    matched_rules = paste(
      rules$rule_id[
        c_across(
          dplyr::all_of(score_columns)
        ) != 0
      ],
      collapse = "; "
    )
  ) |>
  dplyr::ungroup()

# ---------------------------------------------------------------------------
# Aggregate repeated mentions by country
# ---------------------------------------------------------------------------

country_scores <- scored_mentions |>
  dplyr::group_by(
    record_sequence,
    record_id,
    iso3c
  ) |>
  dplyr::summarise(
    country_name = dplyr::first(
      country_name
    ),
    mention_count = dplyr::n(),
    positive_mentions = sum(
      mention_score > 0
    ),
    negative_mentions = sum(
      mention_score < 0
    ),
    max_mention_score = max(
      mention_score,
      na.rm = TRUE
    ),
    raw_score = sum(
      mention_score,
      na.rm = TRUE
    ),
    repetition_bonus = pmin(
      pmax(
        dplyr::n() - 1L,
        0L
      ) * 2L,
      6L
    ),
    country_score = raw_score + repetition_bonus,
    evidence = paste(
      unique(
        paste0(
          "[",
          source,
          "] ",
          matched_text,
          " | score=",
          mention_score,
          " | ",
          matched_rules,
          " | ",
          context
        )
      ),
      collapse = "\n"
    ),
    .groups = "drop"
  )

# ---------------------------------------------------------------------------
# Assignment and review rules
#
# Conservative defaults:
#   country_score >= 6  -> assign
#   country_score 3-5   -> borderline
#   country_score < 3   -> reject
#
# Review if:
#   - any borderline country exists;
#   - two assigned countries are within 3 points of one another;
#   - more than 4 countries are assigned.
# ---------------------------------------------------------------------------

assignment_threshold <- 6
borderline_threshold <- 3
close_score_margin <- 3

country_scores <- country_scores |>
  dplyr::mutate(
    assignment_status = dplyr::case_when(
      country_score >= assignment_threshold ~ "assign",
      country_score >= borderline_threshold ~ "borderline",
      TRUE ~ "reject"
    )
  )

record_diagnostics <- country_scores |>
  dplyr::group_by(
    record_sequence,
    record_id
  ) |>
  dplyr::summarise(
    assigned_country_count = sum(
      assignment_status == "assign"
    ),
    borderline_country_count = sum(
      assignment_status == "borderline"
    ),
    top_score = max(
      country_score,
      na.rm = TRUE
    ),
    second_score = if (
      dplyr::n() >= 2L
    ) {
      sort(
        country_score,
        decreasing = TRUE
      )[2]
    } else {
      NA_real_
    },
    close_top_scores = (
      !is.na(second_score) &
        (top_score - second_score) <= close_score_margin
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    study_location_review_required = (
      borderline_country_count > 0L |
        close_top_scores |
        assigned_country_count > 4L
    )
  )

assigned_countries <- country_scores |>
  dplyr::filter(
    assignment_status == "assign"
  ) |>
  dplyr::arrange(
    record_sequence,
    dplyr::desc(country_score),
    iso3c
  )

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

record_summary <- records |>
  dplyr::select(
    record_sequence,
    record_id,
    title,
    abstract
  ) |>
  dplyr::left_join(
    assigned_countries |>
      dplyr::group_by(
        record_sequence,
        record_id
      ) |>
      dplyr::summarise(
        study_countries = collapse_unique(
          country_name
        ),
        study_iso3c = collapse_unique(
          iso3c
        ),
        study_country_count = dplyr::n_distinct(
          iso3c
        ),
        maximum_country_score = max(
          country_score,
          na.rm = TRUE
        ),
        .groups = "drop"
      ),
    by = c(
      "record_sequence",
      "record_id"
    )
  ) |>
  dplyr::left_join(
    record_diagnostics,
    by = c(
      "record_sequence",
      "record_id"
    )
  ) |>
  dplyr::mutate(
    study_country_count = dplyr::coalesce(
      study_country_count,
      0L
    ),
    assigned_country_count = dplyr::coalesce(
      assigned_country_count,
      0L
    ),
    borderline_country_count = dplyr::coalesce(
      borderline_country_count,
      0L
    ),
    close_top_scores = dplyr::coalesce(
      close_top_scores,
      FALSE
    ),
    study_location_review_required = dplyr::coalesce(
      study_location_review_required,
      FALSE
    )
  )

review_queue <- country_scores |>
  dplyr::semi_join(
    record_summary |>
      dplyr::filter(
        study_location_review_required
      ) |>
      dplyr::select(
        record_sequence
      ),
    by = "record_sequence"
  ) |>
  dplyr::left_join(
    records |>
      dplyr::select(
        record_sequence,
        title,
        abstract
      ),
    by = "record_sequence"
  ) |>
  dplyr::mutate(
    validation_correct = NA_character_,
    corrected_study_countries = NA_character_,
    validation_notes = NA_character_
  ) |>
  dplyr::arrange(
    record_sequence,
    dplyr::desc(country_score),
    iso3c
  )

# ---------------------------------------------------------------------------
# Outputs
# ---------------------------------------------------------------------------

readr::write_csv(
  scored_mentions,
  fs::path(
    output_dir,
    "scored_geography_mentions.csv"
  ),
  na = ""
)

readr::write_csv(
  country_scores,
  fs::path(
    output_dir,
    "country_relevance_scores.csv"
  ),
  na = ""
)

readr::write_csv(
  assigned_countries,
  fs::path(
    output_dir,
    "weighted_study_country_annotations.csv"
  ),
  na = ""
)

readr::write_csv(
  record_summary,
  fs::path(
    output_dir,
    "weighted_study_location_record_summary.csv"
  ),
  na = ""
)

readr::write_csv(
  review_queue,
  fs::path(
    output_dir,
    "weighted_study_location_review_queue.csv"
  ),
  na = ""
)

message("Weighted study-location scoring completed.")
message(
  "Records with assigned study country: ",
  sum(record_summary$study_country_count > 0L)
)
message(
  "Records with multiple assigned study countries: ",
  sum(record_summary$study_country_count > 1L)
)
message(
  "Records requiring review: ",
  sum(record_summary$study_location_review_required)
)
message(
  "Borderline country rows: ",
  sum(country_scores$assignment_status == "borderline")
)
message(
  "Rejected country rows: ",
  sum(country_scores$assignment_status == "reject")
)
