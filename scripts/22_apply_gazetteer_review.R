# =============================================================================
# File: 22_apply_gazetteer_review.R
# Project: salmonscopingreview
# Purpose: Apply reviewed keep/remove decisions to the strict global gazetteer
# =============================================================================

source("scripts/00_setup.R")

input_gazetteer <- here::here(
  "outputs",
  "stage_5_geography",
  "global_country_gazetteer_filtered.csv"
)

input_review <- here::here(
  "outputs",
  "stage_5_geography",
  "strict_global_detection",
  "strict_global_top_matches_reviewed.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_5_geography"
)

stopifnot(
  file.exists(input_gazetteer),
  file.exists(input_review)
)

gazetteer <- readr::read_csv(
  input_gazetteer,
  show_col_types = FALSE
)

review <- readr::read_csv(
  input_review,
  show_col_types = FALSE
) |>
  dplyr::mutate(
    decision = stringr::str_to_lower(
      stringr::str_squish(decision)
    ),
    normalised_match = stringr::str_to_lower(
      stringr::str_squish(matched_place)
    )
  )

invalid_decisions <- review |>
  dplyr::filter(
    !decision %in% c(
      "keep",
      "remove"
    )
  )

if (nrow(invalid_decisions) > 0L) {
  stop(
    "Review file contains invalid or blank decisions. ",
    "Allowed values are keep and remove."
  )
}

review_decisions <- review |>
  dplyr::distinct(
    normalised_match,
    country_name,
    match_type,
    decision
  )

review_conflicts <- review_decisions |>
  dplyr::count(
    normalised_match,
    country_name,
    match_type,
    name = "decision_rows"
  ) |>
  dplyr::filter(
    decision_rows > 1L
  )

if (nrow(review_conflicts) > 0L) {
  stop(
    "Review file contains conflicting decisions for one or more mappings."
  )
}

gazetteer_reviewed <- gazetteer |>
  dplyr::left_join(
    review_decisions,
    by = c(
      "normalised_match",
      "country_name",
      "match_type"
    )
  ) |>
  dplyr::mutate(
    review_decision = dplyr::coalesce(
      decision,
      "not observed"
    )
  )

gazetteer_v2 <- gazetteer_reviewed |>
  dplyr::filter(
    review_decision != "remove"
  ) |>
  dplyr::select(
    -decision
  ) |>
  dplyr::arrange(
    dplyr::desc(priority),
    dplyr::desc(term_length),
    normalised_match,
    country_name
  )

change_log <- gazetteer_reviewed |>
  dplyr::filter(
    review_decision %in% c(
      "keep",
      "remove"
    )
  ) |>
  dplyr::select(
    matched_place,
    normalised_match,
    country_name,
    iso3c,
    match_type,
    review_decision
  ) |>
  dplyr::arrange(
    review_decision,
    matched_place,
    country_name
  )

readr::write_csv(
  gazetteer_v2,
  fs::path(
    output_dir,
    "global_country_gazetteer_v2.csv"
  ),
  na = ""
)

readr::write_csv(
  change_log,
  fs::path(
    output_dir,
    "global_country_gazetteer_v2_change_log.csv"
  ),
  na = ""
)

summary_tbl <- tibble::tibble(
  measure = c(
    "Input gazetteer rows",
    "Reviewed matched mappings",
    "Mappings retained by review",
    "Mappings removed by review",
    "Output gazetteer rows"
  ),
  value = c(
    nrow(gazetteer),
    sum(
      gazetteer_reviewed$review_decision %in%
        c("keep", "remove")
    ),
    sum(
      gazetteer_reviewed$review_decision == "keep"
    ),
    sum(
      gazetteer_reviewed$review_decision == "remove"
    ),
    nrow(gazetteer_v2)
  )
)

readr::write_csv(
  summary_tbl,
  fs::path(
    output_dir,
    "global_country_gazetteer_v2_summary.csv"
  ),
  na = ""
)

message("Reviewed gazetteer written.")
message("Input gazetteer rows: ", nrow(gazetteer))
message(
  "Mappings retained by review: ",
  sum(gazetteer_reviewed$review_decision == "keep")
)
message(
  "Mappings removed by review: ",
  sum(gazetteer_reviewed$review_decision == "remove")
)
message("Output gazetteer rows: ", nrow(gazetteer_v2))
