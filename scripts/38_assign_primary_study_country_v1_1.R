# =============================================================================
# File: 38_assign_primary_study_country_v1_1.R
# Project: salmonscopingreview
# Purpose: Restore the original primary-country classifier and change only the
#          final decision rule so review flags never replace assignments
# =============================================================================

source("scripts/00_setup.R")

input_mentions <- here::here(
  "outputs",
  "stage_5_geography",
  "global_detection_v3",
  "global_geography_mentions_v3.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_5_geography",
  "primary_study_country_v1_1"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(input_mentions)
)

mentions <- readr::read_csv(
  input_mentions,
  show_col_types = FALSE
) |>
  dplyr::filter(
    !is.na(iso3c),
    nzchar(iso3c)
  ) |>
  dplyr::mutate(
    record_id = as.character(record_id),
    context_lower = stringr::str_to_lower(
      dplyr::coalesce(context, "")
    ),
    source = stringr::str_to_lower(source),

    tier = dplyr::case_when(
      source == "title" ~ 1L,

      stringr::str_detect(
        context_lower,
        paste(
          c(
            "study area",
            "study site",
            "study sites",
            "conducted in",
            "conducted at",
            "sampled",
            "collected",
            "surveyed",
            "fieldwork",
            "case study",
            "farms in",
            "farm in",
            "hatcher",
            "site in",
            "located in"
          ),
          collapse = "|"
        )
      ) ~ 2L,

      stringr::str_detect(
        context_lower,
        paste(
          c(
            "stakeholder",
            "stakeholders",
            "farm",
            "farms",
            "company",
            "companies",
            "industry",
            "producer",
            "population",
            "community",
            "river",
            "lake",
            "coast"
          ),
          collapse = "|"
        )
      ) ~ 3L,

      stringr::str_detect(
        context_lower,
        paste(
          c(
            "copyright",
            "published by",
            "publisher",
            "springer",
            "elsevier",
            "wiley",
            "translate with",
            "software",
            "manufacturer",
            "manufactured by",
            "supplied by",
            "provided by",
            "purchased from"
          ),
          collapse = "|"
        )
      ) ~ 99L,

      TRUE ~ 4L
    )
  ) |>
  dplyr::filter(
    tier < 99L
  )

# Rank countries using the original evidence hierarchy ------------------------

country_rank <- mentions |>
  dplyr::group_by(
    record_sequence,
    record_id,
    iso3c,
    country_name
  ) |>
  dplyr::summarise(
    best_tier = min(tier),
    title_mentions = sum(source == "title"),
    substantive_mentions = sum(tier <= 3L),
    total_mentions = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    record_sequence,
    best_tier,
    dplyr::desc(title_mentions),
    dplyr::desc(substantive_mentions),
    dplyr::desc(total_mentions),
    iso3c
  )

# Final decision rule ----------------------------------------------------------
#
# 1. Keep all countries at the best evidence tier.
# 2. If one remains, assign it.
# 3. If two remain, assign both and flag for review.
# 4. If more than two remain, assign the single highest-ranked candidate and
#    flag for review.
# 5. Review never means "leave blank".

ranked_candidates <- country_rank |>
  dplyr::group_by(
    record_sequence,
    record_id
  ) |>
  dplyr::mutate(
    best_tier_record = min(best_tier),
    candidate_at_best_tier = best_tier == best_tier_record,
    best_tier_candidate_count = sum(candidate_at_best_tier)
  ) |>
  dplyr::arrange(
    best_tier,
    dplyr::desc(title_mentions),
    dplyr::desc(substantive_mentions),
    dplyr::desc(total_mentions),
    iso3c,
    .by_group = TRUE
  ) |>
  dplyr::mutate(
    decision_rank = dplyr::row_number()
  ) |>
  dplyr::ungroup()

primary_assignments <- ranked_candidates |>
  dplyr::filter(
    dplyr::case_when(
      best_tier_candidate_count == 1L ~
        candidate_at_best_tier,

      best_tier_candidate_count == 2L ~
        candidate_at_best_tier,

      best_tier_candidate_count > 2L ~
        decision_rank == 1L,

      TRUE ~ FALSE
    )
  ) |>
  dplyr::mutate(
    review_required = best_tier_candidate_count > 1L,
    assignment_reason = dplyr::case_when(
      best_tier_candidate_count == 1L &
        best_tier == 1L ~
        "Single strongest country explicitly named in title",

      best_tier_candidate_count == 1L &
        best_tier == 2L ~
        "Single strongest country from explicit study-location context",

      best_tier_candidate_count == 1L &
        best_tier == 3L ~
        "Single strongest country from substantive study-entity context",

      best_tier_candidate_count == 1L ~
        "Single strongest country from general evidence",

      best_tier_candidate_count == 2L ~
        "Two countries tied at the strongest evidence tier",

      best_tier_candidate_count > 2L ~
        "Top-ranked country assigned from a multi-country strongest-tier tie",

      TRUE ~
        "Primary country assigned"
    )
  ) |>
  dplyr::select(
    record_sequence,
    record_id,
    country_name,
    iso3c,
    best_tier,
    title_mentions,
    substantive_mentions,
    total_mentions,
    best_tier_candidate_count,
    review_required,
    assignment_reason
  ) |>
  dplyr::arrange(
    record_sequence,
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

primary_summary <- primary_assignments |>
  dplyr::group_by(
    record_sequence,
    record_id
  ) |>
  dplyr::summarise(
    primary_countries = collapse_unique(
      country_name
    ),
    primary_iso3c = collapse_unique(
      iso3c
    ),
    primary_country_count = dplyr::n_distinct(
      iso3c
    ),
    review_required = any(
      review_required
    ),
    assignment_reasons = collapse_unique(
      assignment_reason
    ),
    strongest_tier_candidate_count = max(
      best_tier_candidate_count
    ),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    record_sequence
  )

review_queue <- ranked_candidates |>
  dplyr::semi_join(
    primary_summary |>
      dplyr::filter(
        review_required
      ) |>
      dplyr::select(
        record_sequence
      ),
    by = "record_sequence"
  ) |>
  dplyr::left_join(
    primary_assignments |>
      dplyr::select(
        record_sequence,
        assigned_iso3c = iso3c,
        assigned_country = country_name,
        assignment_reason
      ),
    by = "record_sequence",
    relationship = "many-to-many"
  ) |>
  dplyr::arrange(
    record_sequence,
    best_tier,
    dplyr::desc(title_mentions),
    dplyr::desc(substantive_mentions),
    dplyr::desc(total_mentions),
    iso3c
  )

# Outputs ---------------------------------------------------------------------

readr::write_csv(
  country_rank,
  fs::path(
    output_dir,
    "country_evidence_ranking_v1_1.csv"
  ),
  na = ""
)

readr::write_csv(
  primary_assignments,
  fs::path(
    output_dir,
    "primary_country_assignments_v1_1.csv"
  ),
  na = ""
)

readr::write_csv(
  primary_summary,
  fs::path(
    output_dir,
    "primary_country_summary_v1_1.csv"
  ),
  na = ""
)

readr::write_csv(
  review_queue,
  fs::path(
    output_dir,
    "primary_country_review_queue_v1_1.csv"
  ),
  na = ""
)

message("Primary study-country classifier v1.1 completed.")
message(
  "Records assigned: ",
  nrow(primary_summary)
)
message(
  "Single-country assignments: ",
  sum(primary_summary$primary_country_count == 1L)
)
message(
  "Two-country assignments: ",
  sum(primary_summary$primary_country_count == 2L)
)
message(
  "Records requiring review: ",
  sum(primary_summary$review_required)
)
message(
  "Assignments left blank: ",
  sum(is.na(primary_summary$primary_countries))
)
