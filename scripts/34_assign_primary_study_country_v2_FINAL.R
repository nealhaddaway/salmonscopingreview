# =============================================================================
# File: 34_assign_primary_study_country_v2.R
# Project: salmonscopingreview
# Purpose: Assign primary study country with strict title precedence. A single
#          country in the title overrides all country mentions in the abstract.
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
  "primary_study_country_v2"
)

fs::dir_create(output_dir)

stopifnot(file.exists(input_mentions))

mentions <- readr::read_csv(
  input_mentions,
  show_col_types = FALSE
) |>
  dplyr::mutate(
    record_id = as.character(record_id),
    source = stringr::str_to_lower(source),
    context_lower = stringr::str_to_lower(
      dplyr::coalesce(context, "")
    ),
    matched_text_lower = stringr::str_to_lower(
      dplyr::coalesce(matched_text, "")
    )
  )

# ---------------------------------------------------------------------------
# Exclusion rules
# ---------------------------------------------------------------------------

artefact_pattern <- paste(
  c(
    "copyright",
    "all rights reserved",
    "creative commons",
    "published by",
    "publisher",
    "springer",
    "elsevier",
    "wiley",
    "taylor & francis",
    "translate with",
    "translation",
    "language selector",
    "software",
    "version",
    "manufacturer",
    "manufactured by",
    "supplied by",
    "provided by",
    "purchased from",
    "equipment",
    "instrument",
    "microscope",
    "camera",
    "reader",
    "incubator",
    "analyser",
    "analyzer"
  ),
  collapse = "|"
)

# Demonyms used directly as species descriptors should not become geography:
# e.g. "Australian salmon". Build the pattern row by row from the detected
# matched text rather than referring to a non-existent global object.
country_mentions <- mentions |>
  dplyr::filter(
    !is.na(iso3c),
    nzchar(iso3c)
  ) |>
  dplyr::mutate(
    publication_or_vendor_artefact = stringr::str_detect(
      context_lower,
      artefact_pattern
    ),
    species_adjective = purrr::map2_lgl(
      context_lower,
      matched_text_lower,
      function(context_value, matched_value) {
        species_phrases <- paste(
          matched_value,
          c(
            "salmon",
            "trout",
            "char",
            "grayling",
            "fish"
          )
        )

        any(
          stringr::str_detect(
            context_value,
            stringr::fixed(
              species_phrases,
              ignore_case = TRUE
            )
          )
        )
      }
    )
  ) |>
  dplyr::filter(
    !publication_or_vendor_artefact,
    !species_adjective
  )

# ---------------------------------------------------------------------------
# Evidence tiers
# ---------------------------------------------------------------------------

strong_location_pattern <- paste(
  c(
    "conducted in",
    "conducted at",
    "study was carried out in",
    "study was undertaken in",
    "study was performed in",
    "study area",
    "study site",
    "study sites",
    "sampled in",
    "sampled from",
    "samples were collected in",
    "samples were collected from",
    "collected in",
    "collected from",
    "obtained in",
    "obtained from",
    "farms in",
    "farm in",
    "fish farms in",
    "aquaculture farms in",
    "sites in",
    "site in",
    "located in",
    "reared in",
    "raised in",
    "cultured in",
    "produced in",
    "originating from",
    "originated from",
    "surveyed in",
    "interviewed in",
    "fieldwork in",
    "case study in"
  ),
  collapse = "|"
)

substantive_entity_pattern <- paste(
  c(
    "stakeholder",
    "stakeholders",
    "farmer",
    "farmers",
    "producer",
    "producers",
    "company",
    "companies",
    "industry",
    "industries",
    "farm",
    "farms",
    "hatchery",
    "hatcheries",
    "population",
    "populations",
    "community",
    "communities",
    "river",
    "rivers",
    "lake",
    "lakes",
    "site",
    "sites"
  ),
  collapse = "|"
)

background_pattern <- paste(
  c(
    "previous studies in",
    "previous research in",
    "reported in",
    "reported from",
    "compared with",
    "compared to",
    "unlike",
    "elsewhere in",
    "for example in",
    "such as",
    "including"
  ),
  collapse = "|"
)

country_mentions <- country_mentions |>
  dplyr::mutate(
    evidence_tier = dplyr::case_when(
      source == "title" ~ 1L,
      stringr::str_detect(
        context_lower,
        strong_location_pattern
      ) ~ 2L,
      stringr::str_detect(
        context_lower,
        substantive_entity_pattern
      ) &
        !stringr::str_detect(
          context_lower,
          background_pattern
        ) ~ 3L,
      stringr::str_detect(
        context_lower,
        background_pattern
      ) ~ 5L,
      TRUE ~ 4L
    )
  )

# ---------------------------------------------------------------------------
# Detect title-level regional scope
#
# If a title states a continent/macro-region but no country, abstract country
# mentions are not promoted automatically to primary study countries.
# ---------------------------------------------------------------------------

title_region_records <- mentions |>
  dplyr::filter(
    source == "title",
    is.na(iso3c),
    !is.na(region_name),
    nzchar(region_name)
  ) |>
  dplyr::distinct(
    record_sequence
  ) |>
  dplyr::mutate(
    title_has_region_scope = TRUE
  )

title_country_candidates <- country_mentions |>
  dplyr::filter(
    source == "title"
  ) |>
  dplyr::distinct(
    record_sequence,
    record_id,
    iso3c,
    country_name
  )

record_flags <- country_mentions |>
  dplyr::distinct(
    record_sequence,
    record_id
  ) |>
  dplyr::left_join(
    title_country_candidates |>
      dplyr::count(
        record_sequence,
        name = "title_country_count"
      ),
    by = "record_sequence"
  ) |>
  dplyr::left_join(
    title_region_records,
    by = "record_sequence"
  ) |>
  dplyr::mutate(
    title_country_count = dplyr::coalesce(
      title_country_count,
      0L
    ),
    title_has_region_scope = dplyr::coalesce(
      title_has_region_scope,
      FALSE
    )
  )

# ---------------------------------------------------------------------------
# Rank abstract candidates
# ---------------------------------------------------------------------------

country_ranking <- country_mentions |>
  dplyr::group_by(
    record_sequence,
    record_id,
    iso3c,
    country_name
  ) |>
  dplyr::summarise(
    best_tier = min(
      evidence_tier,
      na.rm = TRUE
    ),
    title_mentions = sum(
      source == "title"
    ),
    substantive_mentions = sum(
      evidence_tier <= 3L
    ),
    total_mentions = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    record_flags,
    by = c(
      "record_sequence",
      "record_id"
    )
  ) |>
  dplyr::arrange(
    record_sequence,
    best_tier,
    dplyr::desc(substantive_mentions),
    dplyr::desc(total_mentions),
    iso3c
  )

# ---------------------------------------------------------------------------
# Assignments
#
# A. If one or more countries occur in the title:
#    retain all title countries. They are explicitly co-primary.
#
# B. If the title contains a continent/macro-region but no country:
#    assign no country automatically; send to review.
#
# C. Otherwise:
#    assign exactly one best abstract country.
#    If the best candidates remain exactly tied after evidence tier,
#    substantive mentions and total mentions, assign none and send to review.
# ---------------------------------------------------------------------------

# Strict title precedence:
# - exactly one unique title country -> that country only; abstract geography
#   cannot add or replace the assignment;
# - multiple unique title countries -> retain those title countries only.
title_assignments <- country_ranking |>
  dplyr::filter(
    title_country_count > 0L,
    title_mentions > 0L
  ) |>
  dplyr::mutate(
    assignment_reason = dplyr::case_when(
      title_country_count == 1L ~
        "Single title country overrides abstract geography",
      TRUE ~
        "Countries explicitly co-named in title"
    )
  )

abstract_ranked <- country_ranking |>
  dplyr::filter(
    title_country_count == 0L,
    !title_has_region_scope
  ) |>
  dplyr::group_by(
    record_sequence,
    record_id
  ) |>
  dplyr::mutate(
    best_tier_record = min(best_tier),
    best_substantive_mentions = max(
      substantive_mentions[
        best_tier == best_tier_record
      ]
    ),
    best_total_mentions = max(
      total_mentions[
        best_tier == best_tier_record &
          substantive_mentions == best_substantive_mentions
      ]
    ),
    final_tie = sum(
      best_tier == best_tier_record &
        substantive_mentions == best_substantive_mentions &
        total_mentions == best_total_mentions
    )
  ) |>
  dplyr::ungroup()

abstract_assignments <- abstract_ranked |>
  dplyr::filter(
    best_tier == best_tier_record,
    substantive_mentions == best_substantive_mentions,
    total_mentions == best_total_mentions,
    final_tie == 1L
  ) |>
  dplyr::mutate(
    assignment_reason = dplyr::case_when(
      best_tier == 2L ~
        "Primary country from explicit study-location context",
      best_tier == 3L ~
        "Primary country from substantive study-entity context",
      best_tier == 4L ~
        "Primary country from dominant general mention",
      TRUE ~
        "Primary country from strongest available abstract evidence"
    )
  )

primary_assignments <- dplyr::bind_rows(
  title_assignments,
  abstract_assignments
) |>
  dplyr::select(
    record_sequence,
    record_id,
    country_name,
    iso3c,
    assignment_reason,
    best_tier,
    title_mentions,
    substantive_mentions,
    total_mentions
  ) |>
  dplyr::distinct(
    record_sequence,
    iso3c,
    .keep_all = TRUE
  ) |>
  dplyr::arrange(
    record_sequence,
    iso3c
  )

# Safety check for the title-precedence rule.
single_title_violations <- primary_assignments |>
  dplyr::inner_join(
    record_flags |>
      dplyr::filter(title_country_count == 1L) |>
      dplyr::select(record_sequence),
    by = "record_sequence"
  ) |>
  dplyr::count(record_sequence) |>
  dplyr::filter(n > 1L)

if (nrow(single_title_violations) > 0L) {
  stop(
    "Title-precedence failure: ",
    nrow(single_title_violations),
    " records with one title country received multiple primary countries."
  )
}

# ---------------------------------------------------------------------------
# Review reasons
# ---------------------------------------------------------------------------

tie_records <- abstract_ranked |>
  dplyr::filter(
    final_tie > 1L
  ) |>
  dplyr::distinct(
    record_sequence,
    record_id
  ) |>
  dplyr::mutate(
    review_reason = "Abstract candidates remain exactly tied"
  )

regional_scope_records <- record_flags |>
  dplyr::filter(
    title_country_count == 0L,
    title_has_region_scope
  ) |>
  dplyr::select(
    record_sequence,
    record_id
  ) |>
  dplyr::mutate(
    review_reason = "Title specifies a continent or macro-region rather than a country"
  )

review_records <- dplyr::bind_rows(
  tie_records,
  regional_scope_records
) |>
  dplyr::distinct(
    record_sequence,
    .keep_all = TRUE
  )

collapse_unique <- function(x) {
  values <- sort(
    unique(
      stats::na.omit(x)
    )
  )
  values <- values[nzchar(values)]
  if (length(values) == 0L) {
    NA_character_
  } else {
    paste(values, collapse = "; ")
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
    assignment_reasons = collapse_unique(
      assignment_reason
    ),
    .groups = "drop"
  ) |>
  dplyr::full_join(
    review_records,
    by = c(
      "record_sequence",
      "record_id"
    )
  ) |>
  dplyr::mutate(
    primary_country_count = dplyr::coalesce(
      primary_country_count,
      0L
    ),
    review_required = !is.na(review_reason)
  ) |>
  dplyr::arrange(
    record_sequence
  )

review_queue <- country_ranking |>
  dplyr::semi_join(
    review_records,
    by = c(
      "record_sequence",
      "record_id"
    )
  ) |>
  dplyr::left_join(
    review_records,
    by = c(
      "record_sequence",
      "record_id"
    )
  ) |>
  dplyr::arrange(
    record_sequence,
    best_tier,
    dplyr::desc(substantive_mentions),
    dplyr::desc(total_mentions),
    iso3c
  )

# ---------------------------------------------------------------------------
# Outputs
# ---------------------------------------------------------------------------

readr::write_csv(
  country_ranking,
  fs::path(
    output_dir,
    "country_evidence_ranking_v2.csv"
  ),
  na = ""
)

readr::write_csv(
  primary_assignments,
  fs::path(
    output_dir,
    "primary_country_assignments_v2.csv"
  ),
  na = ""
)

readr::write_csv(
  primary_summary,
  fs::path(
    output_dir,
    "primary_country_summary_v2.csv"
  ),
  na = ""
)

readr::write_csv(
  review_queue,
  fs::path(
    output_dir,
    "primary_country_review_queue_v2.csv"
  ),
  na = ""
)

message("Primary study-country classifier v2 completed.")
message(
  "Records assigned: ",
  sum(primary_summary$primary_country_count > 0L)
)
message(
  "Single-country assignments: ",
  sum(primary_summary$primary_country_count == 1L)
)
message(
  "Multi-country title assignments: ",
  sum(primary_summary$primary_country_count > 1L)
)
message(
  "Records requiring review: ",
  sum(primary_summary$review_required)
)
message(
  "Exact abstract ties: ",
  nrow(tie_records)
)
message(
  "Regional-scope titles: ",
  nrow(regional_scope_records)
)
