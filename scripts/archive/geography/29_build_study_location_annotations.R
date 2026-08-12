# =============================================================================
# File: 29_build_study_location_annotations.R
# Project: salmonscopingreview
# Purpose: Derive conservative study-country annotations from detected
#          geography using title precedence, study-context cues and
#          publication-boilerplate exclusion
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

output_dir <- here::here(
  "outputs",
  "stage_5_geography",
  "study_locations"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(input_records),
  file.exists(input_mentions)
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
    )
  )

# ---------------------------------------------------------------------------
# Exclude obvious publisher, copyright and translation artefacts
# ---------------------------------------------------------------------------

artefact_pattern <- paste(
  c(
    "all rights reserved",
    "copyright",
    "creative commons",
    "publisher",
    "published by",
    "springer",
    "elsevier",
    "wiley",
    "taylor & francis",
    "translate with",
    "translation",
    "language selector",
    "crossref",
    "scopus",
    "web of science",
    "received:",
    "accepted:",
    "available online"
  ),
  collapse = "|"
)

mentions_clean <- mentions |>
  dplyr::mutate(
    publication_artefact = stringr::str_detect(
      context_lower,
      artefact_pattern
    )
  ) |>
  dplyr::filter(
    !publication_artefact
  )

# ---------------------------------------------------------------------------
# Identify strong study-location context
# ---------------------------------------------------------------------------

study_context_pattern <- paste(
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
    "location in",
    "reared in",
    "raised in",
    "cultured in",
    "produced in",
    "originating from",
    "originated from",
    "from farms in",
    "from hatcheries in",
    "from rivers in",
    "from lakes in",
    "from streams in",
    "from waters in",
    "case study in",
    "surveyed in",
    "questionnaire in",
    "fieldwork in"
  ),
  collapse = "|"
)

background_pattern <- paste(
  c(
    "previous studies in",
    "previous research in",
    "reported in",
    "compared with",
    "compared to",
    "unlike",
    "elsewhere in",
    "for example in",
    "such as",
    "including",
    "globally",
    "worldwide",
    "internationally",
    "reviewed studies from"
  ),
  collapse = "|"
)

mentions_clean <- mentions_clean |>
  dplyr::mutate(
    strong_study_context = stringr::str_detect(
      context_lower,
      study_context_pattern
    ),
    background_context = stringr::str_detect(
      context_lower,
      background_pattern
    )
  )

# ---------------------------------------------------------------------------
# Country-level candidates only
# ---------------------------------------------------------------------------

country_mentions <- mentions_clean |>
  dplyr::filter(
    !is.na(iso3c),
    nzchar(iso3c)
  ) |>
  dplyr::distinct(
    record_sequence,
    record_id,
    source,
    matched_text,
    matched_place,
    country_name,
    iso3c,
    context,
    strong_study_context,
    background_context,
    .keep_all = TRUE
  )

title_countries <- country_mentions |>
  dplyr::filter(
    source == "title"
  ) |>
  dplyr::distinct(
    record_sequence,
    record_id,
    country_name,
    iso3c
  )

abstract_candidates <- country_mentions |>
  dplyr::filter(
    source == "abstract"
  )

abstract_country_counts <- abstract_candidates |>
  dplyr::distinct(
    record_sequence,
    iso3c
  ) |>
  dplyr::count(
    record_sequence,
    name = "abstract_country_count"
  )

records_with_title_country <- title_countries |>
  dplyr::distinct(
    record_sequence
  ) |>
  dplyr::mutate(
    has_title_country = TRUE
  )

candidate_table <- records |>
  dplyr::select(
    record_sequence,
    record_id,
    title,
    abstract
  ) |>
  dplyr::left_join(
    records_with_title_country,
    by = "record_sequence"
  ) |>
  dplyr::left_join(
    abstract_country_counts,
    by = "record_sequence"
  ) |>
  dplyr::mutate(
    has_title_country = dplyr::coalesce(
      has_title_country,
      FALSE
    ),
    abstract_country_count = dplyr::coalesce(
      abstract_country_count,
      0L
    )
  )

# ---------------------------------------------------------------------------
# Selection rules
#
# 1. Any country explicitly named in the title is retained.
# 2. Where the title contains a country, an additional abstract country is
#    retained only when it occurs in strong study-location context.
# 3. Where the title contains no country:
#    a. retain abstract countries in strong study-location context;
#    b. if the abstract contains exactly one country and it is not in a clear
#       background context, retain it even without a strong cue;
#    c. otherwise leave unresolved for manual review.
# ---------------------------------------------------------------------------

title_assignments <- title_countries |>
  dplyr::mutate(
    selection_reason = "Country explicitly named in title",
    source_evidence = "title"
  )

abstract_assignments <- abstract_candidates |>
  dplyr::left_join(
    candidate_table |>
      dplyr::select(
        record_sequence,
        has_title_country,
        abstract_country_count
      ),
    by = "record_sequence"
  ) |>
  dplyr::filter(
    dplyr::case_when(
      has_title_country ~ strong_study_context,
      !has_title_country &
        strong_study_context ~ TRUE,
      !has_title_country &
        abstract_country_count == 1L &
        !background_context ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  dplyr::mutate(
    selection_reason = dplyr::case_when(
      has_title_country &
        strong_study_context ~
        "Additional country supported by strong abstract study-location context",
      !has_title_country &
        strong_study_context ~
        "Country supported by strong abstract study-location context",
      !has_title_country &
        abstract_country_count == 1L &
        !background_context ~
        "Only non-background country mentioned in abstract",
      TRUE ~
        "Other"
    ),
    source_evidence = "abstract"
  ) |>
  dplyr::select(
    record_sequence,
    record_id,
    country_name,
    iso3c,
    selection_reason,
    source_evidence
  )

study_country_annotations <- dplyr::bind_rows(
  title_assignments,
  abstract_assignments
) |>
  dplyr::distinct(
    record_sequence,
    record_id,
    iso3c,
    .keep_all = TRUE
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

# ---------------------------------------------------------------------------
# Manual-review triggers
# ---------------------------------------------------------------------------

unresolved_candidates <- country_mentions |>
  dplyr::anti_join(
    study_country_annotations |>
      dplyr::select(
        record_sequence,
        iso3c
      ),
    by = c(
      "record_sequence",
      "iso3c"
    )
  ) |>
  dplyr::distinct(
    record_sequence,
    iso3c
  ) |>
  dplyr::count(
    record_sequence,
    name = "unresolved_country_count"
  )

record_summary <- records |>
  dplyr::select(
    record_sequence,
    record_id,
    title,
    abstract
  ) |>
  dplyr::left_join(
    study_country_annotations |>
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
        selection_reasons = collapse_unique(
          selection_reason
        ),
        .groups = "drop"
      ),
    by = c(
      "record_sequence",
      "record_id"
    )
  ) |>
  dplyr::left_join(
    candidate_table |>
      dplyr::select(
        record_sequence,
        has_title_country,
        abstract_country_count
      ),
    by = "record_sequence"
  ) |>
  dplyr::left_join(
    unresolved_candidates,
    by = "record_sequence"
  ) |>
  dplyr::mutate(
    study_country_count = dplyr::coalesce(
      study_country_count,
      0L
    ),
    unresolved_country_count = dplyr::coalesce(
      unresolved_country_count,
      0L
    ),
    has_title_country = dplyr::coalesce(
      has_title_country,
      FALSE
    ),
    abstract_country_count = dplyr::coalesce(
      abstract_country_count,
      0L
    ),
    study_location_review_required = (
      unresolved_country_count > 0L |
        study_country_count > 1L
    )
  )

review_queue <- record_summary |>
  dplyr::filter(
    study_location_review_required
  ) |>
  dplyr::left_join(
    country_mentions |>
      dplyr::group_by(
        record_sequence
      ) |>
      dplyr::summarise(
        all_detected_countries = collapse_unique(
          country_name
        ),
        all_detected_iso3c = collapse_unique(
          iso3c
        ),
        detected_contexts = paste(
          unique(
            paste0(
              "[",
              source,
              "] ",
              matched_text,
              ": ",
              context
            )
          ),
          collapse = "\n"
        ),
        .groups = "drop"
      ),
    by = "record_sequence"
  ) |>
  dplyr::mutate(
    validation_correct = NA_character_,
    corrected_study_countries = NA_character_,
    validation_notes = NA_character_
  ) |>
  dplyr::arrange(
    dplyr::desc(unresolved_country_count),
    record_sequence
  )

# ---------------------------------------------------------------------------
# Outputs
# ---------------------------------------------------------------------------

readr::write_csv(
  study_country_annotations,
  fs::path(
    output_dir,
    "study_country_annotations.csv"
  ),
  na = ""
)

readr::write_csv(
  record_summary,
  fs::path(
    output_dir,
    "study_location_record_summary.csv"
  ),
  na = ""
)

readr::write_csv(
  review_queue,
  fs::path(
    output_dir,
    "study_location_review_queue.csv"
  ),
  na = ""
)

message("Study-location annotation completed.")
message(
  "Records with assigned study country: ",
  sum(record_summary$study_country_count > 0L)
)
message(
  "Records with title country: ",
  sum(record_summary$has_title_country)
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
  "Records with unresolved country candidates: ",
  sum(record_summary$unresolved_country_count > 0L)
)
