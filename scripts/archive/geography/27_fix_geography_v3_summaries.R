# =============================================================================
# File: 27_fix_geography_v3_summaries.R
# Project: salmonscopingreview
# Purpose: Rebuild v3 geography summaries from detected mentions, fixing the
#          multiple-country count and canonicalising country names by ISO3
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
  "global_detection_v3"
)

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
    record_id = as.character(record_id)
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

# Canonical country names ------------------------------------------------------

country_lookup <- mentions |>
  dplyr::filter(
    !is.na(iso3c),
    nzchar(iso3c)
  ) |>
  dplyr::distinct(
    iso3c,
    country_name
  ) |>
  dplyr::mutate(
    canonical_from_countrycode = countrycode::countrycode(
      iso3c,
      origin = "iso3c",
      destination = "country.name",
      warn = FALSE
    )
  ) |>
  dplyr::group_by(
    iso3c
  ) |>
  dplyr::summarise(
    country_name = dplyr::coalesce(
      dplyr::first(
        stats::na.omit(
          canonical_from_countrycode
        )
      ),
      dplyr::first(
        stats::na.omit(
          country_name
        )
      )
    ),
    .groups = "drop"
  )

country_by_record <- mentions |>
  dplyr::filter(
    !is.na(iso3c),
    nzchar(iso3c)
  ) |>
  dplyr::distinct(
    record_sequence,
    record_id,
    iso3c
  ) |>
  dplyr::left_join(
    country_lookup,
    by = "iso3c"
  )

region_by_record <- mentions |>
  dplyr::filter(
    !is.na(region_name),
    nzchar(region_name)
  ) |>
  dplyr::distinct(
    record_sequence,
    record_id,
    region_name
  )

# Rebuild record-level summary -------------------------------------------------

record_summary <- records |>
  dplyr::select(
    record_sequence,
    record_id,
    title,
    abstract
  ) |>
  dplyr::left_join(
    mentions |>
      dplyr::group_by(
        record_sequence,
        record_id
      ) |>
      dplyr::summarise(
        matched_places = collapse_unique(
          matched_place
        ),
        mention_count = dplyr::n(),
        ambiguous_mention = any(
          ambiguous %in% TRUE
        ),
        .groups = "drop"
      ),
    by = c(
      "record_sequence",
      "record_id"
    )
  ) |>
  dplyr::left_join(
    country_by_record |>
      dplyr::group_by(
        record_sequence,
        record_id
      ) |>
      dplyr::summarise(
        # Count before collapsing ISO codes. In dplyr::summarise(), later
        # expressions can otherwise see columns created earlier in the call.
        country_count = dplyr::n_distinct(
          iso3c
        ),
        countries_mentioned = collapse_unique(
          country_name
        ),
        iso3c = collapse_unique(
          iso3c
        ),
        .groups = "drop"
      ),
    by = c(
      "record_sequence",
      "record_id"
    )
  ) |>
  dplyr::left_join(
    region_by_record |>
      dplyr::group_by(
        record_sequence,
        record_id
      ) |>
      dplyr::summarise(
        region_count = dplyr::n_distinct(
          region_name
        ),
        regions_mentioned = collapse_unique(
          region_name
        ),
        .groups = "drop"
      ),
    by = c(
      "record_sequence",
      "record_id"
    )
  ) |>
  dplyr::mutate(
    mention_count = dplyr::coalesce(
      mention_count,
      0L
    ),
    country_count = dplyr::coalesce(
      country_count,
      0L
    ),
    region_count = dplyr::coalesce(
      region_count,
      0L
    ),
    ambiguous_mention = dplyr::coalesce(
      ambiguous_mention,
      FALSE
    ),
    multiple_countries = country_count > 1L,
    geography_review_required = (
      ambiguous_mention |
        multiple_countries
    )
  )

# Rebuild review queue ---------------------------------------------------------

review_queue <- mentions |>
  dplyr::semi_join(
    record_summary |>
      dplyr::filter(
        geography_review_required
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
    corrected_country = NA_character_,
    validation_notes = NA_character_
  ) |>
  dplyr::arrange(
    record_sequence,
    source,
    token_start,
    country_name,
    region_name
  )

country_frequency <- country_by_record |>
  dplyr::count(
    country_name,
    iso3c,
    name = "records",
    sort = TRUE
  )

# Write corrected outputs ------------------------------------------------------

readr::write_csv(
  record_summary,
  fs::path(
    output_dir,
    "global_geography_record_summary_v3.csv"
  ),
  na = ""
)

readr::write_csv(
  country_by_record,
  fs::path(
    output_dir,
    "record_country_annotations_v3.csv"
  ),
  na = ""
)

readr::write_csv(
  region_by_record,
  fs::path(
    output_dir,
    "record_region_annotations_v3.csv"
  ),
  na = ""
)

readr::write_csv(
  review_queue,
  fs::path(
    output_dir,
    "global_geography_review_queue_v3.csv"
  ),
  na = ""
)

readr::write_csv(
  country_frequency,
  fs::path(
    output_dir,
    "global_country_frequency_v3.csv"
  ),
  na = ""
)

message("Geography v3 summaries rebuilt.")
message(
  "Records with countries: ",
  sum(record_summary$country_count > 0L)
)
message(
  "Records with multiple countries: ",
  sum(record_summary$multiple_countries)
)
message(
  "Records requiring review: ",
  sum(record_summary$geography_review_required)
)
message(
  "Maximum countries in one record: ",
  max(record_summary$country_count)
)
