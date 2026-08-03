# =============================================================================
# File: 19_audit_global_geography_detection.R
# Project: salmonscopingreview
# Purpose: Audit the first global geoparser run before accepting its outputs
# =============================================================================

source("scripts/00_setup.R")

input_mentions <- here::here(
  "outputs",
  "stage_5_geography",
  "global_detection",
  "global_geography_mentions.csv"
)

input_summary <- here::here(
  "outputs",
  "stage_5_geography",
  "global_detection",
  "global_geography_record_summary.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_5_geography",
  "global_detection",
  "audit"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(input_mentions),
  file.exists(input_summary)
)

mentions <- readr::read_csv(
  input_mentions,
  show_col_types = FALSE
)

record_summary <- readr::read_csv(
  input_summary,
  show_col_types = FALSE
)

# Recalculate country counts independently ------------------------------------

recalculated_counts <- mentions |>
  dplyr::filter(
    !is.na(iso3c),
    nzchar(iso3c)
  ) |>
  dplyr::distinct(
    record_sequence,
    iso3c
  ) |>
  dplyr::count(
    record_sequence,
    name = "recalculated_country_count"
  )

country_count_check <- record_summary |>
  dplyr::select(
    record_sequence,
    stored_country_count = country_count,
    stored_multiple_countries = multiple_countries
  ) |>
  dplyr::left_join(
    recalculated_counts,
    by = "record_sequence"
  ) |>
  dplyr::mutate(
    recalculated_country_count = dplyr::coalesce(
      recalculated_country_count,
      0L
    ),
    recalculated_multiple_countries =
      recalculated_country_count > 1L,
    count_disagrees =
      stored_country_count != recalculated_country_count,
    flag_disagrees =
      stored_multiple_countries !=
        recalculated_multiple_countries
  )

# Most frequent matched strings ------------------------------------------------

top_matches <- mentions |>
  dplyr::distinct(
    record_sequence,
    normalised_match,
    matched_place,
    country_name,
    iso3c,
    match_type
  ) |>
  dplyr::count(
    normalised_match,
    matched_place,
    country_name,
    iso3c,
    match_type,
    name = "records",
    sort = TRUE
  )

# Names mapped to multiple countries ------------------------------------------

multi_country_names <- mentions |>
  dplyr::filter(
    !is.na(iso3c),
    nzchar(iso3c)
  ) |>
  dplyr::distinct(
    normalised_match,
    matched_place,
    country_name,
    iso3c
  ) |>
  dplyr::group_by(
    normalised_match,
    matched_place
  ) |>
  dplyr::summarise(
    countries = paste(
      sort(unique(country_name)),
      collapse = "; "
    ),
    iso3c = paste(
      sort(unique(iso3c)),
      collapse = "; "
    ),
    country_n = dplyr::n_distinct(iso3c),
    .groups = "drop"
  ) |>
  dplyr::filter(
    country_n > 1L
  ) |>
  dplyr::arrange(
    dplyr::desc(country_n),
    matched_place
  )

# Records that genuinely contain multiple country codes -----------------------

multi_country_records <- mentions |>
  dplyr::filter(
    !is.na(iso3c),
    nzchar(iso3c)
  ) |>
  dplyr::distinct(
    record_sequence,
    record_id,
    country_name,
    iso3c
  ) |>
  dplyr::group_by(
    record_sequence,
    record_id
  ) |>
  dplyr::summarise(
    countries = paste(
      sort(unique(country_name)),
      collapse = "; "
    ),
    iso3c = paste(
      sort(unique(iso3c)),
      collapse = "; "
    ),
    country_count = dplyr::n_distinct(iso3c),
    .groups = "drop"
  ) |>
  dplyr::filter(
    country_count > 1L
  ) |>
  dplyr::arrange(
    dplyr::desc(country_count),
    record_sequence
  )

# Potentially dangerous short or ordinary-language matches --------------------

suspicious_matches <- top_matches |>
  dplyr::filter(
    nchar(normalised_match) <= 5L |
      normalised_match %in% c(
        "spring",
        "field",
        "green",
        "young",
        "black",
        "white",
        "brown",
        "lake",
        "river",
        "union",
        "mobile",
        "normal",
        "central",
        "western",
        "eastern",
        "northern",
        "southern"
      )
  ) |>
  dplyr::arrange(
    dplyr::desc(records),
    normalised_match
  )

# Write audit outputs ----------------------------------------------------------

readr::write_csv(
  country_count_check,
  fs::path(
    output_dir,
    "country_count_check.csv"
  ),
  na = ""
)

readr::write_csv(
  top_matches,
  fs::path(
    output_dir,
    "top_matched_places.csv"
  ),
  na = ""
)

readr::write_csv(
  multi_country_names,
  fs::path(
    output_dir,
    "names_mapping_to_multiple_countries.csv"
  ),
  na = ""
)

readr::write_csv(
  multi_country_records,
  fs::path(
    output_dir,
    "records_with_multiple_countries_recalculated.csv"
  ),
  na = ""
)

readr::write_csv(
  suspicious_matches,
  fs::path(
    output_dir,
    "suspicious_matched_places.csv"
  ),
  na = ""
)

message("Global geography audit completed.")
message(
  "Stored records with multiple countries: ",
  sum(record_summary$multiple_countries %in% TRUE)
)
message(
  "Recalculated records with multiple countries: ",
  nrow(multi_country_records)
)
message(
  "Records with country-count disagreement: ",
  sum(country_count_check$count_disagrees %in% TRUE)
)
message(
  "Names mapped to multiple countries: ",
  nrow(multi_country_names)
)
message(
  "Suspicious matched-name rows: ",
  nrow(suspicious_matches)
)
message("Top 20 matched names:")
print(
  top_matches |>
    dplyr::select(
      matched_place,
      country_name,
      match_type,
      records
    ) |>
    dplyr::slice_head(n = 20L),
  n = 20L
)
