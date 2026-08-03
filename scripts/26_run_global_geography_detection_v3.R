# =============================================================================
# File: 26_run_global_geography_detection_v3.R
# Project: salmonscopingreview
# Purpose: Run country plus continent/macro-region annotation using longest
#          match precedence
# =============================================================================

source("scripts/00_setup.R")
source("R/read_corpus.R")

input_records <- here::here(
  "data_raw",
  "INCLUDES fixed abstracts.txt"
)

input_gazetteer <- here::here(
  "outputs",
  "stage_5_geography",
  "global_country_gazetteer_v3.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_5_geography",
  "global_detection_v3"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(input_records),
  file.exists(input_gazetteer)
)

records <- read_corpus(input_records)

gazetteer <- readr::read_csv(
  input_gazetteer,
  show_col_types = FALSE
)

gazetteer_insensitive <- gazetteer |>
  dplyr::filter(
    !requires_case_match
  )

gazetteer_sensitive <- gazetteer |>
  dplyr::filter(
    requires_case_match
  )

detect_patterns <- function(
    text,
    record_sequence,
    record_id,
    source_name,
    gazetteer_subset,
    case_insensitive
) {

  if (nrow(gazetteer_subset) == 0L) {
    return(tibble::tibble())
  }

  patterns <- gazetteer_subset |>
    dplyr::distinct(
      normalised_match,
      .keep_all = TRUE
    ) |>
    dplyr::arrange(
      dplyr::desc(term_length),
      dplyr::desc(priority)
    ) |>
    dplyr::pull(matched_place)

  corpus <- quanteda::corpus(
    dplyr::coalesce(
      as.character(text),
      ""
    ),
    docnames = as.character(
      record_sequence
    )
  )

  tokens <- quanteda::tokens(
    corpus,
    remove_punct = FALSE,
    remove_symbols = FALSE,
    remove_numbers = FALSE,
    remove_separators = TRUE,
    split_hyphens = FALSE
  )

  hits <- quanteda::kwic(
    tokens,
    pattern = quanteda::phrase(patterns),
    window = 12L,
    valuetype = "fixed",
    case_insensitive = case_insensitive
  )

  if (nrow(hits) == 0L) {
    return(tibble::tibble())
  }

  hits |>
    tibble::as_tibble() |>
    dplyr::transmute(
      record_sequence = as.integer(docname),
      source = source_name,
      token_start = from,
      token_end = to,
      token_length = to - from + 1L,
      matched_text = keyword,
      normalised_match = stringr::str_to_lower(
        stringr::str_squish(keyword)
      ),
      context = stringr::str_squish(
        paste(
          pre,
          keyword,
          post
        )
      )
    ) |>
    dplyr::left_join(
      tibble::tibble(
        record_sequence = record_sequence,
        record_id = record_id
      ),
      by = "record_sequence"
    ) |>
    dplyr::left_join(
      gazetteer_subset,
      by = "normalised_match",
      relationship = "many-to-many"
    )
}

run_field <- function(
    text,
    source_name
) {
  dplyr::bind_rows(
    detect_patterns(
      text,
      records$record_sequence,
      records$record_id,
      source_name,
      gazetteer_insensitive,
      TRUE
    ),
    detect_patterns(
      text,
      records$record_sequence,
      records$record_id,
      source_name,
      gazetteer_sensitive,
      FALSE
    )
  )
}

raw_mentions <- dplyr::bind_rows(
  run_field(
    records$title,
    "title"
  ),
  run_field(
    records$abstract,
    "abstract"
  )
) |>
  dplyr::filter(
    !is.na(matched_place)
  )

# Longest-match precedence removes "American" when it is contained in
# "Latin American", "South American", "North American" or "Central American".
span_keys <- raw_mentions |>
  dplyr::distinct(
    record_sequence,
    source,
    token_start,
    token_end,
    token_length
  ) |>
  dplyr::arrange(
    record_sequence,
    source,
    token_start,
    dplyr::desc(token_length)
  )

keep_longest <- rep(
  TRUE,
  nrow(span_keys)
)

split_indices <- split(
  seq_len(nrow(span_keys)),
  interaction(
    span_keys$record_sequence,
    span_keys$source,
    drop = TRUE
  )
)

for (group_indices in split_indices) {

  if (length(group_indices) < 2L) {
    next
  }

  for (current_index in group_indices) {

    longer_containers <- group_indices[
      span_keys$token_start[group_indices] <=
        span_keys$token_start[current_index] &
        span_keys$token_end[group_indices] >=
          span_keys$token_end[current_index] &
        span_keys$token_length[group_indices] >
          span_keys$token_length[current_index]
    ]

    if (length(longer_containers) > 0L) {
      keep_longest[current_index] <- FALSE
    }
  }
}

retained_spans <- span_keys[
  keep_longest,
  c(
    "record_sequence",
    "source",
    "token_start",
    "token_end"
  )
]

mentions <- raw_mentions |>
  dplyr::semi_join(
    retained_spans,
    by = c(
      "record_sequence",
      "source",
      "token_start",
      "token_end"
    )
  ) |>
  dplyr::distinct(
    record_sequence,
    record_id,
    source,
    token_start,
    token_end,
    normalised_match,
    country_name,
    iso3c,
    region_name,
    .keep_all = TRUE
  ) |>
  dplyr::arrange(
    record_sequence,
    source,
    token_start,
    dplyr::desc(priority),
    country_name,
    region_name
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

country_by_record <- mentions |>
  dplyr::filter(
    !is.na(iso3c),
    nzchar(iso3c)
  ) |>
  dplyr::distinct(
    record_sequence,
    record_id,
    iso3c,
    country_name
  ) |>
  dplyr::group_by(
    record_sequence,
    record_id,
    iso3c
  ) |>
  dplyr::summarise(
    country_name = dplyr::first(
      country_name
    ),
    .groups = "drop"
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
        countries_mentioned = collapse_unique(
          country_name
        ),
        iso3c = collapse_unique(
          iso3c
        ),
        country_count = dplyr::n_distinct(
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
        regions_mentioned = collapse_unique(
          region_name
        ),
        region_count = dplyr::n_distinct(
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

readr::write_csv(
  mentions,
  fs::path(
    output_dir,
    "global_geography_mentions_v3.csv"
  ),
  na = ""
)

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

top_matches <- mentions |>
  dplyr::distinct(
    record_sequence,
    matched_place,
    country_name,
    iso3c,
    region_name,
    match_type
  ) |>
  dplyr::count(
    matched_place,
    country_name,
    iso3c,
    region_name,
    match_type,
    name = "records",
    sort = TRUE
  )

readr::write_csv(
  top_matches,
  fs::path(
    output_dir,
    "global_top_matches_v3.csv"
  ),
  na = ""
)

message("Geography detection v3 completed.")
message("Mention rows: ", nrow(mentions))
message(
  "Records with countries: ",
  dplyr::n_distinct(
    country_by_record$record_sequence
  )
)
message(
  "Records with regions/continents: ",
  dplyr::n_distinct(
    region_by_record$record_sequence
  )
)
message(
  "Records with multiple countries: ",
  sum(record_summary$multiple_countries)
)
message(
  "Records requiring review: ",
  sum(record_summary$geography_review_required)
)
message("Top 20 matched names:")
print(
  top_matches |>
    dplyr::slice_head(
      n = 20L
    ),
  n = 20L
)
