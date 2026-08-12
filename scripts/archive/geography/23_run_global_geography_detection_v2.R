# =============================================================================
# File: 23_run_global_geography_detection_v2.R
# Project: salmonscopingreview
# Purpose: Run the reviewed global geoparser with longest-match precedence
# =============================================================================

source("scripts/00_setup.R")
source("R/read_corpus.R")

required_packages <- c(
  "quanteda",
  "dplyr",
  "readr",
  "stringr",
  "tibble"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0L) {
  stop(
    "Install missing packages first: ",
    paste(missing_packages, collapse = ", ")
  )
}

input_records <- here::here(
  "data_raw",
  "INCLUDES fixed abstracts.txt"
)

input_gazetteer <- here::here(
  "outputs",
  "stage_5_geography",
  "global_country_gazetteer_v2.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_5_geography",
  "global_detection_v2"
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
    return(
      tibble::tibble()
    )
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
    return(
      tibble::tibble()
    )
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
      text = text,
      record_sequence = records$record_sequence,
      record_id = records$record_id,
      source_name = source_name,
      gazetteer_subset = gazetteer_insensitive,
      case_insensitive = TRUE
    ),
    detect_patterns(
      text = text,
      record_sequence = records$record_sequence,
      record_id = records$record_id,
      source_name = source_name,
      gazetteer_subset = gazetteer_sensitive,
      case_insensitive = FALSE
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

# Longest-match precedence:
# Within each record and field, discard a match when its token span is fully
# contained within a longer matched span. Equal spans are retained because one
# text string may intentionally map to more than one country and require review.
overlap_keys <- raw_mentions |>
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
  nrow(overlap_keys)
)

if (nrow(overlap_keys) > 1L) {

  split_indices <- split(
    seq_len(nrow(overlap_keys)),
    interaction(
      overlap_keys$record_sequence,
      overlap_keys$source,
      drop = TRUE
    )
  )

  for (group_indices in split_indices) {

    if (length(group_indices) < 2L) {
      next
    }

    for (current_position in seq_along(group_indices)) {

      current_index <- group_indices[current_position]

      longer_containers <- group_indices[
        overlap_keys$token_start[group_indices] <=
          overlap_keys$token_start[current_index] &
          overlap_keys$token_end[group_indices] >=
            overlap_keys$token_end[current_index] &
          overlap_keys$token_length[group_indices] >
            overlap_keys$token_length[current_index]
      ]

      if (length(longer_containers) > 0L) {
        keep_longest[current_index] <- FALSE
      }
    }
  }
}

retained_spans <- overlap_keys[
  keep_longest,
  c(
    "record_sequence",
    "source",
    "token_start",
    "token_end"
  )
]

geography_mentions <- raw_mentions |>
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
    admin1,
    .keep_all = TRUE
  ) |>
  dplyr::arrange(
    record_sequence,
    source,
    token_start,
    dplyr::desc(priority),
    country_name
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

country_by_record <- geography_mentions |>
  dplyr::filter(
    !is.na(iso3c),
    nzchar(iso3c)
  ) |>
  dplyr::distinct(
    record_sequence,
    record_id,
    country_name,
    iso3c
  )

record_summary <- records |>
  dplyr::select(
    record_sequence,
    record_id,
    title,
    abstract
  ) |>
  dplyr::left_join(
    geography_mentions |>
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
        title_mention = any(
          source == "title"
        ),
        abstract_mention = any(
          source == "abstract"
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
  dplyr::mutate(
    mention_count = dplyr::coalesce(
      mention_count,
      0L
    ),
    country_count = dplyr::coalesce(
      country_count,
      0L
    ),
    ambiguous_mention = dplyr::coalesce(
      ambiguous_mention,
      FALSE
    ),
    title_mention = dplyr::coalesce(
      title_mention,
      FALSE
    ),
    abstract_mention = dplyr::coalesce(
      abstract_mention,
      FALSE
    ),
    multiple_countries = country_count > 1L,
    geography_review_required = (
      ambiguous_mention |
        multiple_countries
    )
  )

review_queue <- geography_mentions |>
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
    country_name
  )

country_frequency <- country_by_record |>
  dplyr::count(
    country_name,
    iso3c,
    name = "records",
    sort = TRUE
  )

top_matches <- geography_mentions |>
  dplyr::distinct(
    record_sequence,
    matched_place,
    country_name,
    iso3c,
    match_type
  ) |>
  dplyr::count(
    matched_place,
    country_name,
    iso3c,
    match_type,
    name = "records",
    sort = TRUE
  )

readr::write_csv(
  geography_mentions,
  fs::path(
    output_dir,
    "global_geography_mentions_v2.csv"
  ),
  na = ""
)

readr::write_csv(
  record_summary,
  fs::path(
    output_dir,
    "global_geography_record_summary_v2.csv"
  ),
  na = ""
)

readr::write_csv(
  review_queue,
  fs::path(
    output_dir,
    "global_geography_review_queue_v2.csv"
  ),
  na = ""
)

readr::write_csv(
  country_frequency,
  fs::path(
    output_dir,
    "global_country_frequency_v2.csv"
  ),
  na = ""
)

readr::write_csv(
  top_matches,
  fs::path(
    output_dir,
    "global_top_matches_v2.csv"
  ),
  na = ""
)

message("Reviewed global geography detection completed.")
message("Mention rows: ", nrow(geography_mentions))
message(
  "Records with geography: ",
  sum(record_summary$mention_count > 0L)
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
  "Countries represented: ",
  dplyr::n_distinct(
    geography_mentions$iso3c,
    na.rm = TRUE
  )
)
message("Top 20 matched names:")
print(
  top_matches |>
    dplyr::slice_head(
      n = 20L
    ),
  n = 20L
)
