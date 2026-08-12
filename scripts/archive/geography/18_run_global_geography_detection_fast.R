# =============================================================================
# File: 18_run_global_geography_detection_fast.R
# Project: salmonscopingreview
# Purpose: Detect global geographical references efficiently with quanteda
# =============================================================================

source("scripts/00_setup.R")
source("R/read_corpus.R")

required_packages <- c(
  "quanteda",
  "dplyr",
  "readr",
  "stringr",
  "tibble",
  "tidyr",
  "purrr"
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
  "global_country_gazetteer.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_5_geography",
  "global_detection"
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
) |>
  dplyr::filter(
    !is.na(matched_place),
    nzchar(matched_place),
    !is.na(normalised_match),
    nzchar(normalised_match)
  ) |>
  dplyr::arrange(
    dplyr::desc(priority),
    dplyr::desc(term_length)
  )

patterns <- gazetteer |>
  dplyr::distinct(
    normalised_match,
    .keep_all = TRUE
  ) |>
  dplyr::pull(matched_place)

message("Corpus records: ", nrow(records))
message("Gazetteer rows: ", nrow(gazetteer))
message("Unique detection patterns: ", length(patterns))

detect_in_field <- function(
    text,
    record_sequence,
    record_id,
    source_name,
    patterns
) {

  text <- dplyr::coalesce(
    as.character(text),
    ""
  )

  corpus <- quanteda::corpus(
    text,
    docnames = as.character(record_sequence)
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
    case_insensitive = TRUE
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
    )
}

title_hits <- detect_in_field(
  text = records$title,
  record_sequence = records$record_sequence,
  record_id = records$record_id,
  source_name = "title",
  patterns = patterns
)

abstract_hits <- detect_in_field(
  text = records$abstract,
  record_sequence = records$record_sequence,
  record_id = records$record_id,
  source_name = "abstract",
  patterns = patterns
)

raw_hits <- dplyr::bind_rows(
  title_hits,
  abstract_hits
)

message("Raw detected mention rows: ", nrow(raw_hits))

geography_mentions <- raw_hits |>
  dplyr::left_join(
    gazetteer,
    by = "normalised_match"
  ) |>
  dplyr::filter(
    !is.na(matched_place)
  ) |>
  dplyr::mutate(
    token_length = token_end - token_start + 1L
  ) |>
  dplyr::group_by(
    record_sequence,
    source,
    token_start
  ) |>
  dplyr::filter(
    token_length == max(token_length, na.rm = TRUE)
  ) |>
  dplyr::ungroup() |>
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
        countries_mentioned = collapse_unique(
          country_name
        ),
        iso3c = collapse_unique(
          iso3c
        ),
        mention_count = dplyr::n(),
        country_count = dplyr::n_distinct(
          iso3c,
          na.rm = TRUE
        ),
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

country_frequency <- geography_mentions |>
  dplyr::filter(
    !is.na(country_name),
    !is.na(iso3c)
  ) |>
  dplyr::distinct(
    record_sequence,
    country_name,
    iso3c
  ) |>
  dplyr::count(
    country_name,
    iso3c,
    name = "records",
    sort = TRUE
  )

readr::write_csv(
  geography_mentions,
  fs::path(
    output_dir,
    "global_geography_mentions.csv"
  ),
  na = ""
)

readr::write_csv(
  record_summary,
  fs::path(
    output_dir,
    "global_geography_record_summary.csv"
  ),
  na = ""
)

readr::write_csv(
  review_queue,
  fs::path(
    output_dir,
    "global_geography_review_queue.csv"
  ),
  na = ""
)

readr::write_csv(
  country_frequency,
  fs::path(
    output_dir,
    "global_country_frequency.csv"
  ),
  na = ""
)

message("Global geography detection completed.")
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
