# =============================================================================
# File: 16_run_geography_detection.R
# Project: salmonscopingreview
# Purpose: Detect geographical mentions across the full corpus
# =============================================================================

source("scripts/00_setup.R")
source("R/read_corpus.R")
source("R/detect_geography_mentions.R")

# Inputs ----------------------------------------------------------------------

input_records <- here::here(
  "data_raw",
  "INCLUDES fixed abstracts.txt"
)

input_gazetteer <- here::here(
  "outputs",
  "stage_5_geography",
  "country_gazetteer.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_5_geography"
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

# Detect geography -------------------------------------------------------------

detect_one_record <- function(
    record_sequence,
    record_id,
    title,
    abstract
) {
  
  mentions <- detect_geography_mentions(
    title = title,
    abstract = abstract,
    gazetteer = gazetteer
  )
  
  if (nrow(mentions) == 0L) {
    return(
      tibble::tibble()
    )
  }
  
  mentions |>
    dplyr::mutate(
      record_sequence = record_sequence,
      record_id = record_id,
      .before = 1
    )
}

geography_mentions <- purrr::pmap_dfr(
  records |>
    dplyr::select(
      record_sequence,
      record_id,
      title,
      abstract
    ),
  detect_one_record,
  .progress = TRUE
)

# Record-level summary ---------------------------------------------------------

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

geography_record_summary <- records |>
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
    geography_review_required = ambiguous_mention
  )

# Ambiguity review queue -------------------------------------------------------

geography_review_queue <- geography_mentions |>
  dplyr::filter(
    ambiguous %in% TRUE
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
    match_start
  )

# Outputs ---------------------------------------------------------------------

readr::write_csv(
  geography_mentions,
  fs::path(
    output_dir,
    "geography_mentions.csv"
  ),
  na = ""
)

readr::write_csv(
  geography_record_summary,
  fs::path(
    output_dir,
    "geography_record_summary.csv"
  ),
  na = ""
)

readr::write_csv(
  geography_review_queue,
  fs::path(
    output_dir,
    "geography_review_queue.csv"
  ),
  na = ""
)

country_frequency <- geography_mentions |>
  dplyr::filter(
    !is.na(country_name)
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
  country_frequency,
  fs::path(
    output_dir,
    "country_frequency.csv"
  ),
  na = ""
)

message("Geography detection completed.")
message("Corpus records: ", nrow(records))
message("Mention rows: ", nrow(geography_mentions))
message(
  "Records with geography: ",
  sum(geography_record_summary$mention_count > 0L)
)
message(
  "Records requiring review: ",
  sum(geography_record_summary$geography_review_required)
)
message(
  "Countries represented: ",
  dplyr::n_distinct(
    geography_mentions$iso3c,
    na.rm = TRUE
  )
)