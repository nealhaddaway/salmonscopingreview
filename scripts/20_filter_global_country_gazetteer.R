# =============================================================================
# File: 20_filter_global_country_gazetteer.R
# Project: salmonscopingreview
# Purpose: Remove high-risk ordinary-language place names from the global
#          gazetteer and create separate strict matching classes
# =============================================================================

source("scripts/00_setup.R")

input_path <- here::here(
  "outputs",
  "stage_5_geography",
  "global_country_gazetteer.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_5_geography"
)

stopifnot(file.exists(input_path))

gazetteer <- readr::read_csv(
  input_path,
  show_col_types = FALSE
)

# High-risk ordinary words observed in the first global run, plus common
# scientific prose words and generic directional terms.
blocked_terms <- c(
  "along", "bay", "can", "central", "east", "eastern", "field",
  "green", "lake", "lice", "male", "marine", "mobile", "much",
  "north", "northern", "normal", "orange", "reading", "river",
  "south", "southern", "spring", "tank", "union", "west",
  "western", "young", "black", "white", "brown", "culture",
  "growth", "fish", "salmon", "trout"
)

safe_match_types <- c(
  "country",
  "country name",
  "country variant",
  "country abbreviation",
  "demonym",
  "constituent country",
  "territory",
  "subnational region",
  "marine region",
  "transnational marine region",
  "cross-border region"
)

filtered <- gazetteer |>
  dplyr::mutate(
    normalised_match = stringr::str_to_lower(
      stringr::str_squish(matched_place)
    ),
    token_count = stringr::str_count(
      matched_place,
      "\\S+"
    ),
    requires_case_match = !match_type %in% safe_match_types
  ) |>
  dplyr::filter(
    !normalised_match %in% blocked_terms
  ) |>
  dplyr::filter(
    dplyr::case_when(
      match_type %in% c("populated place", "populated place variant") ~
        (
          term_length >= 5L &
            (
              population >= 100000 |
                token_count >= 2L
            )
        ),
      stringr::str_detect(match_type, "^admin1") ~
        (
          term_length >= 5L |
            token_count >= 2L
        ),
      TRUE ~ TRUE
    )
  ) |>
  dplyr::arrange(
    dplyr::desc(priority),
    dplyr::desc(term_length),
    normalised_match,
    country_name
  )

readr::write_csv(
  filtered,
  fs::path(
    output_dir,
    "global_country_gazetteer_filtered.csv"
  ),
  na = ""
)

summary_tbl <- tibble::tibble(
  measure = c(
    "Input rows",
    "Filtered rows",
    "Removed rows",
    "Case-insensitive rows",
    "Case-sensitive rows",
    "Ambiguous rows"
  ),
  value = c(
    nrow(gazetteer),
    nrow(filtered),
    nrow(gazetteer) - nrow(filtered),
    sum(!filtered$requires_case_match),
    sum(filtered$requires_case_match),
    sum(filtered$ambiguous)
  )
)

readr::write_csv(
  summary_tbl,
  fs::path(
    output_dir,
    "global_country_gazetteer_filtered_summary.csv"
  ),
  na = ""
)

message("Filtered global gazetteer written.")
message("Input rows: ", nrow(gazetteer))
message("Filtered rows: ", nrow(filtered))
message("Removed rows: ", nrow(gazetteer) - nrow(filtered))
message(
  "Case-sensitive rows: ",
  sum(filtered$requires_case_match)
)
