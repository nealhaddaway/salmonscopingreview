# =============================================================================
# File: 25_add_continent_region_layer.R
# Project: salmonscopingreview
# Purpose: Final project geography gazetteer layer. Adds macro-regions and
#          deterministic overrides for known false-positive place mappings.
# =============================================================================

source("scripts/00_setup.R")

input_gazetteer <- here::here(
  "outputs",
  "stage_5_geography",
  "global_country_gazetteer_v2.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_5_geography"
)

stopifnot(
  file.exists(input_gazetteer)
)

gazetteer <- readr::read_csv(
  input_gazetteer,
  show_col_types = FALSE
)

# Remove standalone high-risk regional/directional terms. Longer valid names
# such as South Korea, North Macedonia and New South Wales remain unaffected.
blocked_standalone_terms <- c(
  # "America" alone is too ambiguous for country assignment. Explicit forms
  # such as "United States of America" remain in the gazetteer.
  "america",

  # Compass terms must never be treated as places by themselves. Longer valid
  # names such as North America, South Korea and New South Wales remain.
  "north",
  "south",
  "east",
  "west",
  "northern",
  "southern",
  "eastern",
  "western",
  "northeast",
  "northwest",
  "southeast",
  "southwest"
)

gazetteer <- gazetteer |>
  dplyr::filter(
    !normalised_match %in%
      blocked_standalone_terms
  )

region_terms <- tibble::tribble(
  ~matched_place,       ~region_name,       ~region_type,
  "Europe",             "Europe",           "continent",
  "European",           "Europe",           "continent adjective",
  "Asia",               "Asia",             "continent",
  "Asian",              "Asia",             "continent adjective",
  "Africa",             "Africa",           "continent",
  "African",            "Africa",           "continent adjective",
  "Oceania",            "Oceania",          "continent",
  "Oceanian",           "Oceania",          "continent adjective",
  "Antarctica",         "Antarctica",       "continent",
  "Antarctic",          "Antarctica",       "continent adjective",
  "North America",      "North America",    "macro-region",
  "North American",     "North America",    "macro-region adjective",
  "South America",      "South America",    "macro-region",
  "South American",     "South America",    "macro-region adjective",
  "Central America",    "Central America",  "macro-region",
  "Central American",   "Central America",  "macro-region adjective",
  "Latin America",      "Latin America",    "macro-region",
  "Latin American",     "Latin America",    "macro-region adjective"
) |>
  dplyr::mutate(
    normalised_match = stringr::str_to_lower(
      matched_place
    ),
    country_name = NA_character_,
    iso3c = NA_character_,
    admin1 = NA_character_,
    match_type = region_type,
    latitude = NA_real_,
    longitude = NA_real_,
    population = NA_real_,
    priority = 200L,
    ambiguous = FALSE,
    countries_for_name = 0L,
    term_length = nchar(matched_place),
    source_dataset = "Project region supplement",
    token_count = stringr::str_count(
      matched_place,
      "\\S+"
    ),
    requires_case_match = FALSE,
    review_decision = "project rule"
  ) |>
  dplyr::select(
    dplyr::any_of(
      names(gazetteer)
    ),
    region_name
  )

# Exact project overrides ------------------------------------------------------
#
# These are deliberate disambiguations supported by the salmon-farming corpus.
# They take precedence over GeoNames homonyms. In particular, GeoNames also
# contains a populated place called New Brunswick in the United States, but
# "New Brunswick" in this corpus is the Canadian province.

project_overrides <- tibble::tribble(
  ~matched_place,       ~country_name, ~iso3c, ~admin1,          ~match_type,
  "New Brunswick",      "Canada",       "CAN",  "New Brunswick", "project override"
) |>
  dplyr::mutate(
    normalised_match = stringr::str_to_lower(matched_place),
    latitude = NA_real_,
    longitude = NA_real_,
    population = NA_real_,
    priority = 250L,
    ambiguous = FALSE,
    countries_for_name = 1L,
    term_length = nchar(matched_place),
    source_dataset = "Project exact override",
    token_count = stringr::str_count(matched_place, "\\S+"),
    requires_case_match = FALSE,
    review_decision = "project rule",
    region_name = admin1
  ) |>
  dplyr::select(
    dplyr::any_of(
      c(
        names(gazetteer),
        "region_name"
      )
    )
  )

# Remove lower-priority homonyms for exact project overrides before binding.
gazetteer <- gazetteer |>
  dplyr::filter(
    !normalised_match %in% project_overrides$normalised_match
  )

# Add commonly occurring adjectival forms of subnational regions.
subnational_adjectives <- tibble::tribble(
  ~matched_place,       ~country_name,       ~iso3c, ~admin1,
  "British Columbian",  "Canada",             "CAN",  "British Columbia",
  "Californian",        "United States",      "USA",  "California",
  "Tasmanian",          "Australia",          "AUS",  "Tasmania",
  "Patagonian",         "Chile",              "CHL",  "Patagonia"
) |>
  dplyr::mutate(
    normalised_match = stringr::str_to_lower(
      matched_place
    ),
    match_type = "subnational adjective",
    latitude = NA_real_,
    longitude = NA_real_,
    population = NA_real_,
    priority = 90L,
    ambiguous = FALSE,
    countries_for_name = 1L,
    term_length = nchar(matched_place),
    source_dataset = "Project region supplement",
    token_count = stringr::str_count(
      matched_place,
      "\\S+"
    ),
    requires_case_match = FALSE,
    review_decision = "project rule",
    region_name = admin1
  ) |>
  dplyr::select(
    dplyr::any_of(
      c(
        names(gazetteer),
        "region_name"
      )
    )
  )

gazetteer_v3 <- dplyr::bind_rows(
  gazetteer |>
    dplyr::mutate(
      region_name = dplyr::case_when(
        match_type %in% c(
          "constituent country",
          "subnational region",
          "marine region",
          "transnational marine region",
          "cross-border region",
          "admin1 region",
          "admin1 region variant"
        ) ~ matched_place,
        TRUE ~ NA_character_
      )
    ),
  region_terms,
  project_overrides,
  subnational_adjectives
) |>
  dplyr::arrange(
    dplyr::desc(priority),
    dplyr::desc(term_length),
    normalised_match,
    country_name
  ) |>
  dplyr::distinct(
    normalised_match,
    country_name,
    iso3c,
    region_name,
    .keep_all = TRUE
  )

readr::write_csv(
  gazetteer_v3,
  fs::path(
    output_dir,
    "global_country_gazetteer_v3.csv"
  ),
  na = ""
)

summary_tbl <- tibble::tibble(
  measure = c(
    "Input gazetteer rows",
    "Blocked standalone terms removed",
    "Continent/macro-region rows added",
    "Subnational adjective rows added",
    "Output gazetteer rows"
  ),
  value = c(
    nrow(
      readr::read_csv(
        input_gazetteer,
        show_col_types = FALSE
      )
    ),
    sum(
      readr::read_csv(
        input_gazetteer,
        show_col_types = FALSE
      )$normalised_match %in%
        blocked_standalone_terms
    ),
    nrow(region_terms),
    nrow(subnational_adjectives),
    nrow(gazetteer_v3)
  )
)

readr::write_csv(
  summary_tbl,
  fs::path(
    output_dir,
    "global_country_gazetteer_v3_summary.csv"
  ),
  na = ""
)

message("Geography gazetteer v3 written.")
message("Output gazetteer rows: ", nrow(gazetteer_v3))
message(
  "Continent/macro-region terms: ",
  nrow(region_terms)
)
message(
  "Subnational adjective terms: ",
  nrow(subnational_adjectives)
)
