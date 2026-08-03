# =============================================================================
# File: 15_build_country_gazetteer.R
# Project: salmonscopingreview
# Purpose: Build a country-level gazetteer for geographical tagging
# =============================================================================

source("scripts/00_setup.R")

required_packages <- c(
  "countrycode",
  "dplyr",
  "readr",
  "stringr",
  "tibble",
  "tidyr"
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

output_dir <- here::here(
  "outputs",
  "stage_5_geography"
)

fs::dir_create(output_dir)

country_base <- countrycode::codelist |>
  dplyr::transmute(
    country_name = country.name.en,
    iso2c = iso2c,
    iso3c = iso3c
  ) |>
  dplyr::filter(
    !is.na(country_name),
    !is.na(iso3c)
  ) |>
  dplyr::distinct()

country_variants <- tibble::tribble(
  ~matched_place, ~country_name, ~iso3c, ~match_type,
  "UK", "United Kingdom", "GBR", "country abbreviation",
  "U.K.", "United Kingdom", "GBR", "country abbreviation",
  "United Kingdom", "United Kingdom", "GBR", "country name",
  "Great Britain", "United Kingdom", "GBR", "country variant",
  "Britain", "United Kingdom", "GBR", "country variant",
  "USA", "United States", "USA", "country abbreviation",
  "U.S.A.", "United States", "USA", "country abbreviation",
  "US", "United States", "USA", "country abbreviation",
  "U.S.", "United States", "USA", "country abbreviation",
  "United States of America", "United States", "USA", "country variant",
  "South Korea", "South Korea", "KOR", "country name",
  "Republic of Korea", "South Korea", "KOR", "country variant",
  "North Korea", "North Korea", "PRK", "country name",
  "Russian Federation", "Russia", "RUS", "country variant",
  "Czech Republic", "Czechia", "CZE", "country variant",
  "Türkiye", "Türkiye", "TUR", "country name",
  "Turkey", "Türkiye", "TUR", "country variant",
  "Viet Nam", "Vietnam", "VNM", "country variant",
  "Iran", "Iran", "IRN", "country name",
  "Islamic Republic of Iran", "Iran", "IRN", "country variant",
  "Bolivia", "Bolivia", "BOL", "country name",
  "Venezuela", "Venezuela", "VEN", "country name",
  "Tanzania", "Tanzania", "TZA", "country name"
)

demonyms <- tibble::tribble(
  ~matched_place, ~country_name, ~iso3c,
  "Norwegian", "Norway", "NOR",
  "Scottish", "United Kingdom", "GBR",
  "English", "United Kingdom", "GBR",
  "Welsh", "United Kingdom", "GBR",
  "British", "United Kingdom", "GBR",
  "Irish", "Ireland", "IRL",
  "Chilean", "Chile", "CHL",
  "Canadian", "Canada", "CAN",
  "American", "United States", "USA",
  "Australian", "Australia", "AUS",
  "New Zealand", "New Zealand", "NZL",
  "New Zealander", "New Zealand", "NZL",
  "Japanese", "Japan", "JPN",
  "Chinese", "China", "CHN",
  "Korean", "South Korea", "KOR",
  "Russian", "Russia", "RUS",
  "Icelandic", "Iceland", "ISL",
  "Faroese", "Faroe Islands", "FRO",
  "Danish", "Denmark", "DNK",
  "Swedish", "Sweden", "SWE",
  "Finnish", "Finland", "FIN",
  "French", "France", "FRA",
  "German", "Germany", "DEU",
  "Spanish", "Spain", "ESP",
  "Portuguese", "Portugal", "PRT",
  "Italian", "Italy", "ITA",
  "Greek", "Greece", "GRC",
  "Turkish", "Türkiye", "TUR",
  "Iranian", "Iran", "IRN",
  "Brazilian", "Brazil", "BRA",
  "Argentine", "Argentina", "ARG",
  "Argentinian", "Argentina", "ARG",
  "South African", "South Africa", "ZAF"
) |>
  dplyr::mutate(
    match_type = "demonym"
  )

regional_places <- tibble::tribble(
  ~matched_place, ~country_name, ~iso3c, ~match_type,
  "Scotland", "United Kingdom", "GBR", "constituent country",
  "England", "United Kingdom", "GBR", "constituent country",
  "Wales", "United Kingdom", "GBR", "constituent country",
  "Northern Ireland", "United Kingdom", "GBR", "constituent country",
  "British Columbia", "Canada", "CAN", "subnational region",
  "Newfoundland", "Canada", "CAN", "subnational region",
  "Newfoundland and Labrador", "Canada", "CAN", "subnational region",
  "Nova Scotia", "Canada", "CAN", "subnational region",
  "New Brunswick", "Canada", "CAN", "subnational region",
  "Quebec", "Canada", "CAN", "subnational region",
  "Tasmania", "Australia", "AUS", "subnational region",
  "Western Australia", "Australia", "AUS", "subnational region",
  "South Australia", "Australia", "AUS", "subnational region",
  "New South Wales", "Australia", "AUS", "subnational region",
  "Victoria", "Australia", "AUS", "subnational region",
  "Washington State", "United States", "USA", "subnational region",
  "Alaska", "United States", "USA", "subnational region",
  "Maine", "United States", "USA", "subnational region",
  "California", "United States", "USA", "subnational region",
  "Faroe Islands", "Faroe Islands", "FRO", "territory",
  "Patagonia", "Chile", "CHL", "cross-border region",
  "Norwegian Sea", "Norway", "NOR", "marine region",
  "North Sea", NA_character_, NA_character_, "transnational marine region",
  "Baltic Sea", NA_character_, NA_character_, "transnational marine region",
  "Irish Sea", NA_character_, NA_character_, "transnational marine region",
  "Mediterranean Sea", NA_character_, NA_character_, "transnational marine region",
  "North Atlantic", NA_character_, NA_character_, "transnational marine region"
)

country_names <- country_base |>
  dplyr::transmute(
    matched_place = country_name,
    country_name,
    iso3c,
    match_type = "country name"
  )

gazetteer <- dplyr::bind_rows(
  country_names,
  country_variants |>
    dplyr::select(
      matched_place,
      country_name,
      iso3c,
      match_type
    ),
  demonyms |>
    dplyr::select(
      matched_place,
      country_name,
      iso3c,
      match_type
    ),
  regional_places |>
    dplyr::select(
      matched_place,
      country_name,
      iso3c,
      match_type
    )
) |>
  dplyr::mutate(
    matched_place = stringr::str_squish(matched_place),
    normalised_match = stringr::str_to_lower(matched_place),
    term_length = nchar(matched_place),
    ambiguous = normalised_match %in% c(
      "georgia",
      "victoria",
      "turkey",
      "jordan",
      "chad",
      "guinea",
      "maine",
      "washington",
      "patagonia",
      "north sea",
      "baltic sea",
      "irish sea",
      "mediterranean sea",
      "north atlantic"
    )
  ) |>
  dplyr::filter(
    !is.na(matched_place),
    nzchar(matched_place)
  ) |>
  dplyr::arrange(
    dplyr::desc(term_length),
    normalised_match,
    country_name
  ) |>
  dplyr::distinct(
    normalised_match,
    country_name,
    iso3c,
    .keep_all = TRUE
  ) |>
  dplyr::select(
    matched_place,
    normalised_match,
    country_name,
    iso3c,
    match_type,
    ambiguous,
    term_length
  )

readr::write_csv(
  gazetteer,
  fs::path(
    output_dir,
    "country_gazetteer.csv"
  ),
  na = ""
)

summary_tbl <- tibble::tibble(
  measure = c(
    "Gazetteer rows",
    "Countries represented",
    "Ambiguous rows",
    "Country names",
    "Demonyms",
    "Subnational and marine regions"
  ),
  value = c(
    nrow(gazetteer),
    dplyr::n_distinct(
      gazetteer$iso3c,
      na.rm = TRUE
    ),
    sum(gazetteer$ambiguous),
    sum(gazetteer$match_type == "country name"),
    sum(gazetteer$match_type == "demonym"),
    sum(
      gazetteer$match_type %in% c(
        "subnational region",
        "constituent country",
        "territory",
        "cross-border region",
        "marine region",
        "transnational marine region"
      )
    )
  )
)

readr::write_csv(
  summary_tbl,
  fs::path(
    output_dir,
    "country_gazetteer_summary.csv"
  ),
  na = ""
)

message("Country gazetteer written.")
message("Gazetteer rows: ", nrow(gazetteer))
message(
  "Countries represented: ",
  dplyr::n_distinct(
    gazetteer$iso3c,
    na.rm = TRUE
  )
)
message(
  "Ambiguous rows: ",
  sum(gazetteer$ambiguous)
)
