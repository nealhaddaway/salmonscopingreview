# =============================================================================
# File: 17_build_global_country_gazetteer.R
# Project: salmonscopingreview
# Purpose: Build a global place-name gazetteer that maps explicit geographical
#          references to country level
# =============================================================================

source("scripts/00_setup.R")

output_dir <- here::here(
  "outputs",
  "stage_5_geography"
)

cache_dir <- here::here(
  "data_raw",
  "geography_cache"
)

fs::dir_create(output_dir)
fs::dir_create(cache_dir)

# Official GeoNames export files ------------------------------------------------

country_info_url <- paste0(
  "https://download.geonames.org/export/dump/",
  "countryInfo.txt"
)

admin1_url <- paste0(
  "https://download.geonames.org/export/dump/",
  "admin1CodesASCII.txt"
)

cities_url <- paste0(
  "https://download.geonames.org/export/dump/",
  "cities15000.zip"
)

country_info_path <- fs::path(
  cache_dir,
  "countryInfo.txt"
)

admin1_path <- fs::path(
  cache_dir,
  "admin1CodesASCII.txt"
)

cities_zip_path <- fs::path(
  cache_dir,
  "cities15000.zip"
)

cities_txt_path <- fs::path(
  cache_dir,
  "cities15000.txt"
)

download_if_missing <- function(
    url,
    destination
) {

  if (!file.exists(destination)) {

    message(
      "Downloading: ",
      basename(destination)
    )

    utils::download.file(
      url = url,
      destfile = destination,
      mode = "wb",
      quiet = FALSE
    )
  }
}

download_if_missing(
  country_info_url,
  country_info_path
)

download_if_missing(
  admin1_url,
  admin1_path
)

download_if_missing(
  cities_url,
  cities_zip_path
)

if (!file.exists(cities_txt_path)) {

  utils::unzip(
    cities_zip_path,
    files = "cities15000.txt",
    exdir = cache_dir
  )
}

stopifnot(
  file.exists(country_info_path),
  file.exists(admin1_path),
  file.exists(cities_txt_path)
)


# Read a tab-delimited GeoNames export defensively. Some rows contain missing
# trailing fields, so the imported table is padded or truncated to the expected
# schema before names are assigned.
read_geonames_tsv <- function(
    path,
    expected_columns,
    comment = ""
) {

  imported <- readr::read_delim(
    file = path,
    delim = "\t",
    col_names = FALSE,
    col_types = readr::cols(
      .default = readr::col_character()
    ),
    comment = comment,
    quote = "",
    trim_ws = FALSE,
    show_col_types = FALSE,
    progress = FALSE
  )

  if (ncol(imported) < length(expected_columns)) {

    missing_n <- length(expected_columns) - ncol(imported)

    for (i in seq_len(missing_n)) {
      imported[[ncol(imported) + 1L]] <- NA_character_
    }
  }

  if (ncol(imported) > length(expected_columns)) {
    imported <- imported[
      ,
      seq_along(expected_columns),
      drop = FALSE
    ]
  }

  names(imported) <- expected_columns

  imported
}

# Country metadata -------------------------------------------------------------

country_columns <- c(
  "iso2c",
  "iso3c",
  "iso_numeric",
  "fips",
  "country_name",
  "capital",
  "area_km2",
  "population",
  "continent",
  "tld",
  "currency_code",
  "currency_name",
  "phone",
  "postal_code_format",
  "postal_code_regex",
  "languages",
  "geoname_id",
  "neighbours",
  "equivalent_fips"
)

country_info <- read_geonames_tsv(
  path = country_info_path,
  expected_columns = country_columns,
  comment = "#"
) |>
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~ dplyr::na_if(.x, "")
    )
  ) |>
  dplyr::select(
    iso2c,
    iso3c,
    country_name,
    capital,
    geoname_id
  ) |>
  dplyr::filter(
    !is.na(iso2c),
    !is.na(iso3c),
    !is.na(country_name),
    !iso3c %in% c(
      "SCG",  # Serbia and Montenegro: obsolete
      "ANT"   # Netherlands Antilles: obsolete
    )
  ) |>
  dplyr::distinct()

# First-order administrative regions ------------------------------------------

admin1_columns <- c(
  "admin1_key",
  "admin1_name",
  "admin1_ascii",
  "geoname_id"
)

admin1 <- read_geonames_tsv(
  path = admin1_path,
  expected_columns = admin1_columns
) |>
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~ dplyr::na_if(.x, "")
    )
  ) |>
  tidyr::separate_wider_delim(
    admin1_key,
    delim = ".",
    names = c(
      "iso2c",
      "admin1_code"
    ),
    too_many = "merge",
    too_few = "align_start"
  ) |>
  dplyr::left_join(
    country_info |>
      dplyr::select(
        iso2c,
        iso3c,
        country_name
      ),
    by = "iso2c"
  ) |>
  dplyr::filter(
    !is.na(country_name),
    !is.na(admin1_name)
  )

# Global cities and populated places ------------------------------------------

city_columns <- c(
  "geoname_id",
  "name",
  "ascii_name",
  "alternate_names",
  "latitude",
  "longitude",
  "feature_class",
  "feature_code",
  "iso2c",
  "cc2",
  "admin1_code",
  "admin2_code",
  "admin3_code",
  "admin4_code",
  "population",
  "elevation",
  "dem",
  "timezone",
  "modification_date"
)

cities <- read_geonames_tsv(
  path = cities_txt_path,
  expected_columns = city_columns
) |>
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~ dplyr::na_if(.x, "")
    ),
    latitude = suppressWarnings(
      as.numeric(latitude)
    ),
    longitude = suppressWarnings(
      as.numeric(longitude)
    ),
    population = suppressWarnings(
      as.numeric(population)
    )
  ) |>
  dplyr::left_join(
    country_info |>
      dplyr::select(
        iso2c,
        iso3c,
        country_name
      ),
    by = "iso2c"
  ) |>
  dplyr::left_join(
    admin1 |>
      dplyr::select(
        iso2c,
        admin1_code,
        admin1_name
      ) |>
      dplyr::distinct(),
    by = c(
      "iso2c",
      "admin1_code"
    )
  ) |>
  dplyr::filter(
    !is.na(country_name),
    feature_class == "P"
  )

# Retain globally important settlements while limiting false positives.
# GeoNames cities15000 already contains capitals and settlements with
# population above 15,000. We retain:
#   - all national and administrative capitals;
#   - all settlements with population >= 100,000;
#   - settlements >= 25,000 where the name contains at least 6 characters.
cities_filtered <- cities |>
  dplyr::filter(
    feature_code %in% c(
      "PPLC",
      "PPLA",
      "PPLA2",
      "PPLA3",
      "PPLA4"
    ) |
      population >= 100000 |
      (
        population >= 25000 &
          nchar(name) >= 6L
      )
  )

# Gazetteer constructors -------------------------------------------------------

make_rows <- function(
    data,
    place_column,
    match_type,
    priority,
    admin1_column = NULL,
    latitude_column = NULL,
    longitude_column = NULL,
    population_column = NULL,
    source = "GeoNames"
) {

  place <- data[[place_column]]

  admin1_value <- if (is.null(admin1_column)) {
    rep(
      NA_character_,
      nrow(data)
    )
  } else {
    data[[admin1_column]]
  }

  latitude_value <- if (is.null(latitude_column)) {
    rep(
      NA_real_,
      nrow(data)
    )
  } else {
    as.numeric(
      data[[latitude_column]]
    )
  }

  longitude_value <- if (is.null(longitude_column)) {
    rep(
      NA_real_,
      nrow(data)
    )
  } else {
    as.numeric(
      data[[longitude_column]]
    )
  }

  population_value <- if (is.null(population_column)) {
    rep(
      NA_real_,
      nrow(data)
    )
  } else {
    as.numeric(
      data[[population_column]]
    )
  }

  tibble::tibble(
    matched_place = place,
    normalised_match = stringr::str_to_lower(
      stringr::str_squish(place)
    ),
    country_name = data$country_name,
    iso3c = data$iso3c,
    admin1 = admin1_value,
    match_type = match_type,
    latitude = latitude_value,
    longitude = longitude_value,
    population = population_value,
    priority = priority,
    source_dataset = source
  )
}

country_rows <- make_rows(
  data = country_info,
  place_column = "country_name",
  match_type = "country",
  priority = 100L
)

capital_rows <- make_rows(
  data = country_info |>
    dplyr::filter(
      !is.na(capital),
      nzchar(capital)
    ),
  place_column = "capital",
  match_type = "national capital",
  priority = 90L
)

admin1_name_rows <- make_rows(
  data = admin1,
  place_column = "admin1_name",
  match_type = "admin1 region",
  priority = 80L
)

admin1_ascii_rows <- make_rows(
  data = admin1 |>
    dplyr::filter(
      !is.na(admin1_ascii),
      admin1_ascii != admin1_name
    ),
  place_column = "admin1_ascii",
  match_type = "admin1 region variant",
  priority = 79L
)

city_name_rows <- make_rows(
  data = cities_filtered,
  place_column = "name",
  match_type = "populated place",
  priority = 60L,
  admin1_column = "admin1_name",
  latitude_column = "latitude",
  longitude_column = "longitude",
  population_column = "population"
)

city_ascii_rows <- make_rows(
  data = cities_filtered |>
    dplyr::filter(
      !is.na(ascii_name),
      ascii_name != name
    ),
  place_column = "ascii_name",
  match_type = "populated place variant",
  priority = 59L,
  admin1_column = "admin1_name",
  latitude_column = "latitude",
  longitude_column = "longitude",
  population_column = "population"
)

# Preserve the existing project-specific country variants, demonyms and
# recurrent regional terms.
existing_path <- fs::path(
  output_dir,
  "country_gazetteer.csv"
)

existing_rows <- if (file.exists(existing_path)) {

  readr::read_csv(
    existing_path,
    show_col_types = FALSE
  ) |>
    dplyr::transmute(
      matched_place,
      normalised_match,
      country_name,
      iso3c,
      admin1 = NA_character_,
      match_type,
      latitude = NA_real_,
      longitude = NA_real_,
      population = NA_real_,
      priority = dplyr::case_when(
        match_type == "demonym" ~ 95L,
        match_type == "country abbreviation" ~ 95L,
        match_type == "country variant" ~ 95L,
        match_type == "country name" ~ 100L,
        TRUE ~ 85L
      ),
      source_dataset = "Project supplement"
    )

} else {

  tibble::tibble(
    matched_place = character(),
    normalised_match = character(),
    country_name = character(),
    iso3c = character(),
    admin1 = character(),
    match_type = character(),
    latitude = double(),
    longitude = double(),
    population = double(),
    priority = integer(),
    source_dataset = character()
  )
}

global_gazetteer <- dplyr::bind_rows(
  country_rows,
  capital_rows,
  admin1_name_rows,
  admin1_ascii_rows,
  city_name_rows,
  city_ascii_rows,
  existing_rows
) |>
  dplyr::mutate(
    matched_place = stringr::str_squish(
      matched_place
    ),
    normalised_match = stringr::str_to_lower(
      matched_place
    ),
    term_length = nchar(
      matched_place
    )
  ) |>
  dplyr::filter(
    !is.na(matched_place),
    nzchar(matched_place),
    term_length >= 3L
  ) |>
  dplyr::group_by(
    normalised_match
  ) |>
  dplyr::mutate(
    countries_for_name = dplyr::n_distinct(
      iso3c,
      na.rm = TRUE
    ),
    ambiguous = (
      countries_for_name > 1L |
        normalised_match %in% c(
          "georgia",
          "victoria",
          "turkey",
          "jordan",
          "chad",
          "guinea",
          "maine",
          "washington",
          "reading",
          "orange",
          "nice",
          "bath",
          "salmon",
          "trout"
        )
    )
  ) |>
  dplyr::ungroup() |>
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
    .keep_all = TRUE
  ) |>
  dplyr::select(
    matched_place,
    normalised_match,
    country_name,
    iso3c,
    admin1,
    match_type,
    latitude,
    longitude,
    population,
    priority,
    ambiguous,
    countries_for_name,
    term_length,
    source_dataset
  )

# Remove terms that are especially likely to cause false positives in
# scientific abstracts when presented without geographical context.
excluded_generic_terms <- c(
  "reading",
  "orange",
  "nice",
  "bath",
  "salmon",
  "trout",
  "growth",
  "culture",
  "normal",
  "mobile",
  "union",
  "marine",
  "central",
  "western",
  "eastern",
  "northern",
  "southern"
)

global_gazetteer <- global_gazetteer |>
  dplyr::filter(
    !normalised_match %in%
      excluded_generic_terms
  )

readr::write_csv(
  global_gazetteer,
  fs::path(
    output_dir,
    "global_country_gazetteer.csv"
  ),
  na = ""
)

summary_tbl <- tibble::tibble(
  measure = c(
    "Gazetteer rows",
    "Countries represented",
    "Admin1 regions",
    "Populated places",
    "Ambiguous rows",
    "Unique matched names"
  ),
  value = c(
    nrow(global_gazetteer),
    dplyr::n_distinct(
      global_gazetteer$iso3c,
      na.rm = TRUE
    ),
    sum(
      stringr::str_detect(
        global_gazetteer$match_type,
        "^admin1"
      )
    ),
    sum(
      stringr::str_detect(
        global_gazetteer$match_type,
        "^populated place|national capital"
      )
    ),
    sum(
      global_gazetteer$ambiguous
    ),
    dplyr::n_distinct(
      global_gazetteer$normalised_match
    )
  )
)

readr::write_csv(
  summary_tbl,
  fs::path(
    output_dir,
    "global_country_gazetteer_summary.csv"
  ),
  na = ""
)

message("Global country gazetteer written.")
message(
  "Gazetteer rows: ",
  nrow(global_gazetteer)
)
message(
  "Countries represented: ",
  dplyr::n_distinct(
    global_gazetteer$iso3c,
    na.rm = TRUE
  )
)
message(
  "Admin1 regions: ",
  sum(
    stringr::str_detect(
      global_gazetteer$match_type,
      "^admin1"
    )
  )
)
message(
  "Populated places: ",
  sum(
    stringr::str_detect(
      global_gazetteer$match_type,
      "^populated place|national capital"
    )
  )
)
message(
  "Ambiguous rows: ",
  sum(
    global_gazetteer$ambiguous
  )
)
