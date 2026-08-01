source("scripts/00_setup.R")
source("R/detect_topic_mentions.R")

topic_dictionary <- readr::read_csv(
  here::here(
    "outputs",
    "stage_3_topics",
    "topic_dictionary_clean.csv"
  ),
  show_col_types = FALSE
)

# Select dictionary terms that the detector is intended to search
excluded_components <- c(
  "Atlantic salmon",
  "Rainbow trout",
  "Chinook salmon",
  "Coho salmon",
  "Sockeye salmon",
  "Pink salmon",
  "Chum salmon",
  "Masu salmon",
  "Species"
)

testable_terms <- topic_dictionary |>
  dplyr::filter(
    !is.na(term),
    nzchar(stringr::str_squish(term)),
    stringr::str_to_lower(stringr::str_squish(term)) != "general",
    !stringr::str_to_lower(component) %in%
      stringr::str_to_lower(excluded_components),
    stringr::str_to_lower(component) !=
      stringr::str_to_lower(broad_topic),
    stringr::str_to_lower(component) !=
      stringr::str_to_lower(subtopic),
    stringr::str_to_lower(component) !=
      stringr::str_to_lower(feature)
  ) |>
  dplyr::distinct(term)

test_term_1 <- testable_terms |>
  dplyr::slice(1) |>
  dplyr::pull(term)

test_term_2 <- testable_terms |>
  dplyr::slice(2) |>
  dplyr::pull(term)

result_2 <- detect_topic_mentions(
  title = NA_character_,
  abstract = paste(
    "The study assessed",
    test_term_2,
    "in aquaculture."
  ),
  dictionary = topic_dictionary
)

stopifnot(
  nrow(result_2) >= 1L,
  any(
    stringr::str_to_lower(result_2$matched_term) ==
      stringr::str_to_lower(test_term_2)
  ),
  any(result_2$source == "abstract")
)

# Test 3: matching is case-insensitive
result_3 <- detect_topic_mentions(
  title = stringr::str_to_upper(test_term_1),
  abstract = NA_character_,
  dictionary = topic_dictionary
)

stopifnot(
  any(
    stringr::str_to_lower(result_3$matched_term) ==
      stringr::str_to_lower(test_term_1)
  )
)

# Test 4: generic hierarchy label "General" is not matched
result_4 <- detect_topic_mentions(
  title = "General observations on salmon farming",
  abstract = NA_character_,
  dictionary = topic_dictionary
)

stopifnot(
  !any(
    tolower(result_4$matched_term) ==
      "general"
  )
)

# Test 5: no match returns an empty table with expected columns
result_5 <- detect_topic_mentions(
  title = "Unrelated geological study",
  abstract = "The paper describes volcanic rock formations.",
  dictionary = topic_dictionary
)

expected_columns <- c(
  "dictionary_row",
  "broad_topic",
  "subtopic",
  "feature",
  "component",
  "matched_level",
  "matched_term",
  "source",
  "match_start",
  "match_end"
)

stopifnot(
  nrow(result_5) == 0L,
  identical(
    names(result_5),
    expected_columns
  )
)

# Test 6: missing dictionary columns produces an error
invalid_dictionary <- topic_dictionary |>
  dplyr::select(
    -component
  )

error_6 <- try(
  detect_topic_mentions(
    title = "Stocking density",
    abstract = NA_character_,
    dictionary = invalid_dictionary
  ),
  silent = TRUE
)

stopifnot(
  inherits(error_6, "try-error")
)

message("All detect_topic_mentions() tests passed.")