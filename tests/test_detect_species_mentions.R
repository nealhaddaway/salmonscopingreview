# =============================================================================
# File: test_detect_species_mentions.R
# Project: salmonscopingreview
# Purpose: Test dictionary validation and species mention detection
# =============================================================================

source("R/validate_species_dictionary.R")
source("R/detect_species_mentions.R")

species_dictionary <- validate_species_dictionary(
  "dictionary/species_dictionary.csv"
)

# Test 1: common names in the title
result <- detect_species_mentions(
  title = "Effects on Atlantic salmon and brown trout",
  abstract = NA_character_,
  dictionary = species_dictionary
)

stopifnot(nrow(result) == 2)
stopifnot(setequal(
  result$species_id,
  c("SAL_SALAR", "SAL_TRUTTA")
))
stopifnot(all(result$source == "title"))


# Test 2: scientific names in the abstract
result <- detect_species_mentions(
  title = NA_character_,
  abstract = "The experiment included Salmo salar and Salmo trutta.",
  dictionary = species_dictionary
)

stopifnot(nrow(result) == 2)
stopifnot(setequal(
  result$species_id,
  c("SAL_SALAR", "SAL_TRUTTA")
))
stopifnot(all(result$source == "abstract"))


# Test 3: matching is case-insensitive
result <- detect_species_mentions(
  title = "ATLANTIC SALMON production",
  abstract = NA_character_,
  dictionary = species_dictionary
)

stopifnot(nrow(result) == 1)
stopifnot(result$species_id == "SAL_SALAR")


# Test 4: longest overlapping match is retained
result <- detect_species_mentions(
  title = "Atlantic salmon farming",
  abstract = NA_character_,
  dictionary = species_dictionary
)

stopifnot(nrow(result) == 1)
stopifnot(result$species_id == "SAL_SALAR")
stopifnot(result$matched_term == "Atlantic salmon")


# Test 5: generic salmon is detected where no species is specified
result <- detect_species_mentions(
  title = "Environmental effects of salmon farming",
  abstract = NA_character_,
  dictionary = species_dictionary
)

stopifnot(nrow(result) == 1)
stopifnot(result$species_id == "UNSPEC_SALMON")


# Test 6: rainbow trout synonym
result <- detect_species_mentions(
  title = "Health management in steelhead trout",
  abstract = NA_character_,
  dictionary = species_dictionary
)

stopifnot(nrow(result) == 1)
stopifnot(result$species_id == "ONC_MYKISS")


# Test 7: title and abstract matches are both retained
result <- detect_species_mentions(
  title = "Atlantic salmon health",
  abstract = "Brown trout were included as a comparison.",
  dictionary = species_dictionary
)

stopifnot(nrow(result) == 2)
stopifnot(setequal(
  result$source,
  c("title", "abstract")
))


# Test 8: blank title and abstract return an empty result
result <- detect_species_mentions(
  title = NA_character_,
  abstract = "",
  dictionary = species_dictionary
)

stopifnot(nrow(result) == 0)


# Test 9: unrelated text returns an empty result
result <- detect_species_mentions(
  title = "Water quality in coastal ecosystems",
  abstract = "This study measured nitrogen and phosphorus.",
  dictionary = species_dictionary
)

stopifnot(nrow(result) == 0)


# Test 10: invalid vector input produces an error
error_detected <- FALSE

tryCatch(
  {
    detect_species_mentions(
      title = c("Title one", "Title two"),
      abstract = NA_character_,
      dictionary = species_dictionary
    )
  },
  error = function(error) {
    error_detected <<- TRUE
  }
)

stopifnot(error_detected)

message("✓ All species mention tests passed.")