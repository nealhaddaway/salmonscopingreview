# =============================================================================
# File: test_assign_farmed_species.R
# Project: salmonscopingreview
# Purpose: Test farmed-species assignment
# =============================================================================

source("R/validate_species_dictionary.R")
source("R/detect_species_mentions.R")
source("R/assign_farmed_species.R")

species_dictionary <- validate_species_dictionary(
  "dictionary/species_dictionary.csv"
)

# Test 1: one eligible farmed species
mentions <- detect_species_mentions(
  title = "Atlantic salmon production",
  abstract = NA_character_,
  dictionary = species_dictionary
)

result <- assign_farmed_species(mentions)

stopifnot(nrow(result) == 1)
stopifnot(result$farmed_species_id == "SAL_SALAR")
stopifnot(result$assignment_role == "primary")
stopifnot(result$review_required == FALSE)


# Test 2: farmed species plus non-target species
mentions <- detect_species_mentions(
  title = "Effects on Atlantic salmon and brown trout",
  abstract = NA_character_,
  dictionary = species_dictionary
)

result <- assign_farmed_species(mentions)

stopifnot(nrow(result) == 1)
stopifnot(result$farmed_species_id == "SAL_SALAR")
stopifnot(result$non_target_species == "Brown trout")


# Test 3: two eligible farmed species
mentions <- detect_species_mentions(
  title = "Atlantic salmon and rainbow trout production",
  abstract = NA_character_,
  dictionary = species_dictionary
)

result <- assign_farmed_species(mentions)

stopifnot(nrow(result) == 2)
stopifnot(setequal(
  result$farmed_species_id,
  c("SAL_SALAR", "ONC_MYKISS")
))
stopifnot(all(result$assignment_role == "co-primary"))
stopifnot(all(result$review_required == FALSE))


# Test 4: generic salmon only
mentions <- detect_species_mentions(
  title = "Environmental impacts of salmon farming",
  abstract = NA_character_,
  dictionary = species_dictionary
)

result <- assign_farmed_species(mentions)

stopifnot(nrow(result) == 1)
stopifnot(result$farmed_species_id == "UNSPEC_SALMON")
stopifnot(result$assignment_role == "primary")


# Test 5: specific salmon overrides generic salmon
mentions <- detect_species_mentions(
  title = "Atlantic salmon farming",
  abstract = "Salmon production has expanded rapidly.",
  dictionary = species_dictionary
)

result <- assign_farmed_species(mentions)

stopifnot(nrow(result) == 1)
stopifnot(result$farmed_species_id == "SAL_SALAR")


# Test 6: only non-target species
mentions <- detect_species_mentions(
  title = "Brown trout ecology",
  abstract = NA_character_,
  dictionary = species_dictionary
)

result <- assign_farmed_species(mentions)

stopifnot(nrow(result) == 1)
stopifnot(is.na(result$farmed_species_id))
stopifnot(result$review_required == TRUE)
stopifnot(result$assignment_reason == "Only non-target species detected")
stopifnot(result$non_target_species == "Brown trout")


# Test 7: no species detected
mentions <- detect_species_mentions(
  title = "Coastal water quality",
  abstract = "Nitrogen and phosphorus were measured.",
  dictionary = species_dictionary
)

result <- assign_farmed_species(mentions)

stopifnot(nrow(result) == 1)
stopifnot(is.na(result$farmed_species_id))
stopifnot(result$review_required == TRUE)
stopifnot(result$assignment_reason == "No eligible farmed species detected")


# Test 8: duplicate mentions do not duplicate assignments
mentions <- detect_species_mentions(
  title = "Atlantic salmon health",
  abstract = "Atlantic salmon were sampled repeatedly.",
  dictionary = species_dictionary
)

result <- assign_farmed_species(mentions)

stopifnot(nrow(result) == 1)
stopifnot(result$farmed_species_id == "SAL_SALAR")


# Test 9: missing required columns produces an error
invalid_mentions <- data.frame(
  species_id = "SAL_SALAR",
  preferred_name = "Atlantic salmon",
  stringsAsFactors = FALSE
)

error_detected <- FALSE

tryCatch(
  {
    assign_farmed_species(invalid_mentions)
  },
  error = function(error) {
    error_detected <<- TRUE
  }
)

stopifnot(error_detected)


# Test 10: non-target names are collapsed and sorted
mentions <- detect_species_mentions(
  title = "Atlantic salmon, brown trout and Arctic char",
  abstract = NA_character_,
  dictionary = species_dictionary
)

result <- assign_farmed_species(mentions)

stopifnot(nrow(result) == 1)
stopifnot(
  result$non_target_species == "Arctic char; Brown trout"
)

message("✓ All farmed-species assignment tests passed.")