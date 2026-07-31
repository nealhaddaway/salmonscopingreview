source("R/filter_species_mentions.R")

make_mentions <- function(
    preferred_name,
    scientific_name,
    matched_term,
    source,
    match_start,
    match_end,
    is_farmed_candidate,
    species_id = preferred_name,
    synonym_type = "test"
) {
  
  data.frame(
    species_id = species_id,
    preferred_name = preferred_name,
    scientific_name = scientific_name,
    matched_term = matched_term,
    synonym_type = synonym_type,
    source = source,
    match_start = match_start,
    match_end = match_end,
    is_farmed_candidate = is_farmed_candidate,
    stringsAsFactors = FALSE
  )
}

# Test 1: explicitly wild Atlantic salmon is excluded
title_1 <- "Effects on wild Atlantic salmon populations"

mentions_1 <- make_mentions(
  preferred_name = "Atlantic salmon",
  scientific_name = "Salmo salar",
  matched_term = "Atlantic salmon",
  source = "title",
  match_start = 17L,
  match_end = 31L,
  is_farmed_candidate = TRUE
)

result_1 <- filter_species_mentions(
  mentions = mentions_1,
  title = title_1,
  abstract = NA_character_
)

stopifnot(
  identical(result_1$mention_eligible, FALSE),
  identical(
    result_1$filter_reason,
    "Species explicitly identified as wild"
  )
)

# Test 2: an ordinary Atlantic salmon mention is retained
title_2 <- "Growth performance of Atlantic salmon in sea cages"

mentions_2 <- make_mentions(
  preferred_name = "Atlantic salmon",
  scientific_name = "Salmo salar",
  matched_term = "Atlantic salmon",
  source = "title",
  match_start = 23L,
  match_end = 37L,
  is_farmed_candidate = TRUE
)

result_2 <- filter_species_mentions(
  mentions = mentions_2,
  title = title_2,
  abstract = NA_character_
)

stopifnot(
  identical(result_2$mention_eligible, TRUE),
  is.na(result_2$filter_reason)
)

# Test 3: explicitly farmed and wild wording is not automatically excluded
abstract_3 <- paste(
  "We compared farmed Atlantic salmon with wild Atlantic salmon."
)

mentions_3 <- rbind(
  make_mentions(
    preferred_name = "Atlantic salmon",
    scientific_name = "Salmo salar",
    matched_term = "Atlantic salmon",
    source = "abstract",
    match_start = 20L,
    match_end = 34L,
    is_farmed_candidate = TRUE
  ),
  make_mentions(
    preferred_name = "Atlantic salmon",
    scientific_name = "Salmo salar",
    matched_term = "Atlantic salmon",
    source = "abstract",
    match_start = 46L,
    match_end = 60L,
    is_farmed_candidate = TRUE
  )
)

result_3 <- filter_species_mentions(
  mentions = mentions_3,
  title = NA_character_,
  abstract = abstract_3
)

stopifnot(
  result_3$mention_eligible[1],
  result_3$mention_eligible[2]
)

# Test 4: generic salmon near non-target Salmo trutta is excluded
abstract_4 <- paste(
  "Farmed brown trout (Salmo trutta) are commonly marketed as salmon."
)

mentions_4 <- rbind(
  make_mentions(
    preferred_name = "Brown trout",
    scientific_name = "Salmo trutta",
    matched_term = "Salmo trutta",
    source = "abstract",
    match_start = 21L,
    match_end = 31L,
    is_farmed_candidate = FALSE
  ),
  make_mentions(
    preferred_name = "Unspecified farmed salmon",
    scientific_name = NA_character_,
    matched_term = "salmon",
    source = "abstract",
    match_start = 58L,
    match_end = 63L,
    is_farmed_candidate = TRUE,
    species_id = "unspecified_farmed_salmon"
  )
)

result_4 <- filter_species_mentions(
  mentions = mentions_4,
  title = NA_character_,
  abstract = abstract_4
)

stopifnot(
  result_4$mention_eligible[1],
  !result_4$mention_eligible[2],
  identical(
    result_4$filter_reason[2],
    paste0(
      "Generic salmon mention located near a named ",
      "non-target Salmo species"
    )
  )
)

# Test 5: a distant generic salmon reference is retained
abstract_5 <- paste0(
  "Salmo trutta was examined in a freshwater experiment. ",
  paste(rep("Additional ecological context was provided.", 8), collapse = " "),
  " The paper also discusses salmon farming in another region."
)

salmo_start_5 <- regexpr(
  "Salmo trutta",
  abstract_5,
  fixed = TRUE
)[1]

salmon_start_5 <- regexpr(
  "salmon farming",
  abstract_5,
  fixed = TRUE
)[1]

mentions_5 <- rbind(
  make_mentions(
    preferred_name = "Brown trout",
    scientific_name = "Salmo trutta",
    matched_term = "Salmo trutta",
    source = "abstract",
    match_start = salmo_start_5,
    match_end = salmo_start_5 + nchar("Salmo trutta") - 1L,
    is_farmed_candidate = FALSE
  ),
  make_mentions(
    preferred_name = "Unspecified farmed salmon",
    scientific_name = NA_character_,
    matched_term = "salmon",
    source = "abstract",
    match_start = salmon_start_5,
    match_end = salmon_start_5 + nchar("salmon") - 1L,
    is_farmed_candidate = TRUE,
    species_id = "unspecified_farmed_salmon"
  )
)

result_5 <- filter_species_mentions(
  mentions = mentions_5,
  title = NA_character_,
  abstract = abstract_5
)

stopifnot(
  result_5$mention_eligible[1],
  result_5$mention_eligible[2]
)

# Test 6: generic salmon and non-target species in different fields are retained
mentions_6 <- rbind(
  make_mentions(
    preferred_name = "Brown trout",
    scientific_name = "Salmo marmoratus",
    matched_term = "Salmo marmoratus",
    source = "title",
    match_start = 12L,
    match_end = 28L,
    is_farmed_candidate = FALSE
  ),
  make_mentions(
    preferred_name = "Unspecified farmed salmon",
    scientific_name = NA_character_,
    matched_term = "salmon",
    source = "abstract",
    match_start = 30L,
    match_end = 35L,
    is_farmed_candidate = TRUE,
    species_id = "unspecified_farmed_salmon"
  )
)

result_6 <- filter_species_mentions(
  mentions = mentions_6,
  title = "Ecology of Salmo marmoratus",
  abstract = "A separate industry section discusses salmon production."
)

stopifnot(
  result_6$mention_eligible[1],
  result_6$mention_eligible[2]
)

# Test 7: empty mentions are handled safely
empty_mentions <- mentions_1[0, ]

result_7 <- filter_species_mentions(
  mentions = empty_mentions,
  title = "",
  abstract = ""
)

stopifnot(
  nrow(result_7) == 0L,
  "mention_eligible" %in% names(result_7),
  "filter_reason" %in% names(result_7)
)

# Test 8: missing required columns produce an error
invalid_mentions <- data.frame(
  species_id = "atlantic_salmon"
)

error_8 <- try(
  filter_species_mentions(
    mentions = invalid_mentions,
    title = "",
    abstract = ""
  ),
  silent = TRUE
)

stopifnot(
  inherits(error_8, "try-error")
)

message("All filter_species_mentions() tests passed.")