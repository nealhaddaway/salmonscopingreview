# =============================================================================
# File: validate_species_dictionary.R
# Project: salmonscopingreview
# Purpose: Validate the species dictionary before annotation
# =============================================================================

validate_species_dictionary <- function(path) {
  
  if (!file.exists(path)) {
    stop("Dictionary not found: ", path)
  }
  
  dictionary <- read.csv(
    path,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )
  
  required_columns <- c(
    "species_id",
    "preferred_name",
    "scientific_name",
    "synonym",
    "synonym_type",
    "is_farmed_candidate",
    "default_group",
    "notes"
  )
  
  missing_columns <- setdiff(required_columns, names(dictionary))
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  check_blank <- function(column) {
    
    blank_rows <- which(
      is.na(dictionary[[column]]) |
        trimws(dictionary[[column]]) == ""
    )
    
    if (length(blank_rows) > 0) {
      stop(
        column,
        " contains blank values (rows ",
        paste(blank_rows, collapse = ", "),
        ")."
      )
    }
    
  }
  
  check_blank("species_id")
  check_blank("preferred_name")
  check_blank("synonym")
  
  valid_candidate_values <- c("TRUE", "FALSE")
  
  invalid_candidate <- !dictionary$is_farmed_candidate %in%
    valid_candidate_values
  
  if (any(invalid_candidate)) {
    
    stop(
      "Invalid value(s) in is_farmed_candidate."
    )
    
  }
  
  valid_synonym_types <- c(
    "common",
    "scientific",
    "abbreviation",
    "generic"
  )
  
  invalid_synonyms <-
    !dictionary$synonym_type %in% valid_synonym_types
  
  if (any(invalid_synonyms)) {
    
    stop(
      "Invalid synonym_type detected."
    )
    
  }
  
  duplicate_rows <- duplicated(dictionary)
  
  if (any(duplicate_rows)) {
    
    stop(
      "Duplicate row(s) detected."
    )
    
  }
  
  synonym_lookup <-
    aggregate(
      species_id ~ synonym,
      dictionary,
      function(x) length(unique(x))
    )
  
  duplicated_synonyms <-
    synonym_lookup$synonym[
      synonym_lookup$species_id > 1
    ]
  
  if (length(duplicated_synonyms) > 0) {
    
    stop(
      "The following synonym(s) belong to multiple species:\n",
      paste(duplicated_synonyms, collapse = "\n")
    )
    
  }
  
  preferred_lookup <-
    aggregate(
      preferred_name ~ species_id,
      dictionary,
      function(x) length(unique(x))
    )
  
  if (any(preferred_lookup$preferred_name > 1)) {
    
    stop(
      "Each species_id must have exactly one preferred_name."
    )
    
  }
  
  scientific_lookup <-
    aggregate(
      scientific_name ~ species_id,
      dictionary,
      function(x) length(unique(x))
    )
  
  if (any(scientific_lookup$scientific_name > 1)) {
    
    stop(
      "Each species_id must have exactly one scientific_name."
    )
    
  }
  
  message("✓ Species dictionary validation passed.")
  
  return(dictionary)
  
}