# =============================================================================
# File: detect_species_mentions.R
# Project: salmonscopingreview
# Purpose: Detect dictionary-defined species mentions in titles and abstracts
# =============================================================================

detect_species_mentions <- function(
    title = NA_character_,
    abstract = NA_character_,
    dictionary
) {
  
  required_columns <- c(
    "species_id",
    "preferred_name",
    "scientific_name",
    "synonym",
    "synonym_type",
    "is_farmed_candidate",
    "default_group"
  )
  
  missing_columns <- setdiff(required_columns, names(dictionary))
  
  if (length(missing_columns) > 0) {
    stop(
      "Dictionary is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  if (length(title) != 1 || length(abstract) != 1) {
    stop("title and abstract must each contain exactly one value.")
  }
  
  empty_result <- data.frame(
    species_id = character(),
    preferred_name = character(),
    scientific_name = character(),
    matched_term = character(),
    synonym_type = character(),
    source = character(),
    match_start = integer(),
    match_end = integer(),
    is_farmed_candidate = logical(),
    default_group = character(),
    stringsAsFactors = FALSE
  )
  
  escape_regex <- function(text) {
    gsub(
      "([][{}()+*^$|\\\\?.])",
      "\\\\\\1",
      text,
      perl = TRUE
    )
  }
  
  find_matches_in_text <- function(text, source_name) {
    
    if (
      is.na(text) ||
      !nzchar(trimws(text))
    ) {
      return(empty_result)
    }
    
    matches <- list()
    match_number <- 0L
    
    for (row_number in seq_len(nrow(dictionary))) {
      
      synonym <- dictionary$synonym[row_number]
      
      if (
        is.na(synonym) ||
        !nzchar(trimws(synonym))
      ) {
        next
      }
      
      escaped_synonym <- escape_regex(trimws(synonym))
      
      pattern <- paste0(
        "(?<![[:alnum:]_])",
        escaped_synonym,
        "(?![[:alnum:]_])"
      )
      
      locations <- gregexpr(
        pattern,
        text,
        ignore.case = TRUE,
        perl = TRUE
      )[[1]]
      
      if (locations[1] == -1) {
        next
      }
      
      lengths <- attr(locations, "match.length")
      
      for (location_number in seq_along(locations)) {
        
        start_position <- locations[location_number]
        match_length <- lengths[location_number]
        end_position <- start_position + match_length - 1L
        
        match_number <- match_number + 1L
        
        matches[[match_number]] <- data.frame(
          species_id = dictionary$species_id[row_number],
          preferred_name = dictionary$preferred_name[row_number],
          scientific_name = dictionary$scientific_name[row_number],
          matched_term = substr(
            text,
            start_position,
            end_position
          ),
          synonym_type = dictionary$synonym_type[row_number],
          source = source_name,
          match_start = start_position,
          match_end = end_position,
          is_farmed_candidate =
            as.logical(dictionary$is_farmed_candidate[row_number]),
          default_group = dictionary$default_group[row_number],
          stringsAsFactors = FALSE
        )
      }
    }
    
    if (length(matches) == 0) {
      return(empty_result)
    }
    
    results <- do.call(rbind, matches)
    
    # Prefer the longest term where dictionary matches overlap.
    # For example, retain "Atlantic salmon" rather than also returning
    # the nested generic term "salmon".
    term_length <- nchar(results$matched_term)
    
    results <- results[
      order(
        results$match_start,
        -term_length,
        results$species_id
      ),
    ]
    
    keep <- rep(TRUE, nrow(results))
    
    if (nrow(results) > 1) {
      
      for (current_row in seq_len(nrow(results))) {
        
        if (!keep[current_row]) {
          next
        }
        
        overlapping_rows <- which(
          seq_len(nrow(results)) != current_row &
            results$match_start <= results$match_end[current_row] &
            results$match_end >= results$match_start[current_row]
        )
        
        if (length(overlapping_rows) > 0) {
          
          shorter_or_equal <- overlapping_rows[
            nchar(results$matched_term[overlapping_rows]) <=
              nchar(results$matched_term[current_row])
          ]
          
          keep[shorter_or_equal] <- FALSE
          keep[current_row] <- TRUE
        }
      }
    }
    
    results <- results[keep, , drop = FALSE]
    
    results <- unique(results)
    
    rownames(results) <- NULL
    
    results
  }
  
  title_matches <- find_matches_in_text(
    text = title,
    source_name = "title"
  )
  
  abstract_matches <- find_matches_in_text(
    text = abstract,
    source_name = "abstract"
  )
  
  results <- rbind(
    title_matches,
    abstract_matches
  )
  
  if (nrow(results) == 0) {
    return(empty_result)
  }
  
  rownames(results) <- NULL
  
  results
}