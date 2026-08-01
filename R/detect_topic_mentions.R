# =============================================================================
# File: detect_topic_mentions.R
# Project: salmonscopingreview
# Purpose: Detect topic-dictionary components in titles and abstracts
# =============================================================================

detect_topic_mentions <- function(
    title = NA_character_,
    abstract = NA_character_,
    dictionary
) {
  
  required_columns <- c(
    "broad_topic",
    "subtopic",
    "feature",
    "component",
    "term"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(dictionary)
  )
  
  if (length(missing_columns) > 0L) {
    stop(
      "Topic dictionary is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  if (length(title) != 1L || length(abstract) != 1L) {
    stop("title and abstract must each contain exactly one value.")
  }
  
  empty_result <- tibble::tibble(
    dictionary_row = integer(),
    broad_topic = character(),
    subtopic = character(),
    feature = character(),
    component = character(),
    matched_level = character(),
    matched_term = character(),
    source = character(),
    match_start = integer(),
    match_end = integer()
  )
  
  escape_regex <- function(text) {
    gsub(
      "([][{}()+*^$|\\\\?.])",
      "\\\\\\1",
      text,
      perl = TRUE
    )
  }
  
  find_matches <- function(text, source_name) {
    
    if (is.na(text) || !nzchar(trimws(text))) {
      return(empty_result)
    }
    
    matches <- list()
    match_number <- 0L
    
    for (row_number in seq_len(nrow(dictionary))) {
      
      term <- dictionary$term[row_number]
      
      if (
        is.na(term) ||
        !nzchar(trimws(term)) ||
        tolower(trimws(term)) == "general"
      ) {
        next
      }
      
      term <- trimws(term)
      
      pattern <- paste0(
        "(?<![[:alnum:]_])",
        escape_regex(term),
        "(?![[:alnum:]_])"
      )
      
      locations <- gregexpr(
        pattern,
        text,
        ignore.case = TRUE,
        perl = TRUE
      )[[1]]
      
      if (locations[1] == -1L) {
        next
      }
      
      lengths <- attr(locations, "match.length")
      
      for (location_number in seq_along(locations)) {
        
        start_position <- locations[location_number]
        end_position <- (
          start_position +
            lengths[location_number] -
            1L
        )
        
        match_number <- match_number + 1L
        
        matches[[match_number]] <- tibble::tibble(
          dictionary_row = if (
            "dictionary_row" %in% names(dictionary)
          ) {
            dictionary$dictionary_row[row_number]
          } else {
            row_number
          },
          broad_topic = dictionary$broad_topic[row_number],
          subtopic = dictionary$subtopic[row_number],
          feature = dictionary$feature[row_number],
          component = dictionary$component[row_number],
          matched_level = "component",
          matched_term = substr(
            text,
            start_position,
            end_position
          ),
          source = source_name,
          match_start = start_position,
          match_end = end_position
        )
      }
    }
    
    if (length(matches) == 0L) {
      return(empty_result)
    }
    
    dplyr::bind_rows(matches) |>
      dplyr::distinct() |>
      dplyr::arrange(
        match_start,
        dplyr::desc(nchar(matched_term)),
        dictionary_row
      )
  }
  
  title_matches <- find_matches(
    title,
    "title"
  )
  
  abstract_matches <- find_matches(
    abstract,
    "abstract"
  )
  
  dplyr::bind_rows(
    title_matches,
    abstract_matches
  ) |>
    dplyr::distinct()
}