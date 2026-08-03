# =============================================================================
# File: detect_geography_mentions.R
# Project: salmonscopingreview
# Purpose: Detect gazetteer-defined geographical mentions in titles/abstracts
# =============================================================================

detect_geography_mentions <- function(
    title = NA_character_,
    abstract = NA_character_,
    gazetteer
) {
  
  required_columns <- c(
    "matched_place",
    "normalised_match",
    "country_name",
    "iso3c",
    "match_type",
    "ambiguous",
    "term_length"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(gazetteer)
  )
  
  if (length(missing_columns) > 0L) {
    stop(
      "Gazetteer is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  if (length(title) != 1L || length(abstract) != 1L) {
    stop("title and abstract must each contain exactly one value.")
  }
  
  empty_result <- tibble::tibble(
    matched_place = character(),
    matched_text = character(),
    country_name = character(),
    iso3c = character(),
    match_type = character(),
    ambiguous = logical(),
    source = character(),
    match_start = integer(),
    match_end = integer(),
    context = character()
  )
  
  escape_regex <- function(text) {
    gsub(
      "([][{}()+*^$|\\\\?.])",
      "\\\\\\1",
      text,
      perl = TRUE
    )
  }
  
  extract_context <- function(
    text,
    start_position,
    end_position,
    window = 100L
  ) {
    
    context_start <- max(
      1L,
      start_position - window
    )
    
    context_end <- min(
      nchar(text),
      end_position + window
    )
    
    stringr::str_squish(
      substr(
        text,
        context_start,
        context_end
      )
    )
  }
  
  find_matches <- function(
    text,
    source_name
  ) {
    
    if (
      is.na(text) ||
      !nzchar(stringr::str_squish(text))
    ) {
      return(empty_result)
    }
    
    matches <- list()
    match_number <- 0L
    
    ordered_gazetteer <- gazetteer |>
      dplyr::arrange(
        dplyr::desc(term_length),
        matched_place
      )
    
    for (row_number in seq_len(nrow(ordered_gazetteer))) {
      
      term <- ordered_gazetteer$matched_place[row_number]
      
      if (
        is.na(term) ||
        !nzchar(stringr::str_squish(term))
      ) {
        next
      }
      
      term <- stringr::str_squish(term)
      
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
      
      lengths <- attr(
        locations,
        "match.length"
      )
      
      for (location_number in seq_along(locations)) {
        
        start_position <- locations[location_number]
        
        end_position <- (
          start_position +
            lengths[location_number] -
            1L
        )
        
        match_number <- match_number + 1L
        
        matches[[match_number]] <- tibble::tibble(
          matched_place =
            ordered_gazetteer$matched_place[row_number],
          matched_text = substr(
            text,
            start_position,
            end_position
          ),
          country_name =
            ordered_gazetteer$country_name[row_number],
          iso3c =
            ordered_gazetteer$iso3c[row_number],
          match_type =
            ordered_gazetteer$match_type[row_number],
          ambiguous =
            as.logical(
              ordered_gazetteer$ambiguous[row_number]
            ),
          source = source_name,
          match_start = start_position,
          match_end = end_position,
          context = extract_context(
            text,
            start_position,
            end_position
          )
        )
      }
    }
    
    if (length(matches) == 0L) {
      return(empty_result)
    }
    
    results <- dplyr::bind_rows(matches) |>
      dplyr::arrange(
        match_start,
        dplyr::desc(
          nchar(matched_text)
        )
      )
    
    # Retain the longest geographical term where matches overlap.
    keep <- rep(
      TRUE,
      nrow(results)
    )
    
    if (nrow(results) > 1L) {
      
      for (current_row in seq_len(nrow(results))) {
        
        if (!keep[current_row]) {
          next
        }
        
        overlapping_rows <- which(
          seq_len(nrow(results)) != current_row &
            results$match_start <= results$match_end[current_row] &
            results$match_end >= results$match_start[current_row]
        )
        
        if (length(overlapping_rows) > 0L) {
          
          shorter_rows <- overlapping_rows[
            nchar(results$matched_text[overlapping_rows]) <
              nchar(results$matched_text[current_row])
          ]
          
          keep[shorter_rows] <- FALSE
        }
      }
    }
    
    results |>
      dplyr::filter(keep) |>
      dplyr::distinct() |>
      dplyr::arrange(
        match_start,
        match_end,
        matched_place
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