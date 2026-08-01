# Filter detected species mentions before farmed-species assignment
#
# The function retains every detected mention but adds:
# - mention_eligible: whether it may contribute to farmed-species assignment
# - filter_reason: why an ineligible mention was excluded
#
# Initial rules:
# 1. Exclude a species mention explicitly identified as wild.
# 2. Exclude a generic salmon mention when a nearby named non-target
#    Salmo species supplies the apparent species identity.

filter_species_mentions <- function(
    mentions,
    title,
    abstract,
    wild_context_chars = 50L,
    non_target_context_chars = 120L
) {
  
  required_columns <- c(
    "species_id",
    "preferred_name",
    "scientific_name",
    "matched_term",
    "synonym_type",
    "source",
    "match_start",
    "match_end",
    "is_farmed_candidate"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(mentions)
  )
  
  if (length(missing_columns) > 0L) {
    stop(
      "Missing columns from species mentions: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  if (nrow(mentions) == 0L) {
    mentions$mention_eligible <- logical(0)
    mentions$filter_reason <- character(0)
    return(mentions)
  }
  
  title <- ifelse(
    is.na(title),
    "",
    as.character(title)
  )
  
  abstract <- ifelse(
    is.na(abstract),
    "",
    as.character(abstract)
  )
  
  source_text <- function(source) {
    
    if (is.na(source)) {
      return("")
    }
    
    if (tolower(source) == "title") {
      return(title)
    }
    
    if (tolower(source) == "abstract") {
      return(abstract)
    }
    
    ""
  }
  
  extract_context <- function(
    text,
    start,
    end,
    padding
  ) {
    
    if (
      !nzchar(text) ||
      is.na(start) ||
      is.na(end)
    ) {
      return("")
    }
    
    text_length <- nchar(text)
    
    context_start <- max(
      1L,
      as.integer(start) - as.integer(padding)
    )
    
    context_end <- min(
      text_length,
      as.integer(end) + as.integer(padding)
    )
    
    substr(
      text,
      context_start,
      context_end
    )
  }
  
  is_explicitly_wild <- function(
    text,
    start,
    end
  ) {
    
    context <- extract_context(
      text = text,
      start = start,
      end = end,
      padding = wild_context_chars
    )
    
    if (!nzchar(context)) {
      return(FALSE)
    }
    
    context_lower <- tolower(context)
    
    wild_pattern <- paste0(
      "\\b(",
      "wild|",
      "wild-caught|",
      "wild caught|",
      "wild-origin|",
      "wild origin|",
      "naturally reproducing|",
      "free-living",
      ")\\b"
    )
    
    farmed_pattern <- paste0(
      "\\b(",
      "farmed|",
      "cultured|",
      "aquacultur(?:e|ed)|",
      "hatchery-reared|",
      "hatchery reared",
      ")\\b"
    )
    
    has_wild_context <- grepl(
      wild_pattern,
      context_lower,
      perl = TRUE
    )
    
    has_farmed_context <- grepl(
      farmed_pattern,
      context_lower,
      perl = TRUE
    )
    
    has_wild_context && !has_farmed_context
  }
  
  is_generic_salmon <- function(row) {
    
    preferred_name <- tolower(
      ifelse(
        is.na(row$preferred_name),
        "",
        row$preferred_name
      )
    )
    
    species_id <- tolower(
      ifelse(
        is.na(row$species_id),
        "",
        row$species_id
      )
    )
    
    preferred_name == "unspecified farmed salmon" ||
      grepl(
        "unspecified.*salmon|generic.*salmon",
        species_id,
        perl = TRUE
      )
  }
  
  is_named_non_target_salmo <- function(row) {
    
    scientific_name <- ifelse(
      is.na(row$scientific_name),
      "",
      row$scientific_name
    )
    
    starts_with_salmo <- grepl(
      "^Salmo\\s+[[:alpha:]-]+$",
      scientific_name,
      ignore.case = TRUE,
      perl = TRUE
    )
    
    non_target <- !isTRUE(
      row$is_farmed_candidate
    )
    
    starts_with_salmo && non_target
  }
  
  generic_refers_to_non_target <- function(
    row_index
  ) {
    
    generic_row <- mentions[row_index, , drop = FALSE]
    
    if (!is_generic_salmon(generic_row)) {
      return(FALSE)
    }
    
    # Explicit salmon-farming phrases should remain eligible even when a
    # non-target Salmo species is mentioned nearby.
    explicit_farming_terms <- c(
      "salmon farm",
      "salmon farms",
      "farmed salmon",
      "salmon aquaculture",
      "salmon cage",
      "salmon cages",
      "salmon pen",
      "salmon pens"
    )
    
    matched_term <- tolower(
      ifelse(
        is.na(generic_row$matched_term),
        "",
        generic_row$matched_term
      )
    )
    
    if (matched_term %in% explicit_farming_terms) {
      return(FALSE)
    }
    
    same_source <- mentions$source == generic_row$source
    
    non_target_rows <- vapply(
      seq_len(nrow(mentions)),
      function(i) {
        is_named_non_target_salmo(
          mentions[i, , drop = FALSE]
        )
      },
      logical(1)
    )
    
    candidates <- mentions[
      same_source & non_target_rows,
      ,
      drop = FALSE
    ]
    
    if (nrow(candidates) == 0L) {
      return(FALSE)
    }
    
    generic_midpoint <- mean(
      c(
        generic_row$match_start,
        generic_row$match_end
      ),
      na.rm = TRUE
    )
    
    candidate_midpoints <- rowMeans(
      cbind(
        candidates$match_start,
        candidates$match_end
      ),
      na.rm = TRUE
    )
    
    any(
      abs(
        candidate_midpoints - generic_midpoint
      ) <= non_target_context_chars
    )
  }
  
  mentions$mention_eligible <- TRUE
  mentions$filter_reason <- NA_character_
  
  for (i in seq_len(nrow(mentions))) {
    
    if (generic_refers_to_non_target(i)) {
      
      mentions$mention_eligible[i] <- FALSE
      mentions$filter_reason[i] <-
        paste0(
          "Generic salmon mention located near a named ",
          "non-target Salmo species"
        )
    }
  }
  
  mentions
}