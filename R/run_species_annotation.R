# =============================================================================
# File: run_species_annotation.R
# Project: salmonscopingreview
# Purpose: Run species detection and farmed-species assignment across the corpus
# =============================================================================

run_species_annotation <- function(
    records,
    species_dictionary,
    progress = TRUE
) {
  
  required_record_columns <- c(
    "record_sequence",
    "record_id",
    "title",
    "abstract"
  )
  
  missing_record_columns <- setdiff(
    required_record_columns,
    names(records)
  )
  
  if (length(missing_record_columns) > 0) {
    stop(
      "Records are missing required columns: ",
      paste(missing_record_columns, collapse = ", ")
    )
  }
  
  if (!is.data.frame(species_dictionary)) {
    stop("species_dictionary must be a data frame.")
  }
  
  detect_one_record <- function(i) {
    
    mentions <- detect_species_mentions(
      title = records$title[[i]],
      abstract = records$abstract[[i]],
      dictionary = species_dictionary
    )
    
    mentions <- filter_species_mentions(
      mentions = mentions,
      title = records$title[[i]],
      abstract = records$abstract[[i]]
    )
    
    eligible_mentions <- mentions[
      mentions$mention_eligible %in% TRUE,
      ,
      drop = FALSE
    ]
    
    assignment <- assign_farmed_species(
      eligible_mentions
    )
    
    if (nrow(mentions) > 0) {
      mentions <- mentions |>
        dplyr::mutate(
          record_sequence = records$record_sequence[[i]],
          record_id = records$record_id[[i]],
          .before = 1
        )
    }
    
    assignment <- assignment |>
      dplyr::mutate(
        record_sequence = records$record_sequence[[i]],
        record_id = records$record_id[[i]],
        .before = 1
      )
    
    list(
      mentions = mentions,
      assignment = assignment
    )
  }
  
  indices <- seq_len(nrow(records))
  
  if (progress) {
    
    results <- purrr::map(
      indices,
      purrr::possibly(
        detect_one_record,
        otherwise = NULL
      ),
      .progress = TRUE
    )
    
  } else {
    
    results <- purrr::map(
      indices,
      purrr::possibly(
        detect_one_record,
        otherwise = NULL
      )
    )
  }
  
  failed_records <- which(
    vapply(results, is.null, logical(1))
  )
  
  successful_results <- results[
    !vapply(results, is.null, logical(1))
  ]
  
  species_mentions <- purrr::map_dfr(
    successful_results,
    "mentions"
  )
  
  species_assignments <- purrr::map_dfr(
    successful_results,
    "assignment"
  )
  
  failures <- if (length(failed_records) == 0) {
    
    tibble::tibble(
      record_sequence = integer(),
      record_id = character(),
      title = character()
    )
    
  } else {
    
    records |>
      dplyr::slice(failed_records) |>
      dplyr::select(
        record_sequence,
        record_id,
        title
      )
  }
  
  list(
    species_mentions = species_mentions,
    species_assignments = species_assignments,
    failures = failures
  )
}