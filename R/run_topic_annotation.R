# =============================================================================
# File: run_topic_annotation.R
# Project: salmonscopingreview
# Purpose: Run topic detection across the corpus
# =============================================================================

run_topic_annotation <- function(
    records,
    topic_dictionary,
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
  
  if (length(missing_record_columns) > 0L) {
    stop(
      "Records are missing required columns: ",
      paste(missing_record_columns, collapse = ", ")
    )
  }
  
  if (!is.data.frame(topic_dictionary)) {
    stop("topic_dictionary must be a data frame.")
  }
  
  annotate_one_record <- function(i) {
    
    mentions <- detect_topic_mentions(
      title = records$title[[i]],
      abstract = records$abstract[[i]],
      dictionary = topic_dictionary
    )
    
    if (nrow(mentions) > 0L) {
      mentions <- mentions |>
        dplyr::mutate(
          record_sequence = records$record_sequence[[i]],
          record_id = records$record_id[[i]],
          .before = 1
        )
    }
    
    record_topics <- if (nrow(mentions) == 0L) {
      
      tibble::tibble(
        record_sequence = records$record_sequence[[i]],
        record_id = records$record_id[[i]],
        broad_topic = NA_character_,
        subtopic = NA_character_,
        feature = NA_character_,
        component = NA_character_,
        review_required = TRUE,
        assignment_reason = "No topic dictionary terms detected"
      )
      
    } else {
      
      mentions |>
        dplyr::distinct(
          broad_topic,
          subtopic,
          feature,
          component
        ) |>
        dplyr::mutate(
          record_sequence = records$record_sequence[[i]],
          record_id = records$record_id[[i]],
          review_required = FALSE,
          assignment_reason = "One or more topic dictionary terms detected",
          .before = 1
        )
      
    }
    
    list(
      mentions = mentions,
      record_topics = record_topics
    )
  }
  
  indices <- seq_len(nrow(records))
  
  if (progress) {
    
    results <- purrr::map(
      indices,
      purrr::possibly(
        annotate_one_record,
        otherwise = NULL
      ),
      .progress = TRUE
    )
    
  } else {
    
    results <- purrr::map(
      indices,
      purrr::possibly(
        annotate_one_record,
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
  
  topic_mentions <- purrr::map_dfr(
    successful_results,
    "mentions"
  )
  
  topic_assignments <- purrr::map_dfr(
    successful_results,
    "record_topics"
  )
  
  failures <- if (length(failed_records) == 0L) {
    
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
    topic_mentions = topic_mentions,
    topic_assignments = topic_assignments,
    failures = failures
  )
}