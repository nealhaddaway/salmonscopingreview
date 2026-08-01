# =============================================================================
# File: classify_features.R
# Project: salmonscopingreview
# Purpose: Classify features within selected subtopic branches
# =============================================================================

classify_features <- function(
    title,
    abstract,
    subtopics,
    ontology,
    model = "gpt-5-mini"
) {
  
  api_key <- Sys.getenv("OPENAI_API_KEY")
  
  if (!nzchar(api_key)) {
    stop("OPENAI_API_KEY not found.")
  }
  
  required_subtopic_columns <- c(
    "broad_topic",
    "subtopic"
  )
  
  missing_subtopic_columns <- setdiff(
    required_subtopic_columns,
    names(subtopics)
  )
  
  if (length(missing_subtopic_columns) > 0L) {
    stop(
      "Subtopics are missing required columns: ",
      paste(missing_subtopic_columns, collapse = ", ")
    )
  }
  
  required_ontology_columns <- c(
    "broad_topic",
    "subtopic",
    "feature"
  )
  
  missing_ontology_columns <- setdiff(
    required_ontology_columns,
    names(ontology)
  )
  
  if (length(missing_ontology_columns) > 0L) {
    stop(
      "Ontology is missing required columns: ",
      paste(missing_ontology_columns, collapse = ", ")
    )
  }
  
  selected_subtopics <- subtopics |>
    dplyr::distinct(
      broad_topic,
      subtopic
    )
  
  if (nrow(selected_subtopics) == 0L) {
    return(
      tibble::tibble(
        broad_topic = character(),
        subtopic = character(),
        feature = character(),
        review_required = logical(),
        review_reason = character()
      )
    )
  }
  
  candidate_features <- ontology |>
    dplyr::semi_join(
      selected_subtopics,
      by = c(
        "broad_topic",
        "subtopic"
      )
    ) |>
    dplyr::distinct(
      broad_topic,
      subtopic,
      feature
    ) |>
    dplyr::arrange(
      broad_topic,
      subtopic,
      feature
    )
  
  if (nrow(candidate_features) == 0L) {
    stop("No candidate features found for the selected subtopics.")
  }
  
  candidate_lines <- candidate_features |>
    dplyr::transmute(
      line = paste(
        broad_topic,
        subtopic,
        feature,
        sep = " > "
      )
    ) |>
    dplyr::pull(line)
  
  valid_paths <- candidate_lines
  
  system_prompt <- paste(
    "You are classifying scientific abstracts for a systematic evidence map",
    "of farmed salmon and rainbow trout research.",
    "",
    "Broad topics and subtopics have already been selected.",
    "Select only feature paths that are substantive subjects of the study.",
    "",
    "Ignore incidental, contextual and background-only mentions.",
    "Do not assign a feature merely because a related term appears.",
    "Use only the listed feature paths.",
    "Multiple feature paths are allowed when genuinely investigated.",
    "Return an empty assignments array if none applies.",
    sep = "\n"
  )
  
  schema <- list(
    type = "object",
    properties = list(
      assignments = list(
        type = "array",
        items = list(
          type = "string",
          enum = valid_paths
        )
      ),
      review_required = list(
        type = "boolean"
      ),
      review_reason = list(
        type = c(
          "string",
          "null"
        )
      )
    ),
    required = c(
      "assignments",
      "review_required",
      "review_reason"
    ),
    additionalProperties = FALSE
  )
  
  selected_lines <- selected_subtopics |>
    dplyr::transmute(
      line = paste(
        broad_topic,
        subtopic,
        sep = " > "
      )
    ) |>
    dplyr::pull(line)
  
  user_prompt <- paste0(
    "SELECTED SUBTOPIC PATHS\n",
    paste(
      selected_lines,
      collapse = "\n"
    ),
    "\n\nVALID FEATURE PATHS\n",
    paste(
      candidate_lines,
      collapse = "\n"
    ),
    "\n\nTITLE\n",
    title,
    "\n\nABSTRACT\n",
    abstract,
    "\n\nSelect the valid substantive feature paths."
  )
  
  body <- list(
    model = model,
    store = FALSE,
    input = list(
      list(
        role = "system",
        content = list(
          list(
            type = "input_text",
            text = system_prompt
          )
        )
      ),
      list(
        role = "user",
        content = list(
          list(
            type = "input_text",
            text = user_prompt
          )
        )
      )
    ),
    text = list(
      format = list(
        type = "json_schema",
        name = "feature_classification",
        strict = TRUE,
        schema = schema
      )
    )
  )
  
  response <- httr2::request(
    "https://api.openai.com/v1/responses"
  ) |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_body_json(
      body,
      auto_unbox = TRUE
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  
  if (!identical(response$status, "completed")) {
    stop(
      "OpenAI response did not complete. Status: ",
      response$status
    )
  }
  
  message_items <- response$output[
    vapply(
      response$output,
      function(item) {
        identical(item$type, "message")
      },
      logical(1)
    )
  ]
  
  if (length(message_items) == 0L) {
    stop("OpenAI response contained no message output.")
  }
  
  content_items <- unlist(
    lapply(
      message_items,
      function(item) {
        item$content
      }
    ),
    recursive = FALSE
  )
  
  text_items <- content_items[
    vapply(
      content_items,
      function(item) {
        identical(item$type, "output_text") &&
          !is.null(item$text)
      },
      logical(1)
    )
  ]
  
  if (length(text_items) == 0L) {
    stop("OpenAI response contained no output_text content.")
  }
  
  parsed <- jsonlite::fromJSON(
    text_items[[1]]$text
  )
  
  invalid_assignments <- setdiff(
    parsed$assignments,
    valid_paths
  )
  
  if (length(invalid_assignments) > 0L) {
    stop(
      "Invalid feature paths returned: ",
      paste(invalid_assignments, collapse = ", ")
    )
  }
  
  if (length(parsed$assignments) == 0L) {
    return(
      tibble::tibble(
        broad_topic = character(),
        subtopic = character(),
        feature = character(),
        review_required = logical(),
        review_reason = character()
      )
    )
  }
  
  assignment_parts <- stringr::str_split_fixed(
    parsed$assignments,
    "\\s*>\\s*",
    3
  )
  
  tibble::tibble(
    broad_topic = assignment_parts[, 1],
    subtopic = assignment_parts[, 2],
    feature = assignment_parts[, 3],
    review_required = parsed$review_required,
    review_reason = if (
      is.null(parsed$review_reason)
    ) {
      NA_character_
    } else {
      parsed$review_reason
    }
  )
}