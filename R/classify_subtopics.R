# =============================================================================
# File: classify_subtopics.R
# Project: salmonscopingreview
# Purpose: Classify subtopics within selected broad-topic branches
# =============================================================================

classify_subtopics <- function(
    title,
    abstract,
    broad_topics,
    ontology,
    model = "gpt-5-mini"
) {
  
  api_key <- Sys.getenv("OPENAI_API_KEY")
  
  if (!nzchar(api_key)) {
    stop("OPENAI_API_KEY not found.")
  }
  
  allowed_broad_topics <- c(
    "Production",
    "Impacts",
    "Consumption",
    "Business and economy",
    "Research methods"
  )
  
  broad_topics <- unique(
    as.character(broad_topics)
  )
  
  invalid_broad_topics <- setdiff(
    broad_topics,
    allowed_broad_topics
  )
  
  if (length(invalid_broad_topics) > 0L) {
    stop(
      "Invalid broad topics supplied: ",
      paste(invalid_broad_topics, collapse = ", ")
    )
  }
  
  required_ontology_columns <- c(
    "broad_topic",
    "subtopic"
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
  
  if (length(broad_topics) == 0L) {
    return(
      tibble::tibble(
        broad_topic = character(),
        subtopic = character(),
        review_required = logical(),
        review_reason = character()
      )
    )
  }
  
  candidate_subtopics <- ontology |>
    dplyr::filter(
      broad_topic %in% broad_topics
    ) |>
    dplyr::distinct(
      broad_topic,
      subtopic
    ) |>
    dplyr::arrange(
      broad_topic,
      subtopic
    )
  
  if (nrow(candidate_subtopics) == 0L) {
    stop("No candidate subtopics found for the selected broad topics.")
  }
  
  candidate_lines <- candidate_subtopics |>
    dplyr::transmute(
      line = paste0(
        broad_topic,
        " > ",
        subtopic
      )
    ) |>
    dplyr::pull(line)
  
  valid_paths <- candidate_lines
  
  system_prompt <- paste(
    "You are classifying scientific abstracts for a systematic evidence map",
    "of farmed salmon and rainbow trout research.",
    "",
    "The record has already been assigned one or more broad topics.",
    "Select only subtopic paths that are substantive subjects of the study.",
    "",
    "Do not select a subtopic because a related word merely appears.",
    "Ignore incidental, contextual and background-only mentions.",
    "Do not introduce paths that are not listed.",
    "Multiple subtopic paths are allowed when genuinely studied.",
    "Return an empty assignments array if none of the listed subtopics applies.",
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
  
  user_prompt <- paste0(
    "SELECTED BROAD TOPICS\n",
    paste(
      broad_topics,
      collapse = "\n"
    ),
    "\n\nVALID SUBTOPIC PATHS\n",
    paste(
      candidate_lines,
      collapse = "\n"
    ),
    "\n\nTITLE\n",
    title,
    "\n\nABSTRACT\n",
    abstract,
    "\n\nSelect the valid substantive subtopic paths."
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
        name = "subtopic_classification",
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
      "Invalid subtopic paths returned: ",
      paste(invalid_assignments, collapse = ", ")
    )
  }
  
  if (length(parsed$assignments) == 0L) {
    return(
      tibble::tibble(
        broad_topic = character(),
        subtopic = character(),
        review_required = logical(),
        review_reason = character()
      )
    )
  }
  
  assignment_parts <- stringr::str_split_fixed(
    parsed$assignments,
    "\\s*>\\s*",
    2
  )
  
  tibble::tibble(
    broad_topic = assignment_parts[, 1],
    subtopic = assignment_parts[, 2],
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