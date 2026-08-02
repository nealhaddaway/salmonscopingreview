# =============================================================================
# File: classify_terms.R
# Project: salmonscopingreview
# Purpose: Classify controlled dictionary terms within selected component paths
# =============================================================================

classify_terms <- function(
    title,
    abstract,
    components,
    topic_dictionary,
    model = "gpt-5-mini"
) {

  api_key <- Sys.getenv("OPENAI_API_KEY")

  if (!nzchar(api_key)) {
    stop("OPENAI_API_KEY not found.")
  }

  required_component_columns <- c(
    "broad_topic",
    "subtopic",
    "feature",
    "component"
  )

  missing_component_columns <- setdiff(
    required_component_columns,
    names(components)
  )

  if (length(missing_component_columns) > 0L) {
    stop(
      "Components are missing required columns: ",
      paste(missing_component_columns, collapse = ", ")
    )
  }

  required_dictionary_columns <- c(
    "broad_topic",
    "subtopic",
    "feature",
    "component",
    "term"
  )

  missing_dictionary_columns <- setdiff(
    required_dictionary_columns,
    names(topic_dictionary)
  )

  if (length(missing_dictionary_columns) > 0L) {
    stop(
      "Topic dictionary is missing required columns: ",
      paste(missing_dictionary_columns, collapse = ", ")
    )
  }

  selected_components <- components |>
    dplyr::distinct(
      broad_topic,
      subtopic,
      feature,
      component
    )

  if (nrow(selected_components) == 0L) {
    return(
      tibble::tibble(
        broad_topic = character(),
        subtopic = character(),
        feature = character(),
        component = character(),
        term = character(),
        review_required = logical(),
        review_reason = character()
      )
    )
  }

  candidate_terms <- topic_dictionary |>
    dplyr::semi_join(
      selected_components,
      by = c(
        "broad_topic",
        "subtopic",
        "feature",
        "component"
      )
    ) |>
    dplyr::filter(
      !is.na(term),
      nzchar(stringr::str_squish(term))
    ) |>
    dplyr::mutate(
      term = stringr::str_squish(term),
      valid_path = paste(
        broad_topic,
        subtopic,
        feature,
        component,
        term,
        sep = " > "
      )
    ) |>
    dplyr::distinct(
      broad_topic,
      subtopic,
      feature,
      component,
      term,
      valid_path
    ) |>
    dplyr::arrange(
      broad_topic,
      subtopic,
      feature,
      component,
      term
    )

  if (nrow(candidate_terms) == 0L) {
    stop("No candidate terms found for the selected components.")
  }

  valid_paths <- candidate_terms$valid_path

  selected_component_lines <- selected_components |>
    dplyr::transmute(
      line = paste(
        broad_topic,
        subtopic,
        feature,
        component,
        sep = " > "
      )
    ) |>
    dplyr::pull(line)

  system_prompt <- paste(
    "You are assigning controlled vocabulary terms to scientific abstracts",
    "for a systematic evidence map of farmed salmon and rainbow trout research.",
    "",
    "Broad topics, subtopics, features and components have already been selected.",
    "Select only terms that are substantively applicable to the study.",
    "",
    "A selected term may be a semantic label even when the exact words do not",
    "appear in the title or abstract.",
    "Do not select incidental, contextual or background-only terms.",
    "Use only the listed five-level paths.",
    "Multiple terms are allowed when genuinely applicable.",
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
          enum = I(valid_paths)
        )
      ),
      review_required = list(type = "boolean"),
      review_reason = list(type = c("string", "null"))
    ),
    required = c(
      "assignments",
      "review_required",
      "review_reason"
    ),
    additionalProperties = FALSE
  )

  user_prompt <- paste0(
    "SELECTED COMPONENT PATHS\n",
    paste(selected_component_lines, collapse = "\n"),
    "\n\nVALID TERM PATHS\n",
    paste(candidate_terms$valid_path, collapse = "\n"),
    "\n\nTITLE\n",
    title,
    "\n\nABSTRACT\n",
    abstract,
    "\n\nSelect the valid substantive term paths."
  )

  body <- list(
    model = model,
    store = FALSE,
    input = list(
      list(
        role = "system",
        content = list(
          list(type = "input_text", text = system_prompt)
        )
      ),
      list(
        role = "user",
        content = list(
          list(type = "input_text", text = user_prompt)
        )
      )
    ),
    text = list(
      format = list(
        type = "json_schema",
        name = "term_classification",
        strict = TRUE,
        schema = schema
      )
    )
  )

  response <- httr2::request(
    "https://api.openai.com/v1/responses"
  ) |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_timeout(120) |>
    httr2::req_retry(
      max_tries = 3,
      backoff = ~ 2^.x
    ) |>
    httr2::req_error(
      is_error = function(resp) FALSE
    ) |>
    httr2::req_perform()
  
  status_code <- httr2::resp_status(response)
  
  if (status_code >= 400L) {
    
    error_body <- httr2::resp_body_string(response)
    
    stop(
      "OpenAI HTTP ",
      status_code,
      ": ",
      error_body
    )
  }
  
  response <- httr2::resp_body_json(response)

  if (!identical(response$status, "completed")) {
    stop(
      "OpenAI response did not complete. Status: ",
      response$status
    )
  }

  message_items <- response$output[
    vapply(
      response$output,
      function(item) identical(item$type, "message"),
      logical(1)
    )
  ]

  if (length(message_items) == 0L) {
    stop("OpenAI response contained no message output.")
  }

  content_items <- unlist(
    lapply(message_items, function(item) item$content),
    recursive = FALSE
  )

  text_items <- content_items[
    vapply(
      content_items,
      function(item) {
        identical(item$type, "output_text") && !is.null(item$text)
      },
      logical(1)
    )
  ]

  if (length(text_items) == 0L) {
    stop("OpenAI response contained no output_text content.")
  }

  parsed <- jsonlite::fromJSON(text_items[[1]]$text)

  invalid_assignments <- setdiff(
    parsed$assignments,
    valid_paths
  )

  if (length(invalid_assignments) > 0L) {
    stop(
      "Invalid term paths returned: ",
      paste(invalid_assignments, collapse = ", ")
    )
  }

  if (length(parsed$assignments) == 0L) {
    return(
      tibble::tibble(
        broad_topic = character(),
        subtopic = character(),
        feature = character(),
        component = character(),
        term = character(),
        review_required = logical(),
        review_reason = character()
      )
    )
  }

  assignment_parts <- stringr::str_split_fixed(
    parsed$assignments,
    "\\s*>\\s*",
    5
  )

  tibble::tibble(
    broad_topic = assignment_parts[, 1],
    subtopic = assignment_parts[, 2],
    feature = assignment_parts[, 3],
    component = assignment_parts[, 4],
    term = assignment_parts[, 5],
    review_required = parsed$review_required,
    review_reason = if (is.null(parsed$review_reason)) {
      NA_character_
    } else {
      parsed$review_reason
    }
  )
}
