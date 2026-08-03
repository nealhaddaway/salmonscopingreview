# =============================================================================
# File: classify_subtopics.R
# Project: salmonscopingreview
# Purpose: Classify subtopics within selected broad-topic branches
# =============================================================================

if (!exists("subtopic_prompt")) {
  source("R/llm_prompts.R")
}

classify_subtopics <- function(
    title,
    abstract,
    broad_topics,
    ontology,
    model = "gpt-5-mini"
) {

  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (!nzchar(api_key)) stop("OPENAI_API_KEY not found.")

  required <- c("broad_topic", "subtopic")
  missing <- setdiff(required, names(ontology))
  if (length(missing) > 0L) {
    stop("Ontology is missing required columns: ", paste(missing, collapse = ", "))
  }

  broad_topics <- unique(as.character(broad_topics))

  if (length(broad_topics) == 0L) {
    return(tibble::tibble(
      broad_topic = character(),
      subtopic = character(),
      review_required = logical(),
      review_reason = character()
    ))
  }

  candidate_subtopics <- ontology |>
    dplyr::filter(broad_topic %in% broad_topics) |>
    dplyr::distinct(broad_topic, subtopic) |>
    dplyr::arrange(broad_topic, subtopic)

  valid_paths <- paste(
    candidate_subtopics$broad_topic,
    candidate_subtopics$subtopic,
    sep = " > "
  )

  schema <- list(
    type = "object",
    properties = list(
      assignments = list(
        type = "array",
        items = list(type = "string", enum = I(valid_paths))
      ),
      review_required = list(type = "boolean"),
      review_reason = list(type = c("string", "null"))
    ),
    required = c("assignments", "review_required", "review_reason"),
    additionalProperties = FALSE
  )

  user_prompt <- paste0(
    "SELECTED BROAD TOPICS\n",
    paste(broad_topics, collapse = "\n"),
    "\n\nVALID SUBTOPIC PATHS\n",
    paste(valid_paths, collapse = "\n"),
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
        content = list(list(type = "input_text", text = subtopic_prompt()))
      ),
      list(
        role = "user",
        content = list(list(type = "input_text", text = user_prompt))
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

  response <- httr2::request("https://api.openai.com/v1/responses") |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_timeout(120) |>
    httr2::req_retry(max_tries = 3, backoff = ~ 2^.x) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  status_code <- httr2::resp_status(response)
  if (status_code >= 400L) {
    stop("OpenAI HTTP ", status_code, ": ", httr2::resp_body_string(response))
  }

  response <- httr2::resp_body_json(response)

  message_items <- response$output[
    vapply(response$output, function(item) identical(item$type, "message"), logical(1))
  ]
  if (length(message_items) == 0L) stop("OpenAI response contained no message output.")

  content_items <- unlist(
    lapply(message_items, function(item) item$content),
    recursive = FALSE
  )
  text_items <- content_items[
    vapply(
      content_items,
      function(item) identical(item$type, "output_text") && !is.null(item$text),
      logical(1)
    )
  ]
  if (length(text_items) == 0L) stop("OpenAI response contained no output_text content.")

  parsed <- jsonlite::fromJSON(text_items[[1]]$text)

  invalid <- setdiff(parsed$assignments, valid_paths)
  if (length(invalid) > 0L) {
    stop("Invalid subtopic paths returned: ", paste(invalid, collapse = ", "))
  }

  if (length(parsed$assignments) == 0L) {
    return(tibble::tibble(
      broad_topic = character(),
      subtopic = character(),
      review_required = logical(),
      review_reason = character()
    ))
  }

  parts <- stringr::str_split_fixed(parsed$assignments, "\\s*>\\s*", 2)

  tibble::tibble(
    broad_topic = parts[, 1],
    subtopic = parts[, 2],
    review_required = parsed$review_required,
    review_reason = if (is.null(parsed$review_reason)) NA_character_ else parsed$review_reason
  )
}
