# =============================================================================
# File: classify_features.R
# Project: salmonscopingreview
# Purpose: Classify features within selected subtopic branches
# =============================================================================

if (!exists("feature_prompt")) {
  source("R/llm_prompts.R")
}

classify_features <- function(
    title,
    abstract,
    subtopics,
    ontology,
    model = "gpt-5-mini"
) {

  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (!nzchar(api_key)) stop("OPENAI_API_KEY not found.")

  selected_subtopics <- subtopics |>
    dplyr::distinct(broad_topic, subtopic)

  if (nrow(selected_subtopics) == 0L) {
    return(tibble::tibble(
      broad_topic = character(),
      subtopic = character(),
      feature = character(),
      review_required = logical(),
      review_reason = character()
    ))
  }

  candidate_features <- ontology |>
    dplyr::semi_join(
      selected_subtopics,
      by = c("broad_topic", "subtopic")
    ) |>
    dplyr::distinct(broad_topic, subtopic, feature) |>
    dplyr::arrange(broad_topic, subtopic, feature)

  valid_paths <- paste(
    candidate_features$broad_topic,
    candidate_features$subtopic,
    candidate_features$feature,
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

  selected_lines <- paste(
    selected_subtopics$broad_topic,
    selected_subtopics$subtopic,
    sep = " > "
  )

  user_prompt <- paste0(
    "SELECTED SUBTOPIC PATHS\n",
    paste(selected_lines, collapse = "\n"),
    "\n\nVALID FEATURE PATHS\n",
    paste(valid_paths, collapse = "\n"),
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
        content = list(list(type = "input_text", text = feature_prompt()))
      ),
      list(
        role = "user",
        content = list(list(type = "input_text", text = user_prompt))
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
    stop("Invalid feature paths returned: ", paste(invalid, collapse = ", "))
  }

  if (length(parsed$assignments) == 0L) {
    return(tibble::tibble(
      broad_topic = character(),
      subtopic = character(),
      feature = character(),
      review_required = logical(),
      review_reason = character()
    ))
  }

  parts <- stringr::str_split_fixed(parsed$assignments, "\\s*>\\s*", 3)

  tibble::tibble(
    broad_topic = parts[, 1],
    subtopic = parts[, 2],
    feature = parts[, 3],
    review_required = parsed$review_required,
    review_reason = if (is.null(parsed$review_reason)) NA_character_ else parsed$review_reason
  )
}
