# =============================================================================
# File: classify_components.R
# Project: salmonscopingreview
# Purpose: Classify components within selected feature branches
# =============================================================================

if (!exists("component_prompt")) {
  source("R/llm_prompts.R")
}

classify_components <- function(
    title,
    abstract,
    features,
    ontology,
    model = "gpt-5-mini"
) {

  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (!nzchar(api_key)) stop("OPENAI_API_KEY not found.")

  selected_features <- features |>
    dplyr::distinct(broad_topic, subtopic, feature)

  if (nrow(selected_features) == 0L) {
    return(tibble::tibble(
      broad_topic = character(),
      subtopic = character(),
      feature = character(),
      component = character(),
      review_required = logical(),
      review_reason = character()
    ))
  }

  candidate_components <- ontology |>
    dplyr::semi_join(
      selected_features,
      by = c("broad_topic", "subtopic", "feature")
    ) |>
    dplyr::distinct(
      broad_topic,
      subtopic,
      feature,
      component,
      supporting_terms
    ) |>
    dplyr::arrange(broad_topic, subtopic, feature, component)

  shorten_terms <- function(x, maximum_terms = 10L) {
    terms <- stringr::str_split(dplyr::coalesce(x, ""), ";\\s*")[[1]]
    terms <- terms[nzchar(terms)]
    terms <- terms[!duplicated(stringr::str_to_lower(terms))]
    paste(utils::head(terms, maximum_terms), collapse = "; ")
  }

  candidate_components <- candidate_components |>
    dplyr::rowwise() |>
    dplyr::mutate(
      representative_terms = shorten_terms(supporting_terms),
      valid_path = paste(
        broad_topic,
        subtopic,
        feature,
        component,
        sep = " > "
      ),
      prompt_line = paste0(
        valid_path,
        " | Representative terms: ",
        representative_terms
      )
    ) |>
    dplyr::ungroup()

  valid_paths <- candidate_components$valid_path

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
    selected_features$broad_topic,
    selected_features$subtopic,
    selected_features$feature,
    sep = " > "
  )

  user_prompt <- paste0(
    "SELECTED FEATURE PATHS\n",
    paste(selected_lines, collapse = "\n"),
    "\n\nVALID COMPONENT PATHS\n",
    paste(candidate_components$prompt_line, collapse = "\n"),
    "\n\nTITLE\n",
    title,
    "\n\nABSTRACT\n",
    abstract,
    "\n\nSelect the valid substantive component paths."
  )

  body <- list(
    model = model,
    store = FALSE,
    input = list(
      list(
        role = "system",
        content = list(list(type = "input_text", text = component_prompt()))
      ),
      list(
        role = "user",
        content = list(list(type = "input_text", text = user_prompt))
      )
    ),
    text = list(
      format = list(
        type = "json_schema",
        name = "component_classification",
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
    stop("Invalid component paths returned: ", paste(invalid, collapse = ", "))
  }

  if (length(parsed$assignments) == 0L) {
    return(tibble::tibble(
      broad_topic = character(),
      subtopic = character(),
      feature = character(),
      component = character(),
      review_required = logical(),
      review_reason = character()
    ))
  }

  parts <- stringr::str_split_fixed(parsed$assignments, "\\s*>\\s*", 4)

  tibble::tibble(
    broad_topic = parts[, 1],
    subtopic = parts[, 2],
    feature = parts[, 3],
    component = parts[, 4],
    review_required = parsed$review_required,
    review_reason = if (is.null(parsed$review_reason)) NA_character_ else parsed$review_reason
  )
}
