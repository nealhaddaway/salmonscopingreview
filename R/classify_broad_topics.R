# =============================================================================
# File: classify_broad_topics.R
# Project: salmonscopingreview
# Purpose: Classify broad topics using GPT-5 mini
# =============================================================================

if (!exists("broad_topic_prompt")) {
  source("R/llm_prompts.R")
}

classify_broad_topics <- function(
    title,
    abstract,
    model = "gpt-5-mini"
) {

  api_key <- Sys.getenv("OPENAI_API_KEY")

  if (!nzchar(api_key)) {
    stop("OPENAI_API_KEY not found.")
  }

  allowed_topics <- c(
    "Production",
    "Impacts",
    "Consumption",
    "Business and economy",
    "Research methods"
  )

  schema <- list(
    type = "object",
    properties = list(
      broad_topics = list(
        type = "array",
        items = list(
          type = "string",
          enum = I(allowed_topics)
        )
      ),
      review_required = list(type = "boolean"),
      review_reason = list(type = c("string", "null"))
    ),
    required = c(
      "broad_topics",
      "review_required",
      "review_reason"
    ),
    additionalProperties = FALSE
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
            text = broad_topic_prompt()
          )
        )
      ),
      list(
        role = "user",
        content = list(
          list(
            type = "input_text",
            text = paste0(
              "TITLE\n",
              title,
              "\n\nABSTRACT\n",
              abstract
            )
          )
        )
      )
    ),
    text = list(
      format = list(
        type = "json_schema",
        name = "broad_topics",
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
    httr2::req_retry(max_tries = 3, backoff = ~ 2^.x) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  status_code <- httr2::resp_status(response)

  if (status_code >= 400L) {
    stop(
      "OpenAI HTTP ",
      status_code,
      ": ",
      httr2::resp_body_string(response)
    )
  }

  response <- httr2::resp_body_json(response)

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

  invalid <- setdiff(parsed$broad_topics, allowed_topics)

  if (length(invalid) > 0L) {
    stop(
      "Invalid broad topics returned: ",
      paste(invalid, collapse = ", ")
    )
  }

  tibble::tibble(
    broad_topics = list(parsed$broad_topics),
    review_required = parsed$review_required,
    review_reason = if (is.null(parsed$review_reason)) {
      NA_character_
    } else {
      parsed$review_reason
    }
  )
}
