# =============================================================================
# File: classify_broad_topics.R
# Project: salmonscopingreview
# Purpose: Classify broad topics using GPT-5 mini
# =============================================================================

library(httr2)
library(jsonlite)
library(tibble)

classify_broad_topics <- function(
    title,
    abstract,
    model = "gpt-5-mini"
) {
  
  api_key <- Sys.getenv("OPENAI_API_KEY")
  
  if (!nzchar(api_key)) {
    stop("OPENAI_API_KEY not found.")
  }
  
  system_prompt <- paste(
    "You are classifying scientific abstracts for a systematic evidence map.",
    "",
    "Assign ONLY broad topics that are substantive subjects of investigation.",
    "",
    "Ignore:",
    "- background information",
    "- incidental mentions",
    "- keywords that are not central to the study",
    "",
    "The only valid labels are:",
    "Production",
    "Impacts",
    "Consumption",
    "Business and economy",
    "Research methods",
    sep = "\n"
  )
  
  schema <- list(
    type = "object",
    properties = list(
      broad_topics = list(
        type = "array",
        items = list(
          type = "string",
          enum = c(
            "Production",
            "Impacts",
            "Consumption",
            "Business and economy",
            "Research methods"
          )
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
            text = paste0(
              "Title\n",
              title,
              "\n\nAbstract\n",
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
  
  response <-
    
    request("https://api.openai.com/v1/responses") |>
    
    req_auth_bearer_token(api_key) |>
    
    req_body_json(body, auto_unbox = TRUE) |>
    
    req_perform() |>
    
    resp_body_json()
  
  if (!identical(response$status, "completed")) {
    stop(
      "OpenAI response did not complete. Status: ",
      response$status,
      if (!is.null(response$error$message)) {
        paste0(". Error: ", response$error$message)
      } else {
        ""
      }
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
    stop(
      "OpenAI response contained no message output. Output types: ",
      paste(
        vapply(
          response$output,
          function(item) {
            if (is.null(item$type)) "unknown" else item$type
          },
          character(1)
        ),
        collapse = ", "
      )
    )
  }
  
  content_items <- unlist(
    lapply(
      message_items,
      function(item) item$content
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
  
  json_text <- text_items[[1]]$text
  
  parsed <-
    
    jsonlite::fromJSON(json_text)
  
  invalid <- setdiff(
    
    parsed$broad_topics,
    
    c(
      "Production",
      "Impacts",
      "Consumption",
      "Business and economy",
      "Research methods"
    )
    
  )
  
  if (length(invalid) > 0) {
    
    stop(
      
      "Invalid broad topics returned: ",
      paste(invalid, collapse = ", ")
      
    )
    
  }
  
  tibble::tibble(
    broad_topics = list(parsed$broad_topics),
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