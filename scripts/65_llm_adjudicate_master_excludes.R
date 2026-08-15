#!/usr/bin/env Rscript
# LLM adjudication of master relevance-model automatic exclusions.
# Uses the LivingEvidenceMap salmon-farming screening protocol as the specification,
# but keeps this audit runner in salmonscopingreview.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(jsonlite)
  library(httr2)
  library(tibble)
})

args <- commandArgs(trailingOnly = TRUE)
input_path <- if (length(args) >= 1) args[[1]] else "outputs/master_relevance_audit/master_automatic_excludes.csv"
output_path <- if (length(args) >= 2) args[[2]] else "outputs/master_relevance_audit/master_automatic_excludes_llm_adjudication.csv"

api_key <- Sys.getenv("OPENAI_API_KEY")
if (!nzchar(api_key)) stop("OPENAI_API_KEY was not found.", call. = FALSE)

records <- readr::read_csv(input_path, show_col_types = FALSE) |>
  mutate(record_id = as.character(record_id),
         title = coalesce(as.character(title), ""),
         abstract = coalesce(as.character(abstract), ""))

required <- c("record_id", "title", "abstract")
missing <- setdiff(required, names(records))
if (length(missing)) stop("Input is missing: ", paste(missing, collapse = ", "), call. = FALSE)

system_prompt <- paste(
  "You are screening titles and abstracts for a living evidence map of salmon farming.",
  "",
  "RETAIN a record when salmon farming is a substantive focus and it concerns one or more eligible farmed salmonids:",
  "- Atlantic salmon",
  "- Pacific salmon species, including Chinook, coho, sockeye, chum, pink and masu salmon",
  "- rainbow trout",
  "- farmed salmon where the species is not specified",
  "",
  "Eligible records may concern any substantive aspect of farming, production, inputs, fish health, welfare, environmental pressures or impacts, products, economics, governance, labour, communities, consumers, or research methods specifically applied to eligible salmon farming.",
  "",
  "EXCLUDE when:",
  "- the study concerns only wild salmonids, capture fisheries or conservation;",
  "- salmon farming is only background, context or a passing example;",
  "- it concerns only non-eligible aquaculture species;",
  "- it concerns basic salmon biology without a substantive farming context;",
  "- the available title and abstract clearly do not concern eligible salmon farming.",
  "",
  "For mixed-species studies, RETAIN if eligible farmed salmonids are a substantive part of the evidence, analysis or conclusions.",
  "",
  "Reviews, systematic reviews, meta-analyses, policy papers and synthesis papers are eligible when eligible salmon farming is a substantive focus, even if other aquaculture species, fisheries or food systems are also discussed.",
  "",
  "Use UNCERTAIN only as a last resort. Choose RETAIN whenever the available title and abstract make eligibility more defensible than ineligibility. Choose EXCLUDE whenever the available title and abstract make ineligibility more defensible than eligibility.",
  "",
  "DECISION HIERARCHY",
  "1. If clearly eligible, choose RETAIN.",
  "2. Otherwise, if clearly ineligible, choose EXCLUDE.",
  "3. Otherwise, choose UNCERTAIN.",
  "",
  "Base the decision only on the supplied title and abstract. Give one concise reason.",
  sep = "\n"
)

schema <- list(
  type = "object",
  properties = list(
    results = list(type = "array", items = list(
      type = "object",
      properties = list(
        record_id = list(type = "string"),
        decision = list(type = "string", enum = c("retain", "exclude", "uncertain")),
        reason = list(type = "string")
      ),
      required = c("record_id", "decision", "reason"),
      additionalProperties = FALSE
    ))
  ),
  required = c("results"),
  additionalProperties = FALSE
)

extract_output <- function(response) {
  messages <- response$output[vapply(response$output, function(x) identical(x$type, "message"), logical(1))]
  content <- unlist(lapply(messages, function(x) x$content), recursive = FALSE)
  text <- content[vapply(content, function(x) identical(x$type, "output_text") && !is.null(x$text), logical(1))]
  if (!length(text)) stop("No output_text item was returned.")
  text[[1]]$text
}

# Process in small batches to avoid oversized requests and make failures recoverable.
batch_size <- 10L
out <- vector("list", ceiling(nrow(records) / batch_size))

for (start in seq(1L, nrow(records), by = batch_size)) {
  end <- min(start + batch_size - 1L, nrow(records))
  batch <- records[start:end, , drop = FALSE]
  blocks <- vapply(seq_len(nrow(batch)), function(i) paste0(
    "RECORD_ID: ", batch$record_id[[i]], "\nTITLE\n", batch$title[[i]],
    "\n\nABSTRACT\n", batch$abstract[[i]], "\n"
  ), character(1))
  user_prompt <- paste0(
    "Screen every record independently. Return exactly one result for every supplied RECORD_ID. ",
    "Do not omit, merge, or invent records.\n\n", paste(blocks, collapse = "\n---\n")
  )
  body <- list(
    model = "gpt-5-mini", store = FALSE, reasoning = list(effort = "low"),
    input = list(
      list(role = "system", content = list(list(type = "input_text", text = system_prompt))),
      list(role = "user", content = list(list(type = "input_text", text = user_prompt)))
    ),
    text = list(verbosity = "low", format = list(type = "json_schema", name = "salmon_farming_relevance_batch", strict = TRUE, schema = schema))
  )

  parsed <- tryCatch({
    response <- request("https://api.openai.com/v1/responses") |>
      req_auth_bearer_token(api_key) |>
      req_body_json(body, auto_unbox = TRUE) |>
      req_timeout(180) |>
      req_retry(max_tries = 4, backoff = ~ 2^.x) |>
      req_perform() |>
      resp_body_json()
    jsonlite::fromJSON(extract_output(response), simplifyVector = TRUE)$results
  }, error = function(e) {
    tibble(record_id = batch$record_id, llm_decision = "uncertain", llm_reason = NA_character_, llm_failed = TRUE, llm_error = conditionMessage(e))
  })

  if (!"llm_decision" %in% names(parsed)) {
    parsed <- as_tibble(parsed) |>
      transmute(record_id = as.character(record_id), llm_decision = as.character(decision), llm_reason = as.character(reason), llm_failed = FALSE, llm_error = NA_character_)
  }
  parsed$record_id <- as.character(parsed$record_id)
  if (!isTRUE(all(sort(parsed$record_id) == sort(batch$record_id)))) stop("Batch response IDs did not match input IDs.")
  out[[ceiling(start / batch_size)]] <- as_tibble(parsed)
  message(sprintf("LLM adjudication: %d/%d records complete.", end, nrow(records)))
}

results <- bind_rows(out) |>
  left_join(records |> select(record_id, title, abstract), by = "record_id") |>
  left_join(records |> select(record_id, everything()), by = "record_id", suffix = c("", ".input")) |>
  select(-ends_with(".input")) |>
  relocate(record_id, title, abstract, llm_decision, llm_reason, llm_failed, llm_error)

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
write_csv(results, output_path, na = "")
message(sprintf("Wrote %d adjudications to %s", nrow(results), output_path))
