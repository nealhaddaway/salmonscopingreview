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
  mutate(record_id = as.character(record_id), title = coalesce(as.character(title), ""), abstract = coalesce(as.character(abstract), ""))
required <- c("record_id", "title", "abstract")
missing <- setdiff(required, names(records))
if (length(missing)) stop("Input is missing: ", paste(missing, collapse = ", "), call. = FALSE)

system_prompt <- paste(
  "You are screening titles and abstracts for a living evidence map of salmon farming.", "",
  "RETAIN when salmon farming is a substantive focus and it concerns eligible farmed salmonids: Atlantic salmon; Pacific salmon species including Chinook, coho, sockeye, chum, pink and masu salmon; rainbow trout; or genuinely unspecified farmed salmon.",
  "Eligible records may concern any substantive aspect of farming, production, inputs, fish health, welfare, environmental pressures or impacts, products, economics, governance, labour, communities, consumers, or research methods specifically applied to eligible salmon farming.",
  "EXCLUDE when the study concerns only wild salmonids, capture fisheries or conservation; salmon farming is only background/context/passing example; only non-eligible aquaculture species; basic salmon biology without substantive farming context; or the title/abstract clearly does not concern eligible salmon farming.",
  "For mixed-species studies, RETAIN if eligible farmed salmonids are a substantive part of the evidence, analysis or conclusions.",
  "Reviews, systematic reviews, meta-analyses, policy papers and synthesis papers are eligible when eligible salmon farming is a substantive focus.",
  "Use UNCERTAIN only as a last resort. Choose RETAIN whenever eligibility is more defensible than ineligibility; EXCLUDE whenever ineligibility is more defensible than eligibility.",
  "Decision hierarchy: 1 clearly eligible=RETAIN; 2 clearly ineligible=EXCLUDE; 3 otherwise=UNCERTAIN.",
  "Base the decision only on supplied title and abstract. Give one concise reason.", sep = "\n")

# Strict JSON schema for the Responses API. The top-level value is an object,
# with the decisions held in the required `results` array.
result_item_schema <- list(
  type = "object",
  properties = list(
    record_id = list(type = "string"),
    decision = list(type = "string", enum = c("retain", "exclude", "uncertain")),
    reason = list(type = "string")
  ),
  required = c("record_id", "decision", "reason"),
  additionalProperties = FALSE
)

schema <- list(
  type = "object",
  properties = list(
    results = list(
      type = "array",
      items = result_item_schema
    )
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

call_llm <- function(body, batch_ids) {
  req <- request("https://api.openai.com/v1/responses") |>
    req_auth_bearer_token(api_key) |>
    req_body_json(body, auto_unbox = TRUE) |>
    req_timeout(180) |>
    req_retry(max_tries = 4, backoff = ~ 2^.x)
  response <- tryCatch(req |> req_error(is_error = function(resp) FALSE) |> req_perform(), error = function(e) stop("LLM transport failure for batch ", paste(batch_ids, collapse = ", "), ": ", conditionMessage(e), call. = FALSE))
  status <- resp_status(response)
  if (status < 200 || status >= 300) {
    body_text <- tryCatch(resp_body_string(response), error = function(e) "<unable to read response body>")
    stop("LLM API returned HTTP ", status, " for batch ", paste(batch_ids, collapse = ", "), ". Response: ", body_text, call. = FALSE)
  }
  parsed_json <- tryCatch(resp_body_json(response), error = function(e) stop("Invalid JSON response for batch ", paste(batch_ids, collapse = ", "), ": ", conditionMessage(e), call. = FALSE))
  output_text <- extract_output(parsed_json)
  parsed <- tryCatch(jsonlite::fromJSON(output_text, simplifyVector = TRUE), error = function(e) stop("Invalid structured LLM output for batch ", paste(batch_ids, collapse = ", "), ": ", conditionMessage(e), call. = FALSE))
  if (!is.list(parsed) || is.null(parsed$results) || !is.data.frame(parsed$results)) stop("Structured LLM output did not contain a usable `results` object for batch ", paste(batch_ids, collapse = ", "), call. = FALSE)
  parsed$results
}

batch_size <- 10L
out <- vector("list", ceiling(nrow(records) / batch_size))
for (start in seq(1L, nrow(records), by = batch_size)) {
  end <- min(start + batch_size - 1L, nrow(records)); batch <- records[start:end, , drop = FALSE]
  blocks <- vapply(seq_len(nrow(batch)), function(i) paste0("RECORD_ID: ", batch$record_id[[i]], "\nTITLE\n", batch$title[[i]], "\n\nABSTRACT\n", batch$abstract[[i]], "\n"), character(1))
  user_prompt <- paste0("Screen every record independently. Return exactly one result for every supplied RECORD_ID. Do not omit, merge, or invent records.\n\n", paste(blocks, collapse = "\n---\n"))
  body <- list(model = "gpt-5-mini", store = FALSE, reasoning = list(effort = "low"), input = list(list(role = "system", content = list(list(type = "input_text", text = system_prompt))), list(role = "user", content = list(list(type = "input_text", text = user_prompt)))), text = list(format = list(type = "json_schema", name = "salmon_farming_relevance_batch", strict = TRUE, schema = schema), verbosity = "low"))
  parsed <- call_llm(body, batch$record_id)
  parsed <- as_tibble(parsed) |> transmute(record_id = as.character(record_id), llm_decision = as.character(decision), llm_reason = as.character(reason), llm_failed = FALSE, llm_error = NA_character_)
  if (nrow(parsed) != nrow(batch) || !isTRUE(all(sort(parsed$record_id) == sort(batch$record_id)))) stop("Batch response IDs did not exactly match input IDs for batch ", start, "-", end, call. = FALSE)
  out[[ceiling(start / batch_size)]] <- parsed
  message(sprintf("LLM adjudication: %d/%d records complete.", end, nrow(records)))
}

results <- bind_rows(out) |> left_join(records |> select(record_id, title, abstract), by = "record_id") |> relocate(record_id, title, abstract, llm_decision, llm_reason, llm_failed, llm_error)
if (any(results$llm_failed) || anyNA(results$llm_decision)) stop("LLM adjudication contains failed/missing decisions; refusing to produce a reviewable uncertainty set.", call. = FALSE)
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
write_csv(results, output_path, na = "")
message(sprintf("Wrote %d adjudications to %s", nrow(results), output_path))
