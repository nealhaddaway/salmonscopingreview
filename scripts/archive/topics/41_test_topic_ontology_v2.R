# =============================================================================
# File: 41_test_topic_ontology_v2.R
# Project: salmonscopingreview
# Purpose: Test the draft systems-based topic ontology on 20 records without
#          altering or resuming the full-corpus topic run
# =============================================================================

source("scripts/00_setup.R")
source("R/read_corpus.R")

input_records <- here::here(
  "data_raw",
  "INCLUDES fixed abstracts.txt"
)

input_old_results <- here::here(
  "outputs",
  "stage_4_llm",
  "full_corpus",
  "llm_topic_assignments_record.csv"
)

input_ontology <- here::here(
  "data_raw",
  "topic_ontology_v2_draft.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_4_llm",
  "ontology_v2_pilot"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(input_records),
  file.exists(input_old_results),
  file.exists(input_ontology)
)

api_key <- Sys.getenv("OPENAI_API_KEY")

if (!nzchar(api_key)) {
  stop("OPENAI_API_KEY not found.")
}

records <- read_corpus(input_records) |>
  dplyr::mutate(
    record_id = as.character(record_id)
  )

old_results <- readr::read_csv(
  input_old_results,
  show_col_types = FALSE
) |>
  dplyr::mutate(
    record_id = as.character(record_id)
  )

ontology <- readr::read_csv(
  input_ontology,
  show_col_types = FALSE
)

required_columns <- c(
  "path_id",
  "hierarchy_path",
  "definition",
  "include_when",
  "exclude_when",
  "examples"
)

missing_columns <- setdiff(
  required_columns,
  names(ontology)
)

if (length(missing_columns) > 0L) {
  stop(
    "Ontology is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

# Fixed 20-record sample:
# 10 records previously assigned only Research methods, plus
# 10 other completed records selected randomly.
set.seed(20260804)

research_only <- old_results |>
  dplyr::filter(
    !classification_failed,
    stringr::str_squish(
      dplyr::coalesce(assigned_broad_topics, "")
    ) == "Research methods"
  ) |>
  {
    n_sample <- min(10L, dplyr::nrow(.))
    dplyr::slice_sample(., n = n_sample)
  } |>
  dplyr::mutate(
    pilot_stratum = "Previously Research methods only"
  )

other_records <- old_results |>
  dplyr::filter(
    !classification_failed,
    stringr::str_squish(
      dplyr::coalesce(assigned_broad_topics, "")
    ) != "Research methods"
  ) |>
  {
    n_sample <- min(10L, dplyr::nrow(.))
    dplyr::slice_sample(., n = n_sample)
  } |>
  dplyr::mutate(
    pilot_stratum = "Other completed record"
  )

pilot_records <- dplyr::bind_rows(
  research_only,
  other_records
) |>
  dplyr::select(
    pilot_stratum,
    record_sequence,
    record_id,
    title,
    abstract,
    old_assigned_paths = assigned_paths
  ) |>
  dplyr::arrange(
    pilot_stratum,
    record_sequence
  )

ontology_lines <- ontology |>
  dplyr::transmute(
    line = paste0(
      path_id,
      " | ",
      hierarchy_path,
      "\nDefinition: ",
      definition,
      "\nInclude when: ",
      include_when,
      "\nExclude when: ",
      exclude_when,
      "\nExamples: ",
      examples
    )
  ) |>
  dplyr::pull(line)

ontology_prompt <- paste(
  ontology_lines,
  collapse = "\n\n"
)

system_prompt <- paste(
  "You classify title-and-abstract records for a systematic map of salmon",
  "aquaculture research using a systems-based, question-centred ontology.",
  "",
  "Select every path that represents a substantive research question or",
  "substantive outcome investigated by the paper.",
  "Multiple paths are allowed across different domains and, where genuinely",
  "necessary, within the same domain.",
  "",
  "Do not select paths for incidental, contextual or background-only mentions.",
  "Do not select a parent-like general concept when a more precise supplied",
  "path captures the substantive question.",
  "Species and geography are annotated separately.",
  "",
  "METHODS RULE:",
  "Assign a Methods path only when development, validation, comparison or",
  "review of the method is itself the principal contribution of the paper.",
  "Never assign Methods merely because a paper uses laboratory, diagnostic,",
  "monitoring, modelling, statistical, genomic or imaging methods to answer",
  "a substantive question.",
  "",
  "Use only path_id values from the supplied ontology.",
  "Return an empty assignments array only if no supplied path is substantively",
  "applicable.",
  sep = "\n"
)

response_schema <- list(
  type = "object",
  properties = list(
    assignments = list(
      type = "array",
      items = list(
        type = "object",
        properties = list(
          path_id = list(
            type = "string",
            enum = ontology$path_id
          ),
          confidence = list(
            type = "string",
            enum = c("high", "medium", "low")
          ),
          evidence = list(type = "string")
        ),
        required = c(
          "path_id",
          "confidence",
          "evidence"
        ),
        additionalProperties = FALSE
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

classify_one <- function(
    pilot_stratum,
    record_sequence,
    record_id,
    title,
    abstract,
    old_assigned_paths
) {

  user_prompt <- paste0(
    "ONTOLOGY\n",
    ontology_prompt,
    "\n\nRECORD\nTitle: ",
    title,
    "\n\nAbstract: ",
    abstract,
    "\n\nReturn substantive ontology paths only."
  )

  body <- list(
    model = "gpt-5-mini",
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
        name = "topic_ontology_v2_pilot",
        strict = TRUE,
        schema = response_schema
      )
    )
  )

  parsed <- tryCatch(
    {
      response <- httr2::request(
        "https://api.openai.com/v1/responses"
      ) |>
        httr2::req_auth_bearer_token(api_key) |>
        httr2::req_body_json(
          body,
          auto_unbox = TRUE
        ) |>
        httr2::req_timeout(180) |>
        httr2::req_retry(
          max_tries = 3,
          backoff = ~ 2^.x
        ) |>
        httr2::req_perform() |>
        httr2::resp_body_json()

      message_items <- response$output[
        vapply(
          response$output,
          function(item) identical(
            item$type,
            "message"
          ),
          logical(1)
        )
      ]

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
            identical(
              item$type,
              "output_text"
            ) && !is.null(item$text)
          },
          logical(1)
        )
      ]

      jsonlite::fromJSON(
        text_items[[1]]$text,
        simplifyVector = FALSE
      )
    },
    error = function(e) {
      structure(
        list(message = conditionMessage(e)),
        class = "classification_error"
      )
    }
  )

  if (inherits(parsed, "classification_error")) {
    return(
      tibble::tibble(
        pilot_stratum = pilot_stratum,
        record_sequence = record_sequence,
        record_id = record_id,
        title = title,
        abstract = abstract,
        old_assigned_paths = old_assigned_paths,
        new_assigned_paths = NA_character_,
        new_evidence = NA_character_,
        review_required = TRUE,
        review_reason = NA_character_,
        classification_failed = TRUE,
        classification_error = parsed$message
      )
    )
  }

  assignment_ids <- vapply(
    parsed$assignments,
    function(x) x$path_id,
    character(1)
  )

  assignment_evidence <- vapply(
    parsed$assignments,
    function(x) paste0(
      x$path_id,
      " [",
      x$confidence,
      "]: ",
      x$evidence
    ),
    character(1)
  )

  new_paths <- ontology |>
    dplyr::filter(
      path_id %in% assignment_ids
    ) |>
    dplyr::arrange(
      match(path_id, assignment_ids)
    ) |>
    dplyr::pull(hierarchy_path)

  tibble::tibble(
    pilot_stratum = pilot_stratum,
    record_sequence = record_sequence,
    record_id = record_id,
    title = title,
    abstract = abstract,
    old_assigned_paths = old_assigned_paths,
    new_assigned_paths = if (
      length(new_paths) == 0L
    ) {
      NA_character_
    } else {
      paste(
        new_paths,
        collapse = "; "
      )
    },
    new_evidence = if (
      length(assignment_evidence) == 0L
    ) {
      NA_character_
    } else {
      paste(
        assignment_evidence,
        collapse = "\n"
      )
    },
    review_required = parsed$review_required,
    review_reason = if (
      is.null(parsed$review_reason)
    ) {
      NA_character_
    } else {
      parsed$review_reason
    },
    classification_failed = FALSE,
    classification_error = NA_character_
  )
}

pilot_results <- purrr::pmap_dfr(
  pilot_records,
  classify_one,
  .progress = TRUE
) |>
  dplyr::mutate(
    substantive_concepts_captured = NA_character_,
    methods_rule_correct = NA_character_,
    missing_or_incorrect_paths = NA_character_,
    validation_notes = NA_character_
  )

csv_path <- fs::path(
  output_dir,
  "topic_ontology_v2_pilot_20.csv"
)

xlsx_path <- fs::path(
  output_dir,
  "topic_ontology_v2_pilot_20.xlsx"
)

readr::write_csv(
  pilot_results,
  csv_path,
  na = ""
)

wb <- openxlsx2::wb_workbook()

wb$add_worksheet("Pilot 20")
wb$add_data(
  "Pilot 20",
  pilot_results
)
wb$freeze_pane(
  sheet = "Pilot 20",
  first_row = TRUE
)

wb$add_worksheet("Ontology v2")
wb$add_data(
  "Ontology v2",
  ontology
)
wb$freeze_pane(
  sheet = "Ontology v2",
  first_row = TRUE
)

wb$add_worksheet("Instructions")
wb$add_data(
  "Instructions",
  tibble::tribble(
    ~field, ~instruction,
    "substantive_concepts_captured",
    "Enter Yes, Partial or No: does the new coding capture the substantive research questions and outcomes?",
    "methods_rule_correct",
    "Enter Yes or No: was Methods used only for genuinely methodological research?",
    "missing_or_incorrect_paths",
    "List any missing paths or paths that should be removed.",
    "validation_notes",
    "Brief explanation or ontology revision suggestion."
  )
)

wb$save(
  xlsx_path,
  overwrite = TRUE
)

message("Topic ontology v2 pilot completed.")
message("Pilot records: ", nrow(pilot_results))
message(
  "Previously Research methods only: ",
  sum(
    pilot_results$pilot_stratum ==
      "Previously Research methods only"
  )
)
message(
  "Other completed records: ",
  sum(
    pilot_results$pilot_stratum ==
      "Other completed record"
  )
)
message(
  "Classification failures: ",
  sum(pilot_results$classification_failed)
)
message("Workbook: ", xlsx_path)
