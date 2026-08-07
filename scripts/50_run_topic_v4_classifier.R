# =============================================================================
# File: scripts/50_run_topic_v4_classifier.R
# Purpose: Run the final topic classifier using the frozen ontology and
#          conservative substantive-coding rules.
# =============================================================================

source("scripts/00_setup.R")

ontology_file <- here::here(
  "data_raw",
  "topic_ontology_v3.csv"
)

records_file <- here::here(
  "data_raw",
  "topic_validation_adjudicated_50.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_4_llm",
  "ontology_v4_validation"
)

fs::dir_create(output_dir)

checkpoint_file <- fs::path(
  output_dir,
  "topic_v4_checkpoint.rds"
)

record_output_file <- fs::path(
  output_dir,
  "topic_v4_llm_record.csv"
)

long_output_file <- fs::path(
  output_dir,
  "topic_v4_llm_long.csv"
)

failure_file <- fs::path(
  output_dir,
  "topic_v4_failures.csv"
)

system_prompt_file <- fs::path(
  output_dir,
  "topic_v4_system_prompt.txt"
)

ontology_prompt_file <- fs::path(
  output_dir,
  "topic_v4_ontology_prompt.txt"
)

stopifnot(
  file.exists(ontology_file),
  file.exists(records_file)
)

api_key <- Sys.getenv("OPENAI_API_KEY")

if (!nzchar(api_key)) {
  stop("OPENAI_API_KEY was not found.")
}

ontology <- readr::read_csv(
  ontology_file,
  show_col_types = FALSE
)

records <- readr::read_csv(
  records_file,
  show_col_types = FALSE
) |>
  dplyr::mutate(
    record_id = as.character(record_id),
    title = dplyr::coalesce(as.character(title), ""),
    abstract = dplyr::coalesce(as.character(abstract), "")
  ) |>
  dplyr::select(
    sampling_stratum,
    record_sequence,
    record_id,
    title,
    abstract
  )

required_columns <- c(
  "path_id",
  "hierarchy_path",
  "definition",
  "include_when",
  "exclude_when",
  "required_subject_terms",
  "required_focus_terms",
  "alternative_standalone_cues",
  "supporting_terms_from_old_ontology",
  "prompt_logic_note"
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

if (anyDuplicated(ontology$path_id) > 0L) {
  stop("Duplicate path_id values found.")
}

if (anyDuplicated(ontology$hierarchy_path) > 0L) {
  stop("Duplicate hierarchy_path values found.")
}

field_line <- function(label, value) {
  value <- dplyr::coalesce(as.character(value), "")
  if (!nzchar(trimws(value))) {
    return(NULL)
  }
  paste0(label, ": ", value)
}

ontology_entries <- purrr::pmap_chr(
  ontology |>
    dplyr::select(
      path_id,
      hierarchy_path,
      definition,
      include_when,
      exclude_when,
      required_subject_terms,
      required_focus_terms,
      alternative_standalone_cues,
      supporting_terms_from_old_ontology,
      prompt_logic_note
    ),
  function(
    path_id,
    hierarchy_path,
    definition,
    include_when,
    exclude_when,
    required_subject_terms,
    required_focus_terms,
    alternative_standalone_cues,
    supporting_terms_from_old_ontology,
    prompt_logic_note
  ) {
    lines <- c(
      paste0(path_id, " | ", hierarchy_path),
      field_line("Definition", definition),
      field_line("Include when", include_when),
      field_line("Exclude when", exclude_when),
      field_line("Subject concept cues", required_subject_terms),
      field_line("Focus concept cues", required_focus_terms),
      field_line("Alternative specific cues", alternative_standalone_cues),
      field_line("Supporting lexical cues", supporting_terms_from_old_ontology),
      field_line("Interpretation note", prompt_logic_note)
    )
    paste(lines[!vapply(lines, is.null, logical(1))], collapse = "\n")
  }
)

ontology_prompt <- paste(
  ontology_entries,
  collapse = "\n\n"
)

system_prompt <- paste(
  "You are coding titles and abstracts for a systematic map of salmon",
  "aquaculture research using a fixed systems-based ontology.",
  "",
  "OBJECTIVE",
  "Assign an ontology pathway only when it represents a substantive research",
  "question, intervention, outcome or conclusion investigated by the study.",
  "A concept is not substantive merely because it is mentioned, measured,",
  "controlled for, used as background, or discussed in the introduction.",
  "",
  "DECISION PROCESS FOR EVERY PROPOSED CODE",
  "1. Is the concept only background, motivation, context or prior literature?",
  "   If yes, do not code it.",
  "2. Is the concept only a variable measured to evaluate another intervention",
  "   or exposure? If yes, do not code it as a separate topic.",
  "3. Is the concept itself a substantive research question, intervention,",
  "   outcome or conclusion? If yes, code it.",
  "4. Would removing the code materially misrepresent the paper's contribution?",
  "   If no, do not assign it.",
  "",
  "MULTIPLE CODING",
  "5. Code all genuinely substantive concepts, including meaningful secondary",
  "   outcomes, but do not code every measurement.",
  "6. The ontology is not hierarchical for coding purposes. Do not assign a",
  "   broader or related pathway merely because a more specific pathway applies.",
  "7. Each assigned pathway must independently satisfy the substantive-coding",
  "   rules.",
  "",
  "CONTRASTIVE EXAMPLES",
  "8. Feed-additive trial measuring cortisol: code Feed additives. Do not also",
  "   code Stress unless stress is a substantive research question or conclusion.",
  "9. Vaccine trial measuring antibody titres: code disease prevention/treatment.",
  "   Do not also code physiology unless physiological response is substantive.",
  "10. Sea-lice treatment trial measuring growth: code sea-lice control and",
  "    treatment. Do not also code sea-lice impacts unless impacts are a",
  "    substantive objective or conclusion.",
  "11. Benthic chemistry measured only to evaluate fallowing does not by itself",
  "    justify environmental monitoring as a separate topic.",
  "",
  "LEXICAL ANCHORS",
  "12. The ontology provides subject cues, focus cues, alternative specific",
  "    cues and supporting lexical cues.",
  "13. These are semantic guidance, not literal search rules.",
  "14. Where both subject and focus cues are supplied, infer both concepts",
  "    before assigning the pathway. Generic focus words alone are insufficient.",
  "15. Alternative specific cues can identify a pathway without the ordinary",
  "    subject wording when the context clearly establishes the concept.",
  "",
  "BOUNDARIES",
  "16. Cleaner-fish, lumpfish or wrasse studies in salmon farming normally",
  "    concern sea-lice control, even when sea lice are implicit.",
  "17. Distinguish sea-lice epidemiology, control/treatment, fish response",
  "    and impacts by the substantive question.",
  "18. Distinguish other-disease epidemiology, diagnosis/detection,",
  "    prevention/treatment and general disease biology.",
  "19. Product safety and health effects of eating salmon belong under Product.",
  "    Public-health effects caused by farming belong under People and society.",
  "20. General or multiple-issue pathways are exceptional. Use them only when",
  "    the paper genuinely investigates multiple issues in that domain or",
  "    provides a broad synthesis. Never assign a General pathway simply",
  "    because a specific pathway belongs within it.",
  "21. Methods applies only where methodological development, validation,",
  "    comparison or review is itself a principal contribution.",
  "22. For reviews, code each theme actually synthesised or critically",
  "    evaluated, not topics mentioned only for context.",
  "",
  "OUTPUT",
  "23. Use only supplied path_id values.",
  "24. For each assignment, provide one concise evidence-based reason.",
  "25. Set review_required to true only where the abstract is genuinely",
  "    ambiguous, truncated, insufficient or lacks an appropriate pathway.",
  sep = "\n"
)

readr::write_lines(system_prompt, system_prompt_file)
readr::write_lines(ontology_prompt, ontology_prompt_file)

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
          reason = list(type = "string")
        ),
        required = c("path_id", "reason"),
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

extract_output_text <- function(response) {
  message_items <- response$output[
    vapply(
      response$output,
      function(item) identical(item$type, "message"),
      logical(1)
    )
  ]

  content_items <- unlist(
    lapply(message_items, function(item) item$content),
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
    stop("No output_text item was returned.")
  }

  text_items[[1]]$text
}

classify_record <- function(
    sampling_stratum,
    record_sequence,
    record_id,
    title,
    abstract
) {
  user_prompt <- paste0(
    "ONTOLOGY\n\n",
    ontology_prompt,
    "\n\nRECORD\n\nTitle: ",
    title,
    "\n\nAbstract: ",
    abstract,
    "\n\nReturn the substantive ontology assignments."
  )

  body <- list(
    model = "gpt-5-mini",
    store = FALSE,
    reasoning = list(effort = "medium"),
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
      verbosity = "low",
      format = list(
        type = "json_schema",
        name = "topic_v4",
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
        httr2::req_body_json(body, auto_unbox = TRUE) |>
        httr2::req_timeout(180) |>
        httr2::req_retry(
          max_tries = 4,
          backoff = ~ 2^.x
        ) |>
        httr2::req_perform() |>
        httr2::resp_body_json()

      jsonlite::fromJSON(
        extract_output_text(response),
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
    return(list(
      record = tibble::tibble(
        sampling_stratum = sampling_stratum,
        record_sequence = record_sequence,
        record_id = record_id,
        title = title,
        abstract = abstract,
        assigned_path_ids = NA_character_,
        assigned_paths = NA_character_,
        assignment_count = 0L,
        review_required = TRUE,
        review_reason = NA_character_,
        classification_failed = TRUE,
        classification_error = parsed$message
      ),
      long = tibble::tibble()
    ))
  }

  assignments <- parsed$assignments
  ids <- unique(vapply(
    assignments,
    function(x) x$path_id,
    character(1)
  ))

  selected <- ontology |>
    dplyr::filter(path_id %in% ids) |>
    dplyr::arrange(match(path_id, ids))

  unique_assignments <- assignments[
    !duplicated(vapply(
      assignments,
      function(x) x$path_id,
      character(1)
    ))
  ]

  long <- if (length(unique_assignments) == 0L) {
    tibble::tibble()
  } else {
    tibble::tibble(
      sampling_stratum = sampling_stratum,
      record_sequence = record_sequence,
      record_id = record_id,
      title = title,
      abstract = abstract,
      path_id = vapply(
        unique_assignments,
        function(x) x$path_id,
        character(1)
      ),
      reason = vapply(
        unique_assignments,
        function(x) x$reason,
        character(1)
      )
    ) |>
      dplyr::left_join(
        ontology |>
          dplyr::select(path_id, hierarchy_path),
        by = "path_id"
      )
  }

  record <- tibble::tibble(
    sampling_stratum = sampling_stratum,
    record_sequence = record_sequence,
    record_id = record_id,
    title = title,
    abstract = abstract,
    assigned_path_ids = if (nrow(selected) == 0L) {
      NA_character_
    } else {
      paste(selected$path_id, collapse = "; ")
    },
    assigned_paths = if (nrow(selected) == 0L) {
      NA_character_
    } else {
      paste(selected$hierarchy_path, collapse = "; ")
    },
    assignment_count = nrow(selected),
    review_required = parsed$review_required,
    review_reason = if (is.null(parsed$review_reason)) {
      NA_character_
    } else {
      parsed$review_reason
    },
    classification_failed = FALSE,
    classification_error = NA_character_
  )

  list(record = record, long = long)
}

if (file.exists(checkpoint_file)) {
  checkpoint <- readRDS(checkpoint_file)
  record_results <- checkpoint$record_results
  long_results <- checkpoint$long_results
  completed_ids <- unique(record_results$record_id)

  message(
    "Resuming round 2 from checkpoint: ",
    length(completed_ids),
    " / ",
    nrow(records),
    " records completed."
  )
} else {
  record_results <- tibble::tibble()
  long_results <- tibble::tibble()
  completed_ids <- character()

  message("Starting final topic classifier validation.")
}

remaining <- records |>
  dplyr::filter(!record_id %in% completed_ids)

for (i in seq_len(nrow(remaining))) {
  current <- remaining[i, ]

  message(
    "Classifying ",
    length(completed_ids) + i,
    " / ",
    nrow(records),
    ": record_id ",
    current$record_id
  )

  result <- classify_record(
    sampling_stratum = current$sampling_stratum,
    record_sequence = current$record_sequence,
    record_id = current$record_id,
    title = current$title,
    abstract = current$abstract
  )

  record_results <- dplyr::bind_rows(
    record_results,
    result$record
  )

  long_results <- dplyr::bind_rows(
    long_results,
    result$long
  )

  saveRDS(
    list(
      record_results = record_results,
      long_results = long_results
    ),
    checkpoint_file
  )

  readr::write_csv(
    record_results |>
      dplyr::arrange(record_sequence),
    record_output_file,
    na = ""
  )

  readr::write_csv(
    long_results |>
      dplyr::arrange(record_sequence, path_id),
    long_output_file,
    na = ""
  )

  readr::write_csv(
    record_results |>
      dplyr::filter(classification_failed),
    failure_file,
    na = ""
  )
}

message("")
message("Final topic classifier validation completed.")
message("Records: ", nrow(record_results))
message(
  "Failures: ",
  sum(record_results$classification_failed)
)
message("Record output: ", record_output_file)
message("Long output: ", long_output_file)
message("")
message(
  "Next run: source(\"scripts/46_compare_topic_v4.R\")"
)
