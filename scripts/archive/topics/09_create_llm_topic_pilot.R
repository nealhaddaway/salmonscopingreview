# Stage 4.2: create LLM topic-classification pilot batch ----------------------

source("scripts/00_setup.R")
source("R/read_corpus.R")

# Inputs ----------------------------------------------------------------------

input_records <- here::here(
  "data_raw",
  "INCLUDES fixed abstracts.txt"
)

input_ontology <- here::here(
  "outputs",
  "stage_4_llm",
  "llm_topic_ontology.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_4_llm",
  "pilot"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(input_records),
  file.exists(input_ontology)
)

records <- read_corpus(input_records)

ontology <- readr::read_csv(
  input_ontology,
  show_col_types = FALSE
)

required_ontology_columns <- c(
  "path_id",
  "broad_topic",
  "subtopic",
  "feature",
  "component",
  "hierarchy_path",
  "supporting_terms"
)

missing_ontology_columns <- setdiff(
  required_ontology_columns,
  names(ontology)
)

if (length(missing_ontology_columns) > 0L) {
  stop(
    "Ontology is missing required columns: ",
    paste(missing_ontology_columns, collapse = ", ")
  )
}

# Compact the ontology for prompting ------------------------------------------

representative_terms <- function(x, maximum_terms = 8L) {

  terms <- stringr::str_split(
    dplyr::coalesce(x, ""),
    ";\\s*"
  )[[1]]

  terms <- terms[
    nzchar(terms)
  ]

  terms <- terms[
    !duplicated(stringr::str_to_lower(terms))
  ]

  paste(
    utils::head(terms, maximum_terms),
    collapse = "; "
  )
}

ontology_prompt_tbl <- ontology |>
  dplyr::rowwise() |>
  dplyr::mutate(
    representative_terms = representative_terms(
      supporting_terms,
      maximum_terms = 8L
    ),
    prompt_line = paste0(
      path_id,
      " | ",
      hierarchy_path,
      " | Examples: ",
      representative_terms
    )
  ) |>
  dplyr::ungroup()

ontology_prompt <- paste(
  ontology_prompt_tbl$prompt_line,
  collapse = "\n"
)

# Fixed pilot sample -----------------------------------------------------------

set.seed(20260801)

pilot_records <- records |>
  dplyr::filter(
    !is.na(title),
    nzchar(title),
    !is.na(abstract),
    nzchar(abstract)
  ) |>
  dplyr::slice_sample(n = 20L) |>
  dplyr::select(
    record_sequence,
    record_id,
    title,
    abstract
  ) |>
  dplyr::arrange(record_sequence)

readr::write_csv(
  pilot_records,
  fs::path(
    output_dir,
    "llm_topic_pilot_records_20.csv"
  ),
  na = ""
)

# Prompt ----------------------------------------------------------------------

system_prompt <- paste(
  "You classify scientific records for a systematic evidence map of",
  "farmed salmon and rainbow trout research.",
  "",
  "Select only topics that are substantive subjects of the study.",
  "Do not assign a topic merely because a related word appears.",
  "Ignore incidental, contextual and background-only mentions.",
  "Use only path_id values from the supplied ontology.",
  "Multiple paths are allowed when the study genuinely investigates",
  "multiple topics.",
  "Return an empty assignments array when no supplied path is appropriate.",
  "Species are classified separately: do not assign a topic merely because",
  "Atlantic salmon, Pacific salmon or rainbow trout is named.",
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
            enum = c(
              "high",
              "medium",
              "low"
            )
          ),
          evidence = list(
            type = "string"
          )
        ),
        required = c(
          "path_id",
          "confidence",
          "evidence"
        ),
        additionalProperties = FALSE
      )
    ),
    review_required = list(
      type = "boolean"
    ),
    review_reason = list(
      type = c(
        "string",
        "null"
      )
    )
  ),
  required = c(
    "assignments",
    "review_required",
    "review_reason"
  ),
  additionalProperties = FALSE
)

make_user_prompt <- function(title, abstract) {

  paste0(
    "ONTOLOGY\n",
    ontology_prompt,
    "\n\nRECORD\n",
    "Title: ",
    title,
    "\n\nAbstract: ",
    abstract,
    "\n\nClassify the record using only valid ontology path IDs."
  )
}

make_batch_request <- function(
    record_sequence,
    record_id,
    title,
    abstract
) {

  list(
    custom_id = paste0(
      "record_",
      record_sequence
    ),
    method = "POST",
    url = "/v1/responses",
    body = list(
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
              text = make_user_prompt(
                title,
                abstract
              )
            )
          )
        )
      ),
      text = list(
        format = list(
          type = "json_schema",
          name = "topic_classification",
          description = paste(
            "Substantive topic paths assigned to one scientific record."
          ),
          strict = TRUE,
          schema = response_schema
        )
      )
    )
  )
}

batch_requests <- purrr::pmap(
  pilot_records,
  make_batch_request
)

jsonl_path <- fs::path(
  output_dir,
  "llm_topic_pilot_batch_20.jsonl"
)

json_lines <- vapply(
  batch_requests,
  jsonlite::toJSON,
  character(1),
  auto_unbox = TRUE,
  null = "null",
  na = "null"
)

writeLines(
  json_lines,
  jsonl_path,
  useBytes = TRUE
)

# Save prompt materials for audit ---------------------------------------------

writeLines(
  system_prompt,
  fs::path(
    output_dir,
    "llm_topic_system_prompt.txt"
  )
)

writeLines(
  ontology_prompt,
  fs::path(
    output_dir,
    "llm_topic_ontology_prompt.txt"
  )
)

readr::write_csv(
  ontology_prompt_tbl |>
    dplyr::select(
      path_id,
      hierarchy_path,
      representative_terms
    ),
  fs::path(
    output_dir,
    "llm_topic_prompt_ontology.csv"
  ),
  na = ""
)

message("LLM pilot batch created.")
message("Pilot records: ", nrow(pilot_records))
message("Ontology paths: ", nrow(ontology))
message("Batch requests: ", length(batch_requests))
message("JSONL file: ", jsonl_path)
