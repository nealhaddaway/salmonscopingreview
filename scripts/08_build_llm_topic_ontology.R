# Stage 4.1: build LLM-ready topic ontology -----------------------------------

source("scripts/00_setup.R")

input_dictionary <- here::here(
  "data_raw",
  "Salmon scoping review keywords - FULL dictionary_NO_FARMED_SPECIES.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_4_llm"
)

fs::dir_create(output_dir)

stopifnot(file.exists(input_dictionary))

topic_dictionary <- readr::read_csv(
  input_dictionary,
  show_col_types = FALSE,
  na = c("", "NA")
) |>
  janitor::clean_names() |>
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      stringr::str_squish
    )
  )

required_columns <- c(
  "broad_topic",
  "subtopic",
  "feature",
  "component",
  "term"
)

missing_columns <- setdiff(
  required_columns,
  names(topic_dictionary)
)

if (length(missing_columns) > 0L) {
  stop(
    "Topic dictionary is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

llm_topic_ontology <- topic_dictionary |>
  dplyr::filter(
    !is.na(term),
    nzchar(term)
  ) |>
  dplyr::distinct(
    broad_topic,
    subtopic,
    feature,
    component,
    term
  ) |>
  dplyr::arrange(
    broad_topic,
    subtopic,
    feature,
    component,
    term
  ) |>
  dplyr::group_by(
    broad_topic,
    subtopic,
    feature,
    component
  ) |>
  dplyr::summarise(
    term_count = dplyr::n_distinct(
      stringr::str_to_lower(term)
    ),
    supporting_terms = paste(
      term[!duplicated(stringr::str_to_lower(term))],
      collapse = "; "
    ),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    broad_topic,
    subtopic,
    feature,
    component
  ) |>
  dplyr::mutate(
    path_id = sprintf(
      "TOPIC_%03d",
      dplyr::row_number()
    ),
    hierarchy_path = paste(
      broad_topic,
      subtopic,
      feature,
      component,
      sep = " > "
    ),
    .before = 1
  ) |>
  dplyr::relocate(
    hierarchy_path,
    .after = component
  )

if (anyDuplicated(llm_topic_ontology$hierarchy_path) > 0L) {
  stop("The compressed ontology contains duplicate hierarchy paths.")
}

readr::write_csv(
  llm_topic_ontology,
  fs::path(
    output_dir,
    "llm_topic_ontology.csv"
  ),
  na = ""
)

jsonlite::write_json(
  llm_topic_ontology,
  fs::path(
    output_dir,
    "llm_topic_ontology.json"
  ),
  dataframe = "rows",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

summary_tbl <- tibble::tibble(
  measure = c(
    "Source dictionary rows",
    "LLM hierarchy paths",
    "Broad topics",
    "Subtopics",
    "Features",
    "Components",
    "Unique supporting terms"
  ),
  value = c(
    nrow(topic_dictionary),
    nrow(llm_topic_ontology),
    dplyr::n_distinct(llm_topic_ontology$broad_topic),
    dplyr::n_distinct(llm_topic_ontology$subtopic),
    dplyr::n_distinct(llm_topic_ontology$feature),
    dplyr::n_distinct(llm_topic_ontology$component),
    dplyr::n_distinct(
      stringr::str_to_lower(topic_dictionary$term),
      na.rm = TRUE
    )
  )
)

readr::write_csv(
  summary_tbl,
  fs::path(
    output_dir,
    "llm_topic_ontology_summary.csv"
  ),
  na = ""
)

message("LLM topic ontology written.")
message("Source dictionary rows: ", nrow(topic_dictionary))
message("Hierarchy paths: ", nrow(llm_topic_ontology))
message(
  "Unique supporting terms: ",
  dplyr::n_distinct(
    stringr::str_to_lower(topic_dictionary$term),
    na.rm = TRUE
  )
)
