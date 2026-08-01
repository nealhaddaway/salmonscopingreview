# Stage 3.1: validate topic dictionary ----------------------------------------

source("scripts/00_setup.R")

input_dictionary <- here::here(
  "data_raw",
  "Salmon scoping review keywords - FULL dictionary.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_3_topics"
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
    ),
    dictionary_row = dplyr::row_number(),
    hierarchy_path = paste(
      broad_topic,
      subtopic,
      feature,
      component,
      sep = " > "
    ),
    dictionary_entry = paste(
      broad_topic,
      subtopic,
      feature,
      component,
      term,
      sep = " > "
    )
  )

topic_dictionary <- topic_dictionary |>
  dplyr::distinct(
    broad_topic,
    subtopic,
    feature,
    component,
    term,
    .keep_all = TRUE
  ) |>
  dplyr::mutate(
    dictionary_row = dplyr::row_number()
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

missing_values <- topic_dictionary |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(required_columns),
      ~ sum(is.na(.x))
    )
  ) |>
  tidyr::pivot_longer(
    dplyr::everything(),
    names_to = "field",
    values_to = "missing_n"
  )

duplicate_paths <- topic_dictionary |>
  dplyr::add_count(
    dictionary_entry,
    name = "duplicate_n"
  ) |>
  dplyr::filter(
    duplicate_n > 1L
  ) |>
  dplyr::arrange(
    hierarchy_path,
    dictionary_row
  )

hierarchy_summary <- topic_dictionary |>
  dplyr::count(
    broad_topic,
    subtopic,
    name = "dictionary_rows"
  ) |>
  dplyr::arrange(
    broad_topic,
    subtopic
  )

broad_topic_summary <- topic_dictionary |>
  dplyr::count(
    broad_topic,
    name = "dictionary_rows",
    sort = TRUE
  )

summary_tbl <- tibble::tibble(
  measure = c(
    "Dictionary rows",
    "Broad topics",
    "Subtopics",
    "Features",
    "Components",
    "Exact duplicate dictionary entries",
    "Rows with any missing hierarchy value"
  ),
  value = c(
    nrow(topic_dictionary),
    dplyr::n_distinct(topic_dictionary$broad_topic),
    dplyr::n_distinct(topic_dictionary$subtopic),
    dplyr::n_distinct(topic_dictionary$feature),
    dplyr::n_distinct(topic_dictionary$component),
    nrow(duplicate_paths),
    sum(
      !stats::complete.cases(
        topic_dictionary[, required_columns]
      )
    )
  )
)

readr::write_csv(
  topic_dictionary,
  fs::path(
    output_dir,
    "topic_dictionary_clean.csv"
  ),
  na = ""
)

readr::write_csv(
  summary_tbl,
  fs::path(
    output_dir,
    "topic_dictionary_summary.csv"
  ),
  na = ""
)

readr::write_csv(
  hierarchy_summary,
  fs::path(
    output_dir,
    "topic_hierarchy_summary.csv"
  ),
  na = ""
)

readr::write_csv(
  duplicate_paths,
  fs::path(
    output_dir,
    "topic_dictionary_duplicate_paths.csv"
  ),
  na = ""
)

wb <- openxlsx2::wb_workbook()

wb$add_worksheet("Summary")
wb$add_data("Summary", summary_tbl)

wb$add_worksheet("Broad topics")
wb$add_data("Broad topics", broad_topic_summary)

wb$add_worksheet("Hierarchy")
wb$add_data("Hierarchy", hierarchy_summary)

wb$add_worksheet("Missing values")
wb$add_data("Missing values", missing_values)

wb$add_worksheet("Duplicate paths")
wb$add_data("Duplicate paths", duplicate_paths)

wb$add_worksheet("Clean dictionary")
wb$add_data("Clean dictionary", topic_dictionary)

wb$save(
  fs::path(
    output_dir,
    "topic_dictionary_audit.xlsx"
  ),
  overwrite = TRUE
)

message("Topic dictionary validation completed.")
message("Dictionary rows: ", nrow(topic_dictionary))
message(
  "Broad topics: ",
  dplyr::n_distinct(topic_dictionary$broad_topic)
)
message(
  "Duplicate hierarchy paths: ",
  nrow(duplicate_paths)
)
message(
  "Rows with missing hierarchy values: ",
  sum(
    !stats::complete.cases(
      topic_dictionary[, required_columns]
    )
  )
)