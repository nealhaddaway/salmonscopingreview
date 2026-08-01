# Stage 3.2: annotate topics across the corpus --------------------------------

source("scripts/00_setup.R")
source("R/read_corpus.R")
source("R/detect_topic_mentions.R")
source("R/run_topic_annotation.R")

# Input paths -----------------------------------------------------------------

input_records <- here::here(
  "data_raw",
  "INCLUDES fixed abstracts.txt"
)

input_topic_dictionary <- here::here(
  "outputs",
  "stage_3_topics",
  "topic_dictionary_clean.csv"
)

# Output folder ---------------------------------------------------------------

output_dir <- here::here(
  "outputs",
  "stage_3_topics"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(input_records),
  file.exists(input_topic_dictionary)
)

# Read inputs -----------------------------------------------------------------

records <- read_corpus(input_records)

topic_dictionary <- readr::read_csv(
  input_topic_dictionary,
  show_col_types = FALSE
)

# Run annotation --------------------------------------------------------------

topic_results <- run_topic_annotation(
  records = records,
  topic_dictionary = topic_dictionary,
  progress = TRUE
)

# Create summaries ------------------------------------------------------------

topic_assignment_summary <- topic_results$topic_assignments |>
  dplyr::filter(
    !is.na(broad_topic)
  ) |>
  dplyr::count(
    broad_topic,
    subtopic,
    feature,
    component,
    name = "records",
    sort = TRUE
  )

review_queue <- topic_results$topic_assignments |>
  dplyr::filter(
    review_required %in% TRUE
  ) |>
  dplyr::left_join(
    records |>
      dplyr::select(
        record_sequence,
        title,
        abstract
      ),
    by = "record_sequence"
  )

# Write outputs ---------------------------------------------------------------

readr::write_csv(
  topic_results$topic_mentions,
  fs::path(
    output_dir,
    "topic_mentions.csv"
  ),
  na = ""
)

readr::write_csv(
  topic_results$topic_assignments,
  fs::path(
    output_dir,
    "topic_assignments.csv"
  ),
  na = ""
)

readr::write_csv(
  topic_results$failures,
  fs::path(
    output_dir,
    "topic_failures.csv"
  ),
  na = ""
)

readr::write_csv(
  topic_assignment_summary,
  fs::path(
    output_dir,
    "topic_assignment_summary.csv"
  ),
  na = ""
)

readr::write_csv(
  review_queue,
  fs::path(
    output_dir,
    "topic_review_queue.csv"
  ),
  na = ""
)

capture.output(
  sessionInfo(),
  file = fs::path(
    output_dir,
    "session_info.txt"
  )
)

# Report ----------------------------------------------------------------------

cli::cli_alert_success(
  "Stage 3 topic annotation completed."
)

cli::cli_alert_info(
  "Annotated {nrow(records)} records."
)

cli::cli_alert_info(
  "Topic mentions: {nrow(topic_results$topic_mentions)}."
)

cli::cli_alert_info(
  "Topic assignment rows: {nrow(topic_results$topic_assignments)}."
)

cli::cli_alert_info(
  "Failures: {nrow(topic_results$failures)}."
)

cli::cli_alert_info(
  "Manual-review rows: {nrow(review_queue)}."
)