# Stage 2: annotate farmed species across the corpus ---------------------------

source("scripts/00_setup.R")
source("R/read_corpus.R")
source("R/validate_species_dictionary.R")
source("R/detect_species_mentions.R")
source("R/filter_species_mentions.R")
source("R/assign_farmed_species.R")
source("R/run_species_annotation.R")

input_records <- here::here(
  "data_raw",
  "INCLUDES fixed abstracts.txt"
)

input_species_dictionary <- here::here(
  "dictionary",
  "species_dictionary.csv"
)

out_dir <- here::here(
  "outputs",
  "stage_2_species"
)

fs::dir_create(out_dir)

stopifnot(
  file.exists(input_records),
  file.exists(input_species_dictionary)
)

records <- read_corpus(input_records)

species_dictionary <- validate_species_dictionary(
  input_species_dictionary
)

species_results <- run_species_annotation(
  records = records,
  species_dictionary = species_dictionary,
  progress = TRUE
)

readr::write_csv(
  species_results$species_mentions,
  fs::path(out_dir, "species_mentions.csv"),
  na = ""
)

readr::write_csv(
  species_results$species_assignments,
  fs::path(out_dir, "species_assignments.csv"),
  na = ""
)

readr::write_csv(
  species_results$failures,
  fs::path(out_dir, "species_failures.csv"),
  na = ""
)

assignment_summary <- species_results$species_assignments |>
  dplyr::count(
    farmed_species,
    assignment_role,
    review_required,
    assignment_reason,
    name = "records"
  ) |>
  dplyr::arrange(
    dplyr::desc(records),
    farmed_species
  )

review_queue <- species_results$species_assignments |>
  dplyr::filter(review_required) |>
  dplyr::left_join(
    records |>
      dplyr::select(
        record_sequence,
        title,
        abstract
      ),
    by = "record_sequence"
  )

readr::write_csv(
  assignment_summary,
  fs::path(out_dir, "species_assignment_summary.csv"),
  na = ""
)

readr::write_csv(
  review_queue,
  fs::path(out_dir, "species_review_queue.csv"),
  na = ""
)

capture.output(
  sessionInfo(),
  file = fs::path(out_dir, "session_info.txt")
)

cli::cli_alert_success("Stage 2 species annotation completed.")
cli::cli_alert_info(
  "Annotated {nrow(records)} records."
)
cli::cli_alert_info(
  "Species mentions: {nrow(species_results$species_mentions)}."
)
cli::cli_alert_info(
  "Assignment rows: {nrow(species_results$species_assignments)}."
)
cli::cli_alert_info(
  "Failures: {nrow(species_results$failures)}."
)
cli::cli_alert_info(
  "Manual-review rows: {nrow(review_queue)}."
)