source("scripts/00_setup.R")
source("R/read_corpus.R")

# Input paths
input_records <- "data_raw/INCLUDES fixed abstracts.txt"
input_assignments <- "outputs/stage_2_species/species_assignments.csv"

# Output folder
output_dir <- "outputs/stage_2_validation"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Read source records and Stage 2 assignments
records <- read_corpus(input_records)

species_assignments <- readr::read_csv(
  input_assignments,
  show_col_types = FALSE
)

# Check required columns
required_record_columns <- c(
  "record_sequence",
  "record_id",
  "title",
  "abstract"
)

required_assignment_columns <- c(
  "record_sequence",
  "record_id",
  "farmed_species_id",
  "farmed_species",
  "review_required",
  "assignment_reason"
)

missing_record_columns <- setdiff(
  required_record_columns,
  names(records)
)

missing_assignment_columns <- setdiff(
  required_assignment_columns,
  names(species_assignments)
)

if (length(missing_record_columns) > 0) {
  stop(
    "Missing columns from records: ",
    paste(missing_record_columns, collapse = ", ")
  )
}

if (length(missing_assignment_columns) > 0) {
  stop(
    "Missing columns from species assignments: ",
    paste(missing_assignment_columns, collapse = ", ")
  )
}

message("Records loaded: ", nrow(records))
message("Assignment rows loaded: ", nrow(species_assignments))
message("Validation output folder: ", output_dir)

# Collapse long-form assignments to one row per record
record_assignments <- species_assignments |>
  dplyr::group_by(
    record_sequence,
    record_id
  ) |>
  dplyr::summarise(
    species_n = dplyr::n_distinct(
      farmed_species_id,
      na.rm = TRUE
    ),
    assigned_species = dplyr::if_else(
      all(is.na(farmed_species)),
      NA_character_,
      paste(
        sort(unique(stats::na.omit(farmed_species))),
        collapse = "; "
      )
    ),
    review_required = any(review_required),
    assignment_reason = paste(
      sort(unique(assignment_reason)),
      collapse = "; "
    ),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    records |>
      dplyr::select(
        record_sequence,
        source_record_id = record_id,
        title,
        abstract
      ),
    by = "record_sequence"
  )

# Confirm one row per corpus record
if (nrow(record_assignments) != nrow(records)) {
  stop(
    "Expected one validation row per record, but found ",
    nrow(record_assignments),
    " rows for ",
    nrow(records),
    " records."
  )
}

message(
  "Record-level assignment rows created: ",
  nrow(record_assignments)
)

#----------------------------------------------------------
# Create validation datasets
#----------------------------------------------------------

set.seed(20260731)

# 1. Random sample of single-species assignments
single_species_sample <-
  record_assignments |>
  dplyr::filter(
    species_n == 1,
    !review_required
  ) |>
  dplyr::slice_sample(n = 50)

# 2. Random sample of multi-species assignments
multi_species_sample <-
  record_assignments |>
  dplyr::filter(
    species_n >= 2,
    !review_required
  ) |>
  dplyr::slice_sample(n = 50)

# 3. All manual-review records
manual_review_all <-
  record_assignments |>
  dplyr::filter(review_required)

# 4. All records with four or more assigned farmed species
high_multiplicity_all <-
  record_assignments |>
  dplyr::filter(species_n >= 4)

# Add blank validation columns
add_validation_columns <- function(df) {
  
  df |>
    dplyr::mutate(
      validation_correct = NA_character_,
      validation_species = NA_character_,
      validation_notes = NA_character_
    )
  
}

single_species_sample <- add_validation_columns(single_species_sample)
multi_species_sample <- add_validation_columns(multi_species_sample)
manual_review_all <- add_validation_columns(manual_review_all)
high_multiplicity_all <- add_validation_columns(high_multiplicity_all)

#----------------------------------------------------------
# Export validation datasets
#----------------------------------------------------------

readr::write_csv(
  single_species_sample,
  file.path(
    output_dir,
    "single_species_sample.csv"
  )
)

readr::write_csv(
  multi_species_sample,
  file.path(
    output_dir,
    "multi_species_sample.csv"
  )
)

readr::write_csv(
  manual_review_all,
  file.path(
    output_dir,
    "manual_review_all.csv"
  )
)

readr::write_csv(
  high_multiplicity_all,
  file.path(
    output_dir,
    "high_multiplicity_all.csv"
  )
)

message("Validation datasets written.")

#----------------------------------------------------------
# Create validation workbook
#----------------------------------------------------------

wb <- openxlsx2::wb_workbook()

wb$add_worksheet("Single species")
wb$add_data("Single species", single_species_sample)

wb$add_worksheet("Multiple species")
wb$add_data("Multiple species", multi_species_sample)

wb$add_worksheet("Manual review")
wb$add_data("Manual review", manual_review_all)

wb$add_worksheet("High multiplicity")
wb$add_data("High multiplicity", high_multiplicity_all)

wb$save(
  file.path(
    output_dir,
    "species_validation_workbook.xlsx"
  ),
  overwrite = TRUE
)

message("Validation workbook written.")