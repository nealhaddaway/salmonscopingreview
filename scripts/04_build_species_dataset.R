# Stage 2.4: build provisional record-level species dataset -------------------

source("scripts/00_setup.R")
source("R/read_corpus.R")
source("R/validate_species_dictionary.R")

# Input paths -----------------------------------------------------------------

input_records <- here::here(
  "data_raw",
  "INCLUDES fixed abstracts.txt"
)

input_assignments <- here::here(
  "outputs",
  "stage_2_species",
  "species_assignments.csv"
)

input_dictionary <- here::here(
  "dictionary",
  "species_dictionary.csv"
)

input_manual_validation <- here::here(
  "outputs",
  "stage_2_validation",
  "manual_review_validated_80.csv"
)

# Output folder ---------------------------------------------------------------

output_dir <- here::here(
  "outputs",
  "stage_2_species"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(input_records),
  file.exists(input_assignments),
  file.exists(input_dictionary),
  file.exists(input_manual_validation)
)

# Read inputs -----------------------------------------------------------------

records <- read_corpus(input_records)

automatic_assignments <- readr::read_csv(
  input_assignments,
  show_col_types = FALSE
)

species_dictionary <- validate_species_dictionary(
  input_dictionary
)

manual_validation <- readr::read_csv(
  input_manual_validation,
  show_col_types = FALSE
)

# Validate required columns ---------------------------------------------------

required_assignment_columns <- c(
  "record_sequence",
  "record_id",
  "farmed_species_id",
  "farmed_species",
  "assignment_role",
  "review_required",
  "assignment_reason",
  "non_target_species"
)

required_manual_columns <- c(
  "record_sequence",
  "validation_correct",
  "validation_species",
  "validation_notes"
)

missing_assignment_columns <- setdiff(
  required_assignment_columns,
  names(automatic_assignments)
)

missing_manual_columns <- setdiff(
  required_manual_columns,
  names(manual_validation)
)

if (length(missing_assignment_columns) > 0L) {
  stop(
    "Automatic assignments are missing columns: ",
    paste(missing_assignment_columns, collapse = ", ")
  )
}

if (length(missing_manual_columns) > 0L) {
  stop(
    "Manual validation is missing columns: ",
    paste(missing_manual_columns, collapse = ", ")
  )
}

# Normalise manual decisions --------------------------------------------------

manual_validation <- manual_validation |>
  dplyr::mutate(
    validation_correct = validation_correct |>
      as.character() |>
      stringr::str_trim() |>
      stringr::str_to_upper(),
    validation_correct = dplyr::case_when(
      validation_correct %in% c("Y", "YES") ~ "Y",
      validation_correct %in% c("N", "NO") ~ "N",
      TRUE ~ NA_character_
    ),
    validation_species = validation_species |>
      as.character() |>
      stringr::str_squish() |>
      stringr::str_replace(
        "^Unspecified farmed salmonid$",
        "Unspecified farmed salmon"
      ) |>
      dplyr::na_if(""),
    validation_notes = validation_notes |>
      as.character() |>
      stringr::str_squish() |>
      dplyr::na_if("")
  ) |>
  dplyr::filter(
    !is.na(validation_correct)
  )

if (anyDuplicated(manual_validation$record_sequence) > 0L) {
  stop(
    "Manual validation contains duplicate record_sequence values."
  )
}

# Species lookup --------------------------------------------------------------

species_lookup <- species_dictionary |>
  dplyr::filter(
    is_farmed_candidate %in% TRUE
  ) |>
  dplyr::distinct(
    farmed_species_id = species_id,
    farmed_species = preferred_name
  )

# Convert manual corrections to long-form assignments ------------------------

manual_corrections <- manual_validation |>
  dplyr::filter(
    validation_correct == "N",
    !is.na(validation_species)
  ) |>
  dplyr::select(
    record_sequence,
    validation_species,
    validation_notes
  ) |>
  tidyr::separate_longer_delim(
    validation_species,
    delim = ";"
  ) |>
  dplyr::mutate(
    validation_species = stringr::str_squish(
      validation_species
    )
  ) |>
  dplyr::left_join(
    species_lookup,
    by = c(
      "validation_species" = "farmed_species"
    )
  )

unmatched_manual_species <- manual_corrections |>
  dplyr::filter(
    is.na(farmed_species_id)
  ) |>
  dplyr::distinct(
    validation_species
  )

if (nrow(unmatched_manual_species) > 0L) {
  stop(
    "Unrecognised manually assigned species: ",
    paste(
      unmatched_manual_species$validation_species,
      collapse = ", "
    )
  )
}

manual_corrections <- manual_corrections |>
  dplyr::group_by(
    record_sequence
  ) |>
  dplyr::mutate(
    assignment_role = dplyr::if_else(
      dplyr::n() == 1L,
      "manual-primary",
      "manual-co-primary"
    ),
    assignment_reason = dplyr::if_else(
      dplyr::n() == 1L,
      "Manual review assigned one eligible farmed species",
      paste0(
        "Manual review assigned ",
        dplyr::n(),
        " eligible farmed species"
      )
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::transmute(
    record_sequence,
    farmed_species_id,
    farmed_species = validation_species,
    assignment_role,
    review_required = FALSE,
    assignment_reason,
    non_target_species = NA_character_,
    assignment_source = "manual correction",
    validation_notes
  )

# Records manually confirmed as requiring no target-species assignment --------

manual_no_assignment <- manual_validation |>
  dplyr::filter(
    validation_correct == "Y"
  ) |>
  dplyr::transmute(
    record_sequence,
    farmed_species_id = NA_character_,
    farmed_species = NA_character_,
    assignment_role = "manually resolved",
    review_required = FALSE,
    assignment_reason =
      "Manual review confirmed no eligible farmed species assignment",
    non_target_species = NA_character_,
    assignment_source = "manual confirmation",
    validation_notes
  )

# Retain automatic assignments not superseded by manual review ----------------

manually_reviewed_records <- manual_validation$record_sequence

automatic_retained <- automatic_assignments |>
  dplyr::filter(
    !record_sequence %in% manually_reviewed_records
  ) |>
  dplyr::mutate(
    review_required = as.logical(review_required),
    assignment_source = dplyr::if_else(
      review_required,
      "automatic unresolved",
      "automatic"
    ),
    validation_notes = NA_character_
  ) |>
  dplyr::select(
    record_sequence,
    farmed_species_id,
    farmed_species,
    assignment_role,
    review_required,
    assignment_reason,
    non_target_species,
    assignment_source,
    validation_notes
  )

# Combine automatic and manually resolved assignments -------------------------

provisional_assignments <- dplyr::bind_rows(
  automatic_retained,
  manual_corrections,
  manual_no_assignment
) |>
  dplyr::left_join(
    records |>
      dplyr::select(
        record_sequence,
        record_id
      ),
    by = "record_sequence"
  ) |>
  dplyr::relocate(
    record_sequence,
    record_id
  ) |>
  dplyr::arrange(
    record_sequence,
    farmed_species
  )

# Structural checks -----------------------------------------------------------

assignment_record_count <- provisional_assignments |>
  dplyr::summarise(
    records = dplyr::n_distinct(record_sequence)
  ) |>
  dplyr::pull(records)

if (assignment_record_count != nrow(records)) {
  stop(
    "Expected assignments for ",
    nrow(records),
    " records, but found ",
    assignment_record_count,
    "."
  )
}

# Build one-row-per-record dataset --------------------------------------------

species_records_provisional <- provisional_assignments |>
  dplyr::group_by(
    record_sequence,
    record_id
  ) |>
  dplyr::summarise(
    farmed_species_n = dplyr::n_distinct(
      farmed_species_id,
      na.rm = TRUE
    ),
    farmed_species = {
      values <- sort(
        unique(
          stats::na.omit(farmed_species)
        )
      )
      
      if (length(values) == 0L) {
        NA_character_
      } else {
        paste(values, collapse = "; ")
      }
    },
    review_required = any(
      review_required %in% TRUE
    ),
    assignment_source = paste(
      sort(unique(assignment_source)),
      collapse = "; "
    ),
    assignment_reason = paste(
      sort(unique(assignment_reason)),
      collapse = "; "
    ),
    validation_notes = {
      values <- unique(
        stats::na.omit(validation_notes)
      )
      
      if (length(values) == 0L) {
        NA_character_
      } else {
        paste(values, collapse = "; ")
      }
    },
    .groups = "drop"
  ) |>
  dplyr::left_join(
    records |>
      dplyr::select(
        record_sequence,
        title,
        abstract
      ),
    by = "record_sequence"
  ) |>
  dplyr::arrange(
    record_sequence
  )

unresolved_records <- species_records_provisional |>
  dplyr::filter(
    review_required %in% TRUE
  )

# Summary ---------------------------------------------------------------------

summary_tbl <- tibble::tibble(
  measure = c(
    "Corpus records",
    "Long-form assignment rows",
    "Automatically resolved records",
    "Manually corrected records",
    "Manually confirmed no-assignment records",
    "Remaining unresolved records"
  ),
  value = c(
    nrow(records),
    nrow(provisional_assignments),
    provisional_assignments |>
      dplyr::filter(
        assignment_source == "automatic"
      ) |>
      dplyr::summarise(
        n = dplyr::n_distinct(record_sequence)
      ) |>
      dplyr::pull(n),
    provisional_assignments |>
      dplyr::filter(
        assignment_source == "manual correction"
      ) |>
      dplyr::summarise(
        n = dplyr::n_distinct(record_sequence)
      ) |>
      dplyr::pull(n),
    provisional_assignments |>
      dplyr::filter(
        assignment_source == "manual confirmation"
      ) |>
      dplyr::summarise(
        n = dplyr::n_distinct(record_sequence)
      ) |>
      dplyr::pull(n),
    nrow(unresolved_records)
  )
)

# Write outputs ---------------------------------------------------------------

readr::write_csv(
  provisional_assignments,
  fs::path(
    output_dir,
    "species_assignments_provisional.csv"
  ),
  na = ""
)

readr::write_csv(
  species_records_provisional,
  fs::path(
    output_dir,
    "species_records_provisional.csv"
  ),
  na = ""
)

readr::write_csv(
  unresolved_records,
  fs::path(
    output_dir,
    "species_unresolved_records.csv"
  ),
  na = ""
)

readr::write_csv(
  summary_tbl,
  fs::path(
    output_dir,
    "species_provisional_summary.csv"
  ),
  na = ""
)

message("Provisional species dataset written.")
message("Corpus records: ", nrow(records))
message(
  "Long-form assignment rows: ",
  nrow(provisional_assignments)
)
message(
  "Remaining unresolved records: ",
  nrow(unresolved_records)
)