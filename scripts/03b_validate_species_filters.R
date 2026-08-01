# Stage 2.3: validate abstract-level species assignment ------------------------

source("scripts/00_setup.R")
source("R/read_corpus.R")

# Input paths -----------------------------------------------------------------

input_records <- here::here(
  "data_raw",
  "INCLUDES fixed abstracts.txt"
)

input_mentions <- here::here(
  "outputs",
  "stage_2_species",
  "species_mentions.csv"
)

input_assignments <- here::here(
  "outputs",
  "stage_2_species",
  "species_assignments.csv"
)

# Output folder ---------------------------------------------------------------

output_dir <- here::here(
  "outputs",
  "stage_2_validation"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(input_records),
  file.exists(input_mentions),
  file.exists(input_assignments)
)

# Read data -------------------------------------------------------------------

records <- read_corpus(input_records)

species_mentions <- readr::read_csv(
  input_mentions,
  show_col_types = FALSE
)

species_assignments <- readr::read_csv(
  input_assignments,
  show_col_types = FALSE
)

# Validate required columns ---------------------------------------------------

required_mention_columns <- c(
  "record_sequence",
  "record_id",
  "species_id",
  "preferred_name",
  "scientific_name",
  "matched_term",
  "source",
  "match_start",
  "match_end",
  "mention_eligible",
  "filter_reason"
)

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

missing_mention_columns <- setdiff(
  required_mention_columns,
  names(species_mentions)
)

missing_assignment_columns <- setdiff(
  required_assignment_columns,
  names(species_assignments)
)

if (length(missing_mention_columns) > 0L) {
  stop(
    "Species mentions are missing required columns: ",
    paste(missing_mention_columns, collapse = ", ")
  )
}

if (length(missing_assignment_columns) > 0L) {
  stop(
    "Species assignments are missing required columns: ",
    paste(missing_assignment_columns, collapse = ", ")
  )
}

# Normalise logical values ----------------------------------------------------

species_mentions <- species_mentions |>
  dplyr::mutate(
    mention_eligible = as.logical(mention_eligible)
  )

species_assignments <- species_assignments |>
  dplyr::mutate(
    review_required = as.logical(review_required)
  )

# Identify affected records --------------------------------------------------

filtered_mentions <- species_mentions |>
  dplyr::filter(
    mention_eligible %in% FALSE
  )

affected_record_ids <- sort(
  unique(filtered_mentions$record_sequence)
)

if (length(affected_record_ids) == 0L) {
  stop(
    "No filtered species mentions were found. ",
    "Check that Stage 2 was rerun after integrating filtering."
  )
}

# Collapse mention information to one row per record --------------------------

filtered_by_record <- filtered_mentions |>
  dplyr::group_by(
    record_sequence,
    record_id
  ) |>
  dplyr::summarise(
    filtered_mention_n = dplyr::n(),
    filtered_species = paste(
      sort(unique(preferred_name)),
      collapse = "; "
    ),
    filtered_scientific_names = {
      values <- sort(
        unique(
          stats::na.omit(scientific_name)
        )
      )
      
      if (length(values) == 0L) {
        NA_character_
      } else {
        paste(values, collapse = "; ")
      }
    },
    filtered_terms = paste(
      sort(unique(matched_term)),
      collapse = "; "
    ),
    filtered_sources = paste(
      sort(unique(source)),
      collapse = "; "
    ),
    filter_reasons = paste(
      sort(unique(filter_reason)),
      collapse = "; "
    ),
    .groups = "drop"
  )

# Count eligible and ineligible mentions per record ---------------------------

mention_status <- species_mentions |>
  dplyr::filter(
    record_sequence %in% affected_record_ids
  ) |>
  dplyr::group_by(
    record_sequence
  ) |>
  dplyr::summarise(
    detected_mention_n = dplyr::n(),
    eligible_mention_n = sum(
      mention_eligible %in% TRUE,
      na.rm = TRUE
    ),
    ineligible_mention_n = sum(
      mention_eligible %in% FALSE,
      na.rm = TRUE
    ),
    all_mentions_ineligible = (
      eligible_mention_n == 0L &&
        ineligible_mention_n > 0L
    ),
    .groups = "drop"
  )

# Collapse assignments to one row per record ---------------------------------

assignment_by_record <- species_assignments |>
  dplyr::filter(
    record_sequence %in% affected_record_ids
  ) |>
  dplyr::group_by(
    record_sequence
  ) |>
  dplyr::summarise(
    assigned_species_n = dplyr::n_distinct(
      farmed_species_id,
      na.rm = TRUE
    ),
    assigned_species = {
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
    assignment_roles = paste(
      sort(unique(assignment_role)),
      collapse = "; "
    ),
    review_required = any(
      review_required %in% TRUE
    ),
    assignment_reasons = paste(
      sort(unique(assignment_reason)),
      collapse = "; "
    ),
    non_target_species = {
      values <- sort(
        unique(
          stats::na.omit(non_target_species)
        )
      )
      
      if (length(values) == 0L) {
        NA_character_
      } else {
        paste(values, collapse = "; ")
      }
    },
    .groups = "drop"
  )

# Build record-level validation table ----------------------------------------

affected_records <- records |>
  dplyr::filter(
    record_sequence %in% affected_record_ids
  ) |>
  dplyr::select(
    record_sequence,
    record_id,
    title,
    abstract
  ) |>
  dplyr::left_join(
    mention_status,
    by = "record_sequence"
  ) |>
  dplyr::left_join(
    filtered_by_record |>
      dplyr::select(
        -record_id
      ),
    by = "record_sequence"
  ) |>
  dplyr::left_join(
    assignment_by_record,
    by = "record_sequence"
  ) |>
  dplyr::mutate(
    pipeline_assignment = dplyr::case_when(
      is.na(assigned_species) ~ "None",
      TRUE ~ assigned_species
    ),
    validation_scope = NA_character_,
    validation_expected_assignment = NA_character_,
    validation_pipeline_correct = NA_character_,
    validation_error_type = NA_character_,
    validation_notes = NA_character_
  ) |>
  dplyr::select(
    -assigned_species_n,
    -assignment_roles,
    -assignment_reasons
  ) |>
  dplyr::arrange(
    dplyr::desc(all_mentions_ineligible),
    filter_reasons,
    record_sequence
  )

# Create validation strata ---------------------------------------------------

wild_context_records <- affected_records |>
  dplyr::filter(
    grepl(
      "explicitly identified as wild",
      filter_reasons,
      fixed = TRUE
    )
  )

non_target_salmo_records <- affected_records |>
  dplyr::filter(
    grepl(
      "non-target Salmo",
      filter_reasons,
      fixed = TRUE
    )
  )

all_mentions_ineligible <- affected_records |>
  dplyr::filter(
    all_mentions_ineligible %in% TRUE
  )

# Create abstract-level validation sample ------------------------------------

set.seed(20260731)

# 50 affected records (existing strategy)

validation_all_ineligible <- all_mentions_ineligible |>
  dplyr::mutate(
    validation_stratum = "Affected: all mentions ineligible"
  )

already_selected <- validation_all_ineligible$record_sequence

non_target_salmo_pool <- non_target_salmo_records |>
  dplyr::filter(
    !record_sequence %in% already_selected
  )

validation_non_target_salmo <- non_target_salmo_pool |>
  dplyr::slice_sample(
    n = min(10L, nrow(non_target_salmo_pool))
  ) |>
  dplyr::mutate(
    validation_stratum = "Affected: non-target Salmo"
  )

already_selected <- c(
  already_selected,
  validation_non_target_salmo$record_sequence
)

wild_context_pool <- wild_context_records |>
  dplyr::filter(
    !record_sequence %in% already_selected
  )

validation_wild_context <- wild_context_pool |>
  dplyr::slice_sample(
    n = min(9L, nrow(wild_context_pool))
  ) |>
  dplyr::mutate(
    validation_stratum = "Affected: wild context"
  )

validation_affected <- dplyr::bind_rows(
  validation_all_ineligible,
  validation_non_target_salmo,
  validation_wild_context
)

# 50 unaffected records

unaffected_records <- records |>
  dplyr::filter(
    !record_sequence %in% affected_record_ids
  ) |>
  dplyr::left_join(
    species_assignments |>
      dplyr::group_by(record_sequence) |>
      dplyr::summarise(
        assigned_species = paste(
          sort(unique(stats::na.omit(farmed_species))),
          collapse = "; "
        ),
        assignment_reasons = paste(
          sort(unique(stats::na.omit(assignment_reason))),
          collapse = "; "
        ),
        .groups = "drop"
      ),
    by = "record_sequence"
  ) |>
  dplyr::mutate(
    pipeline_assignment = dplyr::case_when(
      is.na(assigned_species) ~ "None",
      TRUE ~ assigned_species
    ),
    validation_stratum = "Unaffected random sample",
    validation_scope = NA_character_,
    validation_expected_assignment = NA_character_,
    validation_pipeline_correct = NA_character_,
    validation_error_type = NA_character_,
    validation_notes = NA_character_
  ) |>
  dplyr::slice_sample(n = 50)

filter_validation_sample <- dplyr::bind_rows(
  validation_affected,
  unaffected_records
) |>
  dplyr::arrange(
    validation_stratum,
    record_sequence
  )

stopifnot(nrow(filter_validation_sample) == 100L)

# Produce summary ------------------------------------------------------------

filter_summary <- tibble::tibble(
  measure = c(
    "Corpus records",
    "Detected species mentions",
    "Filtered mentions",
    "Affected records",
    "Validation sample size"
  ),
  value = c(
    nrow(records),
    nrow(species_mentions),
    nrow(filtered_mentions),
    nrow(affected_records),
    nrow(filter_validation_sample)
  )
)

reason_summary <- filtered_mentions |>
  dplyr::count(
    filter_reason,
    name = "filtered_mentions"
  ) |>
  dplyr::arrange(
    dplyr::desc(filtered_mentions),
    filter_reason
  )

# Write CSV outputs -----------------------------------------------------------

readr::write_csv(
  affected_records,
  fs::path(
    output_dir,
    "filter_affected_records.csv"
  ),
  na = ""
)

readr::write_csv(
  filter_summary,
  fs::path(
    output_dir,
    "assignment_validation_summary.csv"
  ),
  na = ""
)

readr::write_csv(
  filter_validation_sample,
  fs::path(
    output_dir,
    "assignment_validation_sample_100.csv"
  ),
  na = ""
)

# Create workbook -------------------------------------------------------------

wb <- openxlsx2::wb_workbook()

wb$add_worksheet("Summary")
wb$add_data(
  sheet = "Summary",
  x = filter_summary,
  start_row = 1
)
wb$add_data(
  sheet = "Summary",
  x = reason_summary,
  start_row = nrow(filter_summary) + 4L
)

wb$add_worksheet("All affected records")
wb$add_data(
  "All affected records",
  affected_records
)

wb$add_worksheet("Abstract validation")

validation_columns <- c(
  "validation_stratum",
  "record_sequence",
  "record_id",
  "title",
  "abstract",
  "filtered_species",
  "filter_reasons",
  "assignment_reasons",
  "pipeline_assignment",
  "validation_scope",
  "validation_expected_assignment",
  "validation_pipeline_correct",
  "validation_error_type",
  "validation_notes"
)

wb$add_data(
  "Abstract validation",
  filter_validation_sample |>
    dplyr::select(dplyr::all_of(validation_columns))
)

wb$save(
  fs::path(
    output_dir,
    "species_assignment_validation_workbook.xlsx"
  ),
  overwrite = TRUE
)

# Report results --------------------------------------------------------------

message("Abstract validation workbook written.")
message("Corpus records: ", nrow(records))
message("Detected species mentions: ", nrow(species_mentions))
message("Filtered mentions: ", nrow(filtered_mentions))
message("Affected records: ", nrow(affected_records))
message("Validation sample: ", nrow(filter_validation_sample))