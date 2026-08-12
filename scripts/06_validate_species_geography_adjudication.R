# =============================================================================
# File: scripts/06_validate_species_geography_adjudication.R
# Purpose: Create an independent validation workbook for LLM species/geography
#          changes and unresolved cases. Does not modify annotation outputs.
# =============================================================================

source("scripts/00_setup.R")

input_file <- here::here(
  "outputs", "stage_2_5_annotation_adjudication",
  "species_geography_adjudication_full.csv"
)
output_file <- here::here(
  "outputs", "stage_2_5_annotation_adjudication",
  "species_geography_adjudication_validation.xlsx"
)

stopifnot(file.exists(input_file))

adjud <- readr::read_csv(input_file, show_col_types = FALSE)

set.seed(20260810)

# IMPORTANT: calculate sample sizes outside slice_sample().
# dplyr::slice_sample(n=...) requires n to be a constant, not n().
species_change_pool <- adjud |>
  dplyr::filter(species_decision == "CHANGE")

species_change_n <- min(30L, nrow(species_change_pool))

species_changes <- species_change_pool |>
  dplyr::slice_sample(n = species_change_n) |>
  dplyr::mutate(validation_set = "Species: LLM change")

species_unresolved <- adjud |>
  dplyr::filter(species_decision == "UNRESOLVED") |>
  dplyr::mutate(validation_set = "Species: unresolved")

species_validation <- dplyr::bind_rows(
  species_changes,
  species_unresolved
) |>
  dplyr::transmute(
    validation_set,
    record_id,
    title,
    abstract,
    deterministic_species,
    deterministic_species_ids,
    species_reasons,
    non_target_species,
    llm_species,
    species_decision,
    species_reason,
    human_species_decision = NA_character_,
    human_species_final = NA_character_,
    human_species_notes = NA_character_
  )

geography_change_pool <- adjud |>
  dplyr::filter(geography_decision == "CHANGE")

geography_change_n <- min(30L, nrow(geography_change_pool))

geography_changes <- geography_change_pool |>
  dplyr::slice_sample(n = geography_change_n) |>
  dplyr::mutate(validation_set = "Geography: LLM change")

geography_unresolved <- adjud |>
  dplyr::filter(geography_decision == "UNRESOLVED") |>
  dplyr::mutate(validation_set = "Geography: unresolved")

geography_validation <- dplyr::bind_rows(
  geography_changes,
  geography_unresolved
) |>
  dplyr::transmute(
    validation_set,
    record_id,
    title,
    abstract,
    deterministic_primary_countries,
    deterministic_primary_iso3c,
    geography_review_reason,
    geography_candidates,
    llm_primary_country_iso3c,
    geography_decision,
    geography_reason,
    human_geography_decision = NA_character_,
    human_geography_final = NA_character_,
    human_geography_notes = NA_character_
  )

summary <- tibble::tibble(
  validation_set = c(
    "Species: LLM change",
    "Species: unresolved",
    "Geography: LLM change",
    "Geography: unresolved"
  ),
  n = c(
    nrow(species_changes),
    nrow(species_unresolved),
    nrow(geography_changes),
    nrow(geography_unresolved)
  )
)

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop(
    "Package 'openxlsx' is required. Install it with ",
    "install.packages('openxlsx')."
  )
}

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "README")
openxlsx::writeData(
  wb,
  "README",
  tibble::tibble(
    item = c(
      "Purpose",
      "Species change sample",
      "Species unresolved",
      "Geography change sample",
      "Geography unresolved",
      "How to code",
      "Important"
    ),
    instruction = c(
      "Validate whether LLM adjudication is better supported by the title/abstract than the deterministic result.",
      "30 randomly sampled LLM species changes.",
      "All unresolved species cases.",
      "30 randomly sampled LLM geography changes.",
      "All unresolved geography cases.",
      "Enter ACCEPT, CHANGE or UNCERTAIN in the human_*_decision columns. If changing, enter the final value in human_*_final and explain it in human_*_notes.",
      "Do not alter deterministic or LLM columns."
    )
  )
)

openxlsx::addWorksheet(wb, "Summary")
openxlsx::writeData(wb, "Summary", summary)

openxlsx::addWorksheet(wb, "Species")
openxlsx::writeData(wb, "Species", species_validation)

openxlsx::addWorksheet(wb, "Geography")
openxlsx::writeData(wb, "Geography", geography_validation)

openxlsx::freezePane(wb, "Species", firstRow = TRUE)
openxlsx::freezePane(wb, "Geography", firstRow = TRUE)

openxlsx::addFilter(
  wb, "Species", rows = 1,
  cols = seq_len(ncol(species_validation))
)

openxlsx::addFilter(
  wb, "Geography", rows = 1,
  cols = seq_len(ncol(geography_validation))
)

openxlsx::setColWidths(
  wb, "README", cols = 1:2, widths = c(28, 100)
)

openxlsx::setColWidths(
  wb, "Summary", cols = 1:2, widths = c(32, 10)
)

openxlsx::setColWidths(
  wb, "Species",
  cols = seq_len(ncol(species_validation)),
  widths = "auto"
)

openxlsx::setColWidths(
  wb, "Geography",
  cols = seq_len(ncol(geography_validation)),
  widths = "auto"
)

openxlsx::saveWorkbook(
  wb,
  output_file,
  overwrite = TRUE
)

message("")
message("Validation workbook created:")
message(output_file)
message("")
print(summary)
