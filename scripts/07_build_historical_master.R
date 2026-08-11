# Stage 7: build the frozen historical master corpus --------------------------
#
# Applies the validated species + geography adjudication, the human corrections
# recorded in the validation workbook, and the 31 retrospective screening
# exclusions. Topics are merged from the validated topic-assignment output.
#
# IMPORTANT: this script does not rerun any classifier.
#
# Before running, set SALMON_TOPIC_ASSIGNMENTS to the validated V4 topic
# assignment CSV if it is not at one of the default paths below.

source("scripts/00_setup.R")
source("R/read_corpus.R")

input_records <- here::here("data_raw", "INCLUDES fixed abstracts.txt")
species_assignments_file <- here::here("outputs", "stage_2_species", "species_assignments.csv")
geo_summary_file <- here::here("outputs", "stage_5_geography", "primary_study_country_v2", "primary_country_summary_v2.csv")
adjudication_file <- here::here("outputs", "stage_2_5_annotation_adjudication", "species_geography_adjudication_full.csv")
exclusions_file <- here::here("data_manual", "historical_screening_corrections.csv")
geo_overrides_file <- here::here("data_manual", "historical_geography_overrides.csv")

find_topic_file <- function() {
  env_file <- Sys.getenv("SALMON_TOPIC_ASSIGNMENTS", "")
  candidates <- unique(c(
    env_file,
    here::here("outputs", "stage_3_topics_v4", "topic_assignments.csv"),
    here::here("outputs", "stage_3_topics_v4", "topic_assignments_v4.csv"),
    here::here("outputs", "stage_3_topics", "topic_assignments.csv")
  ))
  candidates <- candidates[nzchar(candidates)]
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 1L) return(existing[[1]])
  if (length(existing) == 0L) {
    stop(
      "No topic assignment file found. Set SALMON_TOPIC_ASSIGNMENTS to the " ,
      "validated V4 topic_assignments.csv before running this script."
    )
  }
  stop(
    "More than one topic assignment candidate exists. Set " ,
    "SALMON_TOPIC_ASSIGNMENTS explicitly to the validated V4 file.\n",
    paste(existing, collapse = "\n")
  )
}

topic_file <- find_topic_file()

required <- c(input_records, species_assignments_file, geo_summary_file,
              adjudication_file, exclusions_file, geo_overrides_file, topic_file)
if (any(!file.exists(required))) {
  stop("One or more required input files do not exist.")
}

records <- read_corpus(input_records) |>
  dplyr::mutate(record_id = as.character(record_id))

exclusions <- readr::read_csv(exclusions_file, show_col_types = FALSE) |>
  dplyr::mutate(record_id = as.character(record_id))

overrides <- readr::read_csv(geo_overrides_file, show_col_types = FALSE) |>
  dplyr::mutate(record_id = as.character(record_id))

if (anyDuplicated(exclusions$record_id) > 0L) stop("Duplicate screening-correction record_id values.")
if (anyDuplicated(overrides$record_id) > 0L) stop("Duplicate geography-override record_id values.")

# -----------------------------------------------------------------------------
# Species: deterministic assignments + validated LLM adjudication
# -----------------------------------------------------------------------------

species <- readr::read_csv(species_assignments_file, show_col_types = FALSE) |>
  dplyr::mutate(record_id = as.character(record_id))

adjud <- readr::read_csv(adjudication_file, show_col_types = FALSE) |>
  dplyr::mutate(record_id = as.character(record_id))

species_llm <- adjud |>
  dplyr::filter(species_review_required %in% TRUE) |>
  dplyr::transmute(
    record_id,
    species_decision,
    llm_species,
    species_reason,
    llm_failed
  )

if (anyDuplicated(species_llm$record_id) > 0L) stop("Duplicate species adjudication rows.")

species_final <- species |>
  dplyr::group_by(record_id) |>
  dplyr::summarise(
    deterministic_species = paste(sort(unique(stats::na.omit(farmed_species))), collapse = "; "),
    deterministic_species_review = any(review_required %in% TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(deterministic_species = dplyr::na_if(deterministic_species, "")) |>
  dplyr::left_join(species_llm, by = "record_id") |>
  dplyr::mutate(
    final_species = dplyr::case_when(
      is.na(species_decision) ~ deterministic_species,
      species_decision == "ACCEPT" ~ deterministic_species,
      species_decision == "CHANGE" ~ llm_species,
      species_decision == "UNRESOLVED" ~ llm_species,
      TRUE ~ deterministic_species
    ),
    species_source = dplyr::case_when(
      is.na(species_decision) ~ "deterministic",
      species_decision == "ACCEPT" ~ "deterministic + LLM accepted",
      species_decision == "CHANGE" ~ "LLM adjudication",
      species_decision == "UNRESOLVED" ~ "LLM adjudication accepted after human validation",
      TRUE ~ "deterministic"
    )
  ) |>
  dplyr::select(record_id, final_species, species_source, species_decision, llm_species, species_reason)

# -----------------------------------------------------------------------------
# Geography: final v2 deterministic output + validated LLM adjudication +
# human overrides. Multiple ISO3 countries are permitted.
# -----------------------------------------------------------------------------

geo <- readr::read_csv(geo_summary_file, show_col_types = FALSE) |>
  dplyr::mutate(record_id = as.character(record_id))

geo_llm <- adjud |>
  dplyr::filter(geography_review_required %in% TRUE) |>
  dplyr::transmute(
    record_id,
    geography_decision,
    llm_primary_country_iso3c,
    geography_reason,
    llm_failed
  )

if (anyDuplicated(geo_llm$record_id) > 0L) stop("Duplicate geography adjudication rows.")

# Preserve the existing geography output column where possible.
geo_country_col <- if ("primary_iso3c" %in% names(geo)) "primary_iso3c" else if ("primary_countries" %in% names(geo)) "primary_countries" else stop("Geography summary lacks primary_iso3c/primary_countries.")

geo_base <- geo |>
  dplyr::transmute(
    record_id,
    deterministic_primary_country = .data[[geo_country_col]],
    geography_review_required = dplyr::coalesce(as.logical(review_required), FALSE)
  )

geo_final <- geo_base |>
  dplyr::left_join(geo_llm, by = "record_id") |>
  dplyr::left_join(overrides |>
                     dplyr::select(record_id, human_primary_country = final_primary_country_iso3c, override_reason),
                   by = "record_id") |>
  dplyr::mutate(
    llm_country = stringr::str_replace_all(dplyr::coalesce(llm_primary_country_iso3c, ""), ",", ";"),
    final_primary_country_iso3c = dplyr::case_when(
      !is.na(human_primary_country) ~ stringr::str_replace_all(human_primary_country, "\\s*[,;]\\s*", ";"),
      is.na(geography_decision) ~ deterministic_primary_country,
      geography_decision == "ACCEPT" ~ deterministic_primary_country,
      geography_decision %in% c("CHANGE", "UNRESOLVED") ~ dplyr::na_if(llm_country, ""),
      TRUE ~ deterministic_primary_country
    ),
    geography_source = dplyr::case_when(
      !is.na(human_primary_country) ~ "human override",
      is.na(geography_decision) ~ "deterministic",
      geography_decision == "ACCEPT" ~ "deterministic + LLM accepted",
      geography_decision == "CHANGE" ~ "LLM adjudication",
      geography_decision == "UNRESOLVED" ~ "LLM adjudication accepted after human validation",
      TRUE ~ "deterministic"
    )
  ) |>
  dplyr::select(record_id, final_primary_country_iso3c, geography_source,
                geography_decision, llm_primary_country_iso3c, geography_reason,
                override_reason)

# -----------------------------------------------------------------------------
# Topics: use the already validated V4 output. Collapse character annotation
# fields to one row per record while retaining the hierarchy.
# -----------------------------------------------------------------------------

topics_raw <- readr::read_csv(topic_file, show_col_types = FALSE) |>
  dplyr::mutate(record_id = as.character(record_id))

if (!"record_id" %in% names(topics_raw)) stop("Topic file lacks record_id.")

character_topic_cols <- setdiff(
  names(topics_raw)[vapply(topics_raw, is.character, logical(1))],
  "record_id"
)

if (length(character_topic_cols) == 0L) stop("Topic file contains no character topic fields.")

collapse_unique <- function(x) {
  x <- sort(unique(stats::na.omit(stringr::str_squish(as.character(x)))))
  x <- x[nzchar(x)]
  if (!length(x)) NA_character_ else paste(x, collapse = "; ")
}

topic_summary <- topics_raw |>
  dplyr::group_by(record_id) |>
  dplyr::summarise(
    dplyr::across(dplyr::all_of(character_topic_cols), collapse_unique),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# Assemble and apply the 31 retrospective screening corrections.
# -----------------------------------------------------------------------------

master <- records |>
  dplyr::anti_join(exclusions |> dplyr::select(record_id), by = "record_id") |>
  dplyr::left_join(species_final, by = "record_id") |>
  dplyr::left_join(geo_final, by = "record_id") |>
  dplyr::left_join(topic_summary, by = "record_id") |>
  dplyr::mutate(
    historical_screening_correction = FALSE
  )

if (nrow(master) != 12043L) {
  stop("Expected 12,043 final historical records after 31 exclusions; found ", nrow(master), ".")
}

if (anyDuplicated(master$record_id) > 0L) stop("Duplicate record_id values in final master.")

output_dir <- here::here("outputs", "master")
fs::dir_create(output_dir)

readr::write_csv(master, fs::path(output_dir, "historical_master_12043.csv"), na = "")

summary <- tibble::tibble(
  measure = c(
    "Original records",
    "Retrospective screening exclusions",
    "Final historical records",
    "Records with species",
    "Records with primary geography",
    "Records with topic annotation"
  ),
  value = c(
    nrow(records),
    nrow(exclusions),
    nrow(master),
    sum(!is.na(master$final_species) & master$final_species != ""),
    sum(!is.na(master$final_primary_country_iso3c) & master$final_primary_country_iso3c != ""),
    sum(rowSums(!is.na(master[, intersect(character_topic_cols, names(master)), drop = FALSE])) > 0)
  )
)

readr::write_csv(summary, fs::path(output_dir, "historical_master_summary.csv"), na = "")

message("Historical master assembled successfully.")
print(summary)
