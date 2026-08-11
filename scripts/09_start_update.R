# =============================================================================
# File: scripts/09_start_update.R
# Purpose: Archive the current master and immutable incoming search files before
#          an update begins. This script does NOT process or overwrite the master.
#
# Usage:
#   source("scripts/09_start_update.R")
#
# Before running, place new search-result files in:
#   data_updates/incoming/
#
# The script creates an update-specific archive and a PRISMA/ROSES-oriented
# manifest. The current master is copied before any processing occurs.
# =============================================================================

source("scripts/00_setup.R")

update_id <- paste0(
  "UPDATE_",
  format(Sys.Date(), "%Y-%m-%d")
)

current_file <- here::here(
  "data_current", "salmon_evidence_map.csv"
)
incoming_dir <- here::here("data_updates", "incoming")

archive_master_dir <- here::here("data_archive", "masters")
archive_incoming_dir <- here::here("data_archive", "incoming", update_id)
archive_update_dir <- here::here("data_archive", "updates", update_id)
manifest_dir <- here::here("data_archive", "manifests")

if (!file.exists(current_file)) {
  stop(
    "No canonical current master found at ", current_file,
    ". Run scripts/08_initialise_living_evidence_map.R first."
  )
}

if (!dir.exists(incoming_dir)) {
  stop(
    "Incoming directory does not exist: ", incoming_dir,
    "\nCreate it and place the new search-result files there."
  )
}

incoming_files <- fs::dir_ls(
  incoming_dir,
  type = "file",
  recurse = FALSE
)

if (length(incoming_files) == 0L) {
  stop("No incoming search-result files found in ", incoming_dir)
}

if (any(grepl("\\.xlsx$", incoming_files, ignore.case = TRUE))) {
  warning("An XLSX file is present. Prefer RIS/CSV/plain database exports where possible.")
}

# Prevent accidental reuse of the same update date.
if (dir.exists(archive_incoming_dir) || dir.exists(archive_update_dir)) {
  stop(
    "An update with ID ", update_id, " already exists. ",
    "Use a new date/update ID rather than overwriting an existing update."
  )
}

fs::dir_create(archive_master_dir)
fs::dir_create(archive_incoming_dir, recurse = TRUE)
fs::dir_create(archive_update_dir, recurse = TRUE)
fs::dir_create(manifest_dir)

master <- readr::read_csv(current_file, show_col_types = FALSE)

if (!"record_id" %in% names(master)) {
  stop("Current master does not contain record_id.")
}

if (anyDuplicated(master$record_id) > 0L) {
  stop("Current master contains duplicate record_id values.")
}

# Archive the exact current master before any update processing.
archived_master <- fs::path(
  archive_master_dir,
  paste0("salmon_evidence_map_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
)

if (file.exists(archived_master)) {
  stop(
    "Archive already exists: ", archived_master,
    "\nThis protects the previous master from accidental overwrite."
  )
}

fs::file_copy(current_file, archived_master, overwrite = FALSE)

# Archive every incoming file byte-for-byte under the update ID.
for (f in incoming_files) {
  fs::file_copy(
    f,
    fs::path(archive_incoming_dir, fs::path_file(f)),
    overwrite = FALSE
  )
}

# Create an initial record-level manifest with one row per source file.
manifest <- tibble::tibble(
  update_id = update_id,
  update_date = as.character(Sys.Date()),
  previous_master_file = fs::path_file(archived_master),
  source = tools::file_path_sans_ext(fs::path_file(incoming_files)),
  source_file = fs::path_file(incoming_files),
  search_date = NA_character_,
  search_strategy_version = NA_character_,
  records_identified = NA_integer_,
  within_source_duplicates = NA_integer_,
  cross_source_duplicates = NA_integer_,
  duplicates_previous_master = NA_integer_,
  records_after_deduplication = NA_integer_,
  retractions_removed = NA_integer_,
  corrections_errata_removed = NA_integer_,
  other_pre_screen_exclusions = NA_integer_,
  records_screened = NA_integer_,
  automated_exclusions = NA_integer_,
  llm_exclusions = NA_integer_,
  human_exclusions = NA_integer_,
  records_retained_after_screening = NA_integer_,
  records_added_to_master = NA_integer_,
  final_master_records = NA_integer_
)

manifest_file <- fs::path(
  manifest_dir,
  paste0(update_id, ".csv")
)

readr::write_csv(manifest, manifest_file, na = "")

message("Update initialised: ", update_id)
message("Previous master archived: ", archived_master)
message("Incoming files archived: ", archive_incoming_dir)
message("Manifest: ", manifest_file)
message("Previous master records: ", nrow(master))
message("Incoming files: ", length(incoming_files))
