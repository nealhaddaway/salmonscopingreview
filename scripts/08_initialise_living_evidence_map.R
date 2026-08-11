# =============================================================================
# File: scripts/08_initialise_living_evidence_map.R
# Purpose: Promote the validated historical master to the canonical current
#          living-evidence-map location and create the archive structure.
#
# This is a one-time initialisation step. Future updates must use
# scripts/09_start_update.R rather than overwriting the current master.
# =============================================================================

source("scripts/00_setup.R")

historical_master <- here::here(
  "outputs", "master", "historical_master_12043.csv"
)

current_dir <- here::here("data_current")
archive_master_dir <- here::here("data_archive", "masters")
archive_incoming_dir <- here::here("data_archive", "incoming")
archive_update_dir <- here::here("data_archive", "updates")
manifest_dir <- here::here("data_archive", "manifests")

required <- c(historical_master)
if (any(!file.exists(required))) {
  stop("Historical master not found: ", historical_master)
}

fs::dir_create(current_dir)
fs::dir_create(archive_master_dir)
fs::dir_create(archive_incoming_dir)
fs::dir_create(archive_update_dir)
fs::dir_create(manifest_dir)

current_file <- fs::path(current_dir, "salmon_evidence_map.csv")

if (file.exists(current_file)) {
  stop(
    "Canonical current master already exists: ", current_file,
    "\nDo not overwrite it with the initialisation script."
  )
}

master <- readr::read_csv(historical_master, show_col_types = FALSE)

if (nrow(master) != 12043L) {
  stop("Expected 12,043 historical records; found ", nrow(master), ".")
}

if (anyDuplicated(master$record_id) > 0L) {
  stop("Duplicate record_id values found in historical master.")
}

fs::file_copy(historical_master, current_file, overwrite = FALSE)

# The first master is both the baseline and the first archived immutable version.
archive_file <- fs::path(
  archive_master_dir,
  "salmon_evidence_map_2026-08-11.csv"
)

fs::file_copy(current_file, archive_file, overwrite = FALSE)

message("Living evidence map initialised.")
message("Current master: ", current_file)
message("Archived baseline: ", archive_file)
message("Records: ", nrow(master))
