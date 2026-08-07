# =============================================================================
# File: scripts/65_remove_retractions_and_notices.R
# Purpose: Remove retracted works and publication notices from the screened
#          update using the free OpenAlex API.
#
# Rules:
#   1. Titles beginning with retraction, withdrawn, correction, corrigendum or
#      erratum are removed as notices.
#   2. DOI-bearing records are looked up exactly in OpenAlex.
#   3. Records for which OpenAlex returns is_retracted = TRUE are removed.
#   4. DOI lookup failures or missing DOI do not cause removal.
#
# Input:
#   outputs/living_map_screening/screening_audit.csv
#
# Outputs:
#   outputs/living_map_screening/publication_status/
#     records_cleared_publication_status.csv
#     removed_retractions_and_notices.csv
#     publication_status_audit.csv
# =============================================================================

source("scripts/00_setup.R")

input_file <- here::here(
  "outputs",
  "living_map_screening",
  "screening_audit.csv"
)

output_dir <- here::here(
  "outputs",
  "living_map_screening",
  "publication_status"
)

fs::dir_create(output_dir)

cleared_file <- fs::path(
  output_dir,
  "records_cleared_publication_status.csv"
)

removed_file <- fs::path(
  output_dir,
  "removed_retractions_and_notices.csv"
)

audit_file <- fs::path(
  output_dir,
  "publication_status_audit.csv"
)

checkpoint_file <- fs::path(
  output_dir,
  "openalex_retraction_checkpoint.rds"
)

stopifnot(
  file.exists(input_file)
)

api_key <- Sys.getenv("OPENALEX_API_KEY")

if (!nzchar(api_key)) {
  stop(
    "OPENALEX_API_KEY was not found. Create a free OpenAlex key and set it with:\n",
    "Sys.setenv(OPENALEX_API_KEY = \"your-key\")"
  )
}

records <- readr::read_csv(
  input_file,
  show_col_types = FALSE,
  col_types = readr::cols(
    record_id = readr::col_character()
  )
) |>
  dplyr::mutate(
    publication_status_row = dplyr::row_number(),
    title = dplyr::coalesce(as.character(title), ""),
    doi_for_lookup = dplyr::coalesce(
      as.character(doi_key),
      as.character(doi),
      ""
    ) |>
      stringr::str_to_lower() |>
      stringr::str_remove("^https?://(dx\\.)?doi\\.org/") |>
      stringr::str_remove("^doi:\\s*") |>
      stringr::str_trim(),
    notice_type = dplyr::case_when(
      stringr::str_detect(
        title,
        stringr::regex(
          "^\\s*(retraction(?:\\s+notice)?|retracted)\\s*[:\\-—.]",
          ignore_case = TRUE
        )
      ) ~ "retraction_notice",
      stringr::str_detect(
        title,
        stringr::regex(
          "^\\s*(withdrawn|withdrawal)\\s*[:\\-—.]",
          ignore_case = TRUE
        )
      ) ~ "withdrawal_notice",
      stringr::str_detect(
        title,
        stringr::regex(
          "^\\s*(correction|corrigendum|erratum)\\s*[:\\-—.]",
          ignore_case = TRUE
        )
      ) ~ "correction_notice",
      TRUE ~ NA_character_
    )
  )

# Do not query records already identified as notices.
doi_records <- records |>
  dplyr::filter(
    is.na(notice_type),
    nzchar(doi_for_lookup)
  ) |>
  dplyr::distinct(
    doi_for_lookup
  )

lookup_one_doi <- function(doi) {
  url <- paste0(
    "https://api.openalex.org/works/doi:",
    utils::URLencode(
      doi,
      reserved = TRUE
    )
  )

  result <- tryCatch(
    {
      response <- httr2::request(url) |>
        httr2::req_url_query(
          api_key = api_key,
          select = "id,doi,display_name,is_retracted"
        ) |>
        httr2::req_timeout(30) |>
        httr2::req_retry(
          max_tries = 4,
          backoff = ~ 2^.x
        ) |>
        httr2::req_perform()

      body <- httr2::resp_body_json(
        response,
        simplifyVector = TRUE
      )

      tibble::tibble(
        doi_for_lookup = doi,
        openalex_id = dplyr::coalesce(
          as.character(body$id),
          NA_character_
        ),
        openalex_title = dplyr::coalesce(
          as.character(body$display_name),
          NA_character_
        ),
        openalex_is_retracted = isTRUE(
          body$is_retracted
        ),
        openalex_lookup_status = "matched",
        openalex_error = NA_character_
      )
    },
    httr2_http_404 = function(e) {
      tibble::tibble(
        doi_for_lookup = doi,
        openalex_id = NA_character_,
        openalex_title = NA_character_,
        openalex_is_retracted = FALSE,
        openalex_lookup_status = "not_found",
        openalex_error = NA_character_
      )
    },
    error = function(e) {
      tibble::tibble(
        doi_for_lookup = doi,
        openalex_id = NA_character_,
        openalex_title = NA_character_,
        openalex_is_retracted = FALSE,
        openalex_lookup_status = "failed",
        openalex_error = conditionMessage(e)
      )
    }
  )

  result
}

if (file.exists(checkpoint_file)) {
  lookup_results <- readRDS(
    checkpoint_file
  )

  completed_dois <- unique(
    lookup_results$doi_for_lookup
  )

  message(
    "Resuming OpenAlex retraction checks: ",
    length(completed_dois),
    " / ",
    nrow(doi_records),
    " DOIs completed."
  )
} else {
  lookup_results <- tibble::tibble()
  completed_dois <- character()

  message(
    "Checking ",
    nrow(doi_records),
    " unique DOIs in OpenAlex."
  )
}

remaining_dois <- doi_records |>
  dplyr::filter(
    !doi_for_lookup %in% completed_dois
  )

for (i in seq_len(nrow(remaining_dois))) {
  current_doi <- remaining_dois$doi_for_lookup[[i]]

  message(
    "OpenAlex DOI check ",
    length(completed_dois) + i,
    " / ",
    nrow(doi_records)
  )

  lookup_results <- dplyr::bind_rows(
    lookup_results,
    lookup_one_doi(current_doi)
  )

  saveRDS(
    lookup_results,
    checkpoint_file
  )
}

audit <- records |>
  dplyr::left_join(
    lookup_results,
    by = "doi_for_lookup"
  ) |>
  dplyr::mutate(
    openalex_lookup_status = dplyr::case_when(
      !is.na(notice_type) ~ "not_queried_notice",
      !nzchar(doi_for_lookup) ~ "not_queried_no_doi",
      TRUE ~ dplyr::coalesce(
        openalex_lookup_status,
        "not_queried"
      )
    ),
    openalex_is_retracted = dplyr::coalesce(
      openalex_is_retracted,
      FALSE
    ),
    remove_publication_status = !is.na(notice_type) |
      openalex_is_retracted,
    removal_reason = dplyr::case_when(
      !is.na(notice_type) ~ notice_type,
      openalex_is_retracted ~ "retracted_original",
      TRUE ~ NA_character_
    )
  )

removed <- audit |>
  dplyr::filter(
    remove_publication_status
  )

cleared <- audit |>
  dplyr::filter(
    !remove_publication_status
  )

readr::write_csv(
  audit,
  audit_file,
  na = ""
)

readr::write_csv(
  removed,
  removed_file,
  na = ""
)

readr::write_csv(
  cleared,
  cleared_file,
  na = ""
)

summary <- tibble::tibble(
  item = c(
    "Records checked",
    "Title-identified notices removed",
    "OpenAlex-retracted originals removed",
    "Records cleared",
    "DOI lookups not found",
    "DOI lookup failures"
  ),
  value = c(
    nrow(audit),
    sum(!is.na(audit$notice_type)),
    sum(audit$openalex_is_retracted),
    nrow(cleared),
    sum(audit$openalex_lookup_status == "not_found"),
    sum(audit$openalex_lookup_status == "failed")
  )
)

message("")
message("Publication-status screening completed.")
print(summary)
message("")
message("Cleared records: ", cleared_file)
message("Removed records: ", removed_file)
