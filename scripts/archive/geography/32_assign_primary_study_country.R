# =============================================================================
# File: 32_assign_primary_study_country.R
# Purpose: Assign one primary study country (or two only if genuinely co-primary)
# =============================================================================

source("scripts/00_setup.R")

mentions_file <- here::here(
  "outputs","stage_5_geography","global_detection_v3",
  "global_geography_mentions_v3.csv"
)

out_dir <- here::here(
  "outputs","stage_5_geography","primary_study_country"
)

fs::dir_create(out_dir)

mentions <- readr::read_csv(
  mentions_file,
  show_col_types = FALSE
) |>
dplyr::filter(!is.na(iso3c), nzchar(iso3c)) |>
dplyr::mutate(
  context_lower = stringr::str_to_lower(dplyr::coalesce(context,"")),
  source = stringr::str_to_lower(source),

  tier = dplyr::case_when(
    source=="title" ~ 1L,

    stringr::str_detect(
      context_lower,
      "study area|study site|study sites|conducted in|sampled|collected|surveyed|fieldwork|case study|farms in|farm in|hatcher|site in|located in"
    ) ~ 2L,

    stringr::str_detect(
      context_lower,
      "stakeholder|stakeholders|farm|farms|company|companies|industry|producer|population|community|river|lake|coast"
    ) ~ 3L,

    stringr::str_detect(
      context_lower,
      "copyright|published by|springer|elsevier|wiley|translate with|software|manufacturer|supplied by"
    ) ~ 99L,

    TRUE ~ 4L
  )
) |>
dplyr::filter(tier < 99L)

country_rank <- mentions |>
dplyr::group_by(record_sequence,record_id,iso3c,country_name) |>
dplyr::summarise(
  best_tier=min(tier),
  title_mentions=sum(source=="title"),
  total_mentions=dplyr::n(),
  .groups="drop"
) |>
dplyr::arrange(
  record_sequence,
  best_tier,
  dplyr::desc(title_mentions),
  dplyr::desc(total_mentions),
  country_name
)

primary <- country_rank |>
dplyr::group_by(record_sequence,record_id) |>
dplyr::mutate(
  min_tier=min(best_tier),
  max_mentions=max(total_mentions[best_tier==min_tier]),
  keep=
    best_tier==min_tier &
    total_mentions==max_mentions
) |>
dplyr::filter(keep) |>
dplyr::mutate(
  co_primary=dplyr::n()>1
) |>
dplyr::ungroup()

summary <- primary |>
dplyr::group_by(record_sequence,record_id) |>
dplyr::summarise(
 primary_countries=paste(country_name,collapse="; "),
 primary_iso3c=paste(iso3c,collapse="; "),
 primary_country_count=dplyr::n(),
 review_required=primary_country_count>2,
 .groups="drop"
)

readr::write_csv(country_rank,fs::path(out_dir,"country_evidence_ranking.csv"),na="")
readr::write_csv(primary,fs::path(out_dir,"primary_country_assignments.csv"),na="")
readr::write_csv(summary,fs::path(out_dir,"primary_country_summary.csv"),na="")

message("Primary country classifier complete.")
message("Records assigned: ",nrow(summary))
message("Single-country assignments: ",sum(summary$primary_country_count==1))
message("Two-country assignments: ",sum(summary$primary_country_count==2))
message("Review required: ",sum(summary$review_required))
