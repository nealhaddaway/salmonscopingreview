# =============================================================================
# File: 33_create_primary_country_validation.R
# Purpose: Create validation workbook for primary study-country classifier
# =============================================================================

source("scripts/00_setup.R")
source("R/read_corpus.R")

set.seed(20260803)

summary_file <- here::here(
  "outputs","stage_5_geography","primary_study_country",
  "primary_country_summary.csv"
)

ranking_file <- here::here(
  "outputs","stage_5_geography","primary_study_country",
  "country_evidence_ranking.csv"
)

records_file <- here::here(
  "data_raw","INCLUDES fixed abstracts.txt"
)

out_dir <- here::here(
  "outputs","stage_5_geography","primary_study_country","validation"
)

fs::dir_create(out_dir)

records <- read_corpus(records_file) |>
  dplyr::mutate(record_id = as.character(record_id))

summary_tbl <- readr::read_csv(summary_file, show_col_types = FALSE) |>
  dplyr::mutate(record_id = as.character(record_id))

ranking <- readr::read_csv(ranking_file, show_col_types = FALSE) |>
  dplyr::mutate(record_id = as.character(record_id))

sample_up_to <- function(df,n){
  if(nrow(df)<=n) df else dplyr::slice_sample(df,n=n)
}

title_sample <- summary_tbl |>
  dplyr::filter(stringr::str_length(primary_countries)>0) |>
  dplyr::semi_join(
    ranking |> dplyr::filter(best_tier==1) |> dplyr::distinct(record_sequence),
    by="record_sequence"
  ) |>
  sample_up_to(20) |>
  dplyr::mutate(stratum="Title-derived")

abstract_sample <- summary_tbl |>
  dplyr::filter(stringr::str_length(primary_countries)>0) |>
  dplyr::anti_join(
    ranking |> dplyr::filter(best_tier==1) |> dplyr::distinct(record_sequence),
    by="record_sequence"
  ) |>
  dplyr::filter(primary_country_count==1) |>
  sample_up_to(20) |>
  dplyr::mutate(stratum="Abstract-derived")

two_country_sample <- summary_tbl |>
  dplyr::filter(primary_country_count==2) |>
  sample_up_to(10) |>
  dplyr::mutate(stratum="Two-country")

review_records <- summary_tbl |>
  dplyr::filter(review_required) |>
  dplyr::mutate(stratum="Review")

validation <- dplyr::bind_rows(
  title_sample,
  abstract_sample,
  two_country_sample,
  review_records
) |>
  dplyr::distinct(record_sequence,.keep_all=TRUE) |>
  dplyr::left_join(
    records |> dplyr::select(record_sequence,title,abstract),
    by="record_sequence",
    suffix=c("","_corpus")
  ) |>
  dplyr::mutate(
    correct=NA_character_,
    corrected_country=NA_character_,
    notes=NA_character_
  )

wb <- openxlsx2::wb_workbook()
wb$add_worksheet("Validation")
wb$add_data("Validation",validation)
wb$freeze_pane(sheet="Validation",first_row=TRUE)

outfile <- fs::path(out_dir,"primary_country_validation.xlsx")
wb$save(outfile,overwrite=TRUE)

message("Validation workbook created.")
message("Records: ",nrow(validation))
message("Workbook: ",outfile)
