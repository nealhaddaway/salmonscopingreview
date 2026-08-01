# =============================================================================
# File: 07_create_topic_validation.R
# Project: salmonscopingreview
# Purpose: Create validation datasets for topic assignment
# =============================================================================

source("scripts/00_setup.R")
source("R/read_corpus.R")

#--------------------------------------------------------------------
# Input paths
#--------------------------------------------------------------------

input_records <- here::here(
  "data_raw",
  "INCLUDES fixed abstracts.txt"
)

input_assignments <- here::here(
  "outputs",
  "stage_3_topics",
  "topic_assignments.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_3_validation"
)

fs::dir_create(output_dir)

records <- read_corpus(input_records)

topic_assignments <- readr::read_csv(
  input_assignments,
  show_col_types = FALSE
)

#--------------------------------------------------------------------
# Collapse to one row per record
#--------------------------------------------------------------------

record_topics <-
  topic_assignments |>
  dplyr::group_by(
    record_sequence,
    record_id
  ) |>
  dplyr::summarise(
    
    topic_n =
      dplyr::n_distinct(
        component,
        na.rm = TRUE
      ),
    
    assigned_topics =
      dplyr::if_else(
        all(is.na(component)),
        NA_character_,
        paste(
          sort(
            unique(
              stats::na.omit(component)
            )
          ),
          collapse = "; "
        )
      ),
    
    review_required =
      any(review_required),
    
    assignment_reason =
      paste(
        sort(
          unique(
            assignment_reason
          )
        ),
        collapse = "; "
      ),
    
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
    
  )

#--------------------------------------------------------------------
# Validation helper
#--------------------------------------------------------------------

add_validation_columns <- function(df) {
  
  df |>
    
    dplyr::mutate(
      
      validation_correct = NA_character_,
      validation_topics = NA_character_,
      validation_notes = NA_character_
      
    )
  
}

#--------------------------------------------------------------------
# Samples
#--------------------------------------------------------------------

set.seed(20260801)

automatic_sample <-
  
  record_topics |>
  
  dplyr::filter(
    
    !review_required
    
  ) |>
  
  dplyr::slice_sample(
    
    n = 50
    
  )

manual_review_all <-
  
  record_topics |>
  
  dplyr::filter(
    
    review_required
    
  )

high_topic_sample <-
  
  record_topics |>
  
  dplyr::filter(
    
    !review_required
    
  ) |>
  
  dplyr::arrange(
    
    dplyr::desc(topic_n)
    
  ) |>
  
  dplyr::slice_head(
    
    n = 250
    
  ) |>
  
  dplyr::slice_sample(
    
    n = 50
    
  )

single_topic_sample <-
  
  record_topics |>
  
  dplyr::filter(
    
    topic_n == 1,
    !review_required
    
  ) |>
  
  dplyr::slice_sample(
    
    n = 50
    
  )

automatic_sample <- add_validation_columns(
  automatic_sample
)

manual_review_all <- add_validation_columns(
  manual_review_all
)

high_topic_sample <- add_validation_columns(
  high_topic_sample
)

single_topic_sample <- add_validation_columns(
  single_topic_sample
)

#--------------------------------------------------------------------
# Write CSVs
#--------------------------------------------------------------------

readr::write_csv(
  
  automatic_sample,
  
  fs::path(
    output_dir,
    "topic_validation_random50.csv"
  )
  
)

readr::write_csv(
  
  manual_review_all,
  
  fs::path(
    output_dir,
    "topic_validation_manual.csv"
  )
  
)

readr::write_csv(
  
  high_topic_sample,
  
  fs::path(
    output_dir,
    "topic_validation_high_topics50.csv"
  )
  
)

readr::write_csv(
  
  single_topic_sample,
  
  fs::path(
    output_dir,
    "topic_validation_single_topic50.csv"
  )
  
)

#--------------------------------------------------------------------
# Workbook
#--------------------------------------------------------------------

wb <- openxlsx2::wb_workbook()

wb$add_worksheet(
  "Random 50"
)

wb$add_data(
  "Random 50",
  automatic_sample
)

wb$add_worksheet(
  "Manual review"
)

wb$add_data(
  "Manual review",
  manual_review_all
)

wb$add_worksheet(
  "High topic count"
)

wb$add_data(
  "High topic count",
  high_topic_sample
)

wb$add_worksheet(
  "Single topic"
)

wb$add_data(
  "Single topic",
  single_topic_sample
)

wb$save(
  
  fs::path(
    output_dir,
    "topic_validation_workbook.xlsx"
  ),
  
  overwrite = TRUE
  
)

cli::cli_alert_success(
  "Topic validation workbook written."
)

cli::cli_alert_info(
  "Random sample: {nrow(automatic_sample)}"
)

cli::cli_alert_info(
  "Manual review: {nrow(manual_review_all)}"
)

cli::cli_alert_info(
  "High-topic sample: {nrow(high_topic_sample)}"
)

cli::cli_alert_info(
  "Single-topic sample: {nrow(single_topic_sample)}"
)