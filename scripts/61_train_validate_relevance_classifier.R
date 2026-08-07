# =============================================================================
# File: scripts/61_train_validate_relevance_classifier.R
# Purpose: Train and validate a title-and-abstract relevance classifier.
#          No RIS document-type or bibliographic metadata is used as a model
#          predictor.
# =============================================================================

source("scripts/00_setup.R")
source("R/relevance_screening.R")

ensure_relevance_packages()

training_file <- here::here(
  "outputs",
  "stage_5_relevance_screening",
  "training",
  "relevance_training_records.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_5_relevance_screening",
  "model"
)

fs::dir_create(output_dir)

stopifnot(
  file.exists(training_file)
)

records <- readr::read_csv(
  training_file,
  show_col_types = FALSE,
  col_types = readr::cols(
    record_id = readr::col_character(),
    validation = readr::col_logical()
  )
)

model <- fit_relevance_model(records)

validation <- records |>
  dplyr::filter(validation) |>
  dplyr::mutate(
    probability_relevant = predict_relevance_probability(
      model,
      dplyr::pick(dplyr::everything())
    )
  )

thresholds <- select_operating_thresholds(
  truth = validation$eligibility,
  probability = validation$probability_relevant,
  target_sensitivity = 0.99,
  target_precision = 0.95
)

validation <- validation |>
  dplyr::mutate(
    screening_decision = assign_screening_decision(
      probability_relevant,
      thresholds
    )
  )

decision_summary <- validation |>
  dplyr::count(
    eligibility,
    screening_decision,
    name = "records"
  )

operating_metrics <- dplyr::bind_rows(
  classification_metrics(
    validation$eligibility,
    validation$probability_relevant,
    thresholds$exclude_threshold
  ) |>
    dplyr::mutate(
      operating_point = "automatic-exclude boundary"
    ),
  classification_metrics(
    validation$eligibility,
    validation$probability_relevant,
    thresholds$include_threshold
  ) |>
    dplyr::mutate(
      operating_point = "automatic-retain boundary"
    )
) |>
  dplyr::select(
    operating_point,
    dplyr::everything()
  )

title_only_metrics <- validation |>
  dplyr::group_split(has_abstract) |>
  purrr::map_dfr(
    function(group) {
      classification_metrics(
        group$eligibility,
        group$probability_relevant,
        thresholds$exclude_threshold
      ) |>
        dplyr::mutate(
          record_text = ifelse(
            unique(group$has_abstract),
            "title and abstract",
            "title only"
          )
        )
    }
  ) |>
  dplyr::select(
    record_text,
    dplyr::everything()
  )

saveRDS(
  list(
    model = model,
    thresholds = thresholds[
      c(
        "exclude_threshold",
        "include_threshold",
        "target_sensitivity",
        "target_precision"
      )
    ],
    trained_at = Sys.time(),
    training_rows = nrow(
      records |>
        dplyr::filter(!validation)
    ),
    validation_rows = nrow(validation)
  ),
  fs::path(
    output_dir,
    "salmon_farming_relevance_model.rds"
  )
)

readr::write_csv(
  validation,
  fs::path(
    output_dir,
    "relevance_validation_predictions.csv"
  ),
  na = ""
)

readr::write_csv(
  operating_metrics,
  fs::path(
    output_dir,
    "relevance_operating_metrics.csv"
  ),
  na = ""
)

readr::write_csv(
  decision_summary,
  fs::path(
    output_dir,
    "relevance_validation_decision_summary.csv"
  ),
  na = ""
)

readr::write_csv(
  title_only_metrics,
  fs::path(
    output_dir,
    "relevance_title_only_metrics.csv"
  ),
  na = ""
)

readr::write_csv(
  thresholds$metrics,
  fs::path(
    output_dir,
    "relevance_threshold_curve.csv"
  ),
  na = ""
)

message("")
message("Relevance classifier trained and validated.")
message(
  "Automatic-exclude threshold: ",
  round(thresholds$exclude_threshold, 4)
)
message(
  "Automatic-retain threshold: ",
  round(thresholds$include_threshold, 4)
)
message("")
print(operating_metrics)
message("")
message(
  "Inspect validation outputs before using automatic exclusion."
)
