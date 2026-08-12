# =============================================================================
# File: 42_prepare_topic_ontology_v2_validation.R
# Project: salmonscopingreview
# Purpose:
#   1. Generate the fixed ontology artefacts from one source CSV.
#   2. Create a 100-record human validation workbook without any API calls.
#
# This script does NOT alter the existing full-corpus LLM checkpoint or outputs.
# =============================================================================

source("scripts/00_setup.R")
source("R/read_corpus.R")

# -----------------------------------------------------------------------------
# Paths
# -----------------------------------------------------------------------------

corpus_file <- here::here(
  "data_raw",
  "INCLUDES fixed abstracts.txt"
)

ontology_file <- here::here(
  "data_raw",
  "topic_ontology_v2_FIXED.csv"
)

output_dir <- here::here(
  "outputs",
  "stage_4_llm",
  "ontology_v2"
)

validation_dir <- fs::path(
  output_dir,
  "human_validation"
)

fs::dir_create(output_dir)
fs::dir_create(validation_dir)

stopifnot(
  file.exists(corpus_file),
  file.exists(ontology_file)
)

# -----------------------------------------------------------------------------
# Read and validate ontology
# -----------------------------------------------------------------------------

ontology <- readr::read_csv(
  ontology_file,
  show_col_types = FALSE
) |>
  dplyr::rename(
    level_1 = `Level 1`,
    level_2 = `Level 2`,
    level_3 = `Level 3`,
    definition = Definition,
    include_when = `Include when`,
    exclude_when = `Exclude when`,
    examples = Examples
  ) |>
  dplyr::mutate(
    path_id = sprintf(
      "V2_%03d",
      dplyr::row_number()
    ),
    hierarchy_path = paste(
      level_1,
      level_2,
      level_3,
      sep = " > "
    )
  ) |>
  dplyr::select(
    path_id,
    level_1,
    level_2,
    level_3,
    hierarchy_path,
    definition,
    include_when,
    exclude_when,
    examples,
    dplyr::everything()
  )

required_columns <- c(
  "path_id",
  "level_1",
  "level_2",
  "level_3",
  "hierarchy_path",
  "definition",
  "include_when",
  "exclude_when",
  "examples"
)

missing_columns <- setdiff(
  required_columns,
  names(ontology)
)

if (length(missing_columns) > 0L) {
  stop(
    "Ontology is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

if (anyDuplicated(ontology$hierarchy_path) > 0L) {
  duplicates <- ontology |>
    dplyr::count(
      hierarchy_path,
      name = "rows"
    ) |>
    dplyr::filter(rows > 1L)

  print(duplicates)

  stop(
    "Duplicate ontology paths detected."
  )
}

# -----------------------------------------------------------------------------
# Generate ontology artefacts
# -----------------------------------------------------------------------------

ontology_summary <- ontology |>
  dplyr::count(
    level_1,
    level_2,
    name = "terminal_nodes"
  ) |>
  dplyr::arrange(
    level_1,
    level_2
  )

ontology_json <- ontology |>
  dplyr::select(
    path_id,
    hierarchy_path,
    definition,
    include_when,
    exclude_when,
    examples
  )

prompt_lines <- ontology |>
  dplyr::transmute(
    prompt_entry = paste0(
      path_id,
      " | ",
      hierarchy_path,
      "\nDefinition: ",
      definition,
      "\nInclude when: ",
      include_when,
      "\nExclude when: ",
      exclude_when,
      "\nExamples: ",
      examples
    )
  ) |>
  dplyr::pull(prompt_entry)

ontology_prompt <- paste(
  c(
    "FIXED TOPIC ONTOLOGY V2",
    "",
    paste(
      prompt_lines,
      collapse = "\n\n"
    )
  ),
  collapse = "\n"
)

# Template only. Terms will be reviewed and expanded after topic validation.
term_dictionary_template <- ontology |>
  dplyr::transmute(
    path_id,
    hierarchy_path,
    preferred_term = level_3,
    variant = NA_character_,
    match_case = FALSE,
    match_type = "whole word or phrase",
    notes = NA_character_
  )

readr::write_csv(
  ontology,
  fs::path(
    output_dir,
    "topic_ontology_v2.csv"
  ),
  na = ""
)

readr::write_csv(
  ontology_summary,
  fs::path(
    output_dir,
    "topic_ontology_v2_summary.csv"
  ),
  na = ""
)

jsonlite::write_json(
  ontology_json,
  fs::path(
    output_dir,
    "topic_ontology_v2.json"
  ),
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

readr::write_lines(
  ontology_prompt,
  fs::path(
    output_dir,
    "topic_ontology_v2_prompt.txt"
  )
)

readr::write_csv(
  term_dictionary_template,
  fs::path(
    output_dir,
    "topic_term_dictionary_template.csv"
  ),
  na = ""
)

# -----------------------------------------------------------------------------
# Read corpus
# -----------------------------------------------------------------------------

records <- read_corpus(corpus_file) |>
  dplyr::mutate(
    record_id = as.character(record_id),
    title = dplyr::coalesce(
      as.character(title),
      ""
    ),
    abstract = dplyr::coalesce(
      as.character(abstract),
      ""
    ),
    text_for_sampling = stringr::str_to_lower(
      paste(
        title,
        abstract
      )
    )
  )

if (!"record_sequence" %in% names(records)) {
  records <- records |>
    dplyr::mutate(
      record_sequence = dplyr::row_number()
    )
}

# -----------------------------------------------------------------------------
# Human-validation sample
#
# Ten non-overlapping strata, 10 records each:
#   - Feed and nutrition
#   - Sea lice
#   - Other disease and health
#   - Environment
#   - Product and consumers
#   - Industry and economics
#   - People and society
#   - Governance and history
#   - Likely methods papers
#   - General random records
#
# Strata are used only to obtain broad coverage. They are not gold-standard
# classifications.
# -----------------------------------------------------------------------------

strata <- tibble::tribble(
  ~stratum_order, ~sampling_stratum, ~pattern,
  1L, "Feed and nutrition",
  "\\b(feed|diet|nutrition|nutrient|fishmeal|fish oil|soy|insect meal|algal oil|digestib|feed conversion)\\b",
  2L, "Sea lice",
  "\\b(sea lice|sea louse|lepeophtheirus|caligus|delous|cleaner fish|lumpfish|lump fish|wrasse)\\b",
  3L, "Other disease and health",
  "\\b(disease|virus|viral|bacter|pathogen|infection|vaccine|immun|parasite|saprolegnia|mortality|welfare|stress)\\b",
  4L, "Environment",
  "\\b(environment|benthic|sediment|hydrodynamic|water quality|escape|biodiversity|ecosystem|emission|effluent|climate|carbon footprint|life cycle assessment|lca)\\b",
  5L, "Product and consumers",
  "\\b(fillet|product quality|sensory|shelf life|smoked salmon|food safety|consumer|willingness to pay|purchase|preference|omega-3|fatty acid composition)\\b",
  6L, "Industry and economics",
  "\\b(economic|economics|profit|cost|market|trade|export|import|value chain|supply chain|company|industry|investment|productivity|efficiency)\\b",
  7L, "People and society",
  "\\b(labour|labor|worker|employment|livelihood|community|indigenous|sami|human rights|social licence|social license|food security|justice|migration|migrant)\\b",
  8L, "Governance and history",
  "\\b(history|historical|governance|policy|regulation|licen[cs]|certification|standard|planning|regional development)\\b",
  9L, "Likely methods papers",
  "\\b(method development|method validation|validation of (a |the )?method|novel method|new method|compare methods|comparison of methods|assay validation|sampling method|monitoring method|classification system|ontology|benchmark dataset)\\b"
)

sample_n <- 10L
set.seed(20260804)

safe_sample <- function(data, n) {
  sample_size <- min(
    n,
    nrow(data)
  )

  if (sample_size == 0L) {
    return(data)
  }

  dplyr::slice_sample(
    data,
    n = sample_size
  )
}

sampled_ids <- character()
sample_parts <- vector(
  "list",
  nrow(strata)
)

for (i in seq_len(nrow(strata))) {
  candidates <- records |>
    dplyr::filter(
      !record_id %in% sampled_ids,
      stringr::str_detect(
        text_for_sampling,
        stringr::regex(
          strata$pattern[[i]],
          ignore_case = TRUE
        )
      )
    )

  selected <- safe_sample(
    candidates,
    sample_n
  ) |>
    dplyr::mutate(
      stratum_order = strata$stratum_order[[i]],
      sampling_stratum = strata$sampling_stratum[[i]]
    )

  sampled_ids <- c(
    sampled_ids,
    selected$record_id
  )

  sample_parts[[i]] <- selected
}

# Fill each short stratum from as-yet unused records, retaining its label.
sample_parts <- purrr::map2(
  sample_parts,
  seq_along(sample_parts),
  function(part, i) {
    missing_n <- sample_n - nrow(part)

    if (missing_n <= 0L) {
      return(part)
    }

    filler <- records |>
      dplyr::filter(
        !record_id %in% sampled_ids
      ) |>
      safe_sample(missing_n) |>
      dplyr::mutate(
        stratum_order = strata$stratum_order[[i]],
        sampling_stratum = paste0(
          strata$sampling_stratum[[i]],
          " - random fill"
        )
      )

    sampled_ids <<- c(
      sampled_ids,
      filler$record_id
    )

    dplyr::bind_rows(
      part,
      filler
    )
  }
)

# Final general-random stratum.
general_random <- records |>
  dplyr::filter(
    !record_id %in% sampled_ids
  ) |>
  safe_sample(sample_n) |>
  dplyr::mutate(
    stratum_order = 10L,
    sampling_stratum = "General random records"
  )

validation_sample <- dplyr::bind_rows(
  sample_parts,
  general_random
) |>
  dplyr::arrange(
    stratum_order,
    record_sequence
  ) |>
  dplyr::select(
    sampling_stratum,
    record_sequence,
    record_id,
    title,
    abstract
  ) |>
  dplyr::mutate(
    code_1 = NA_character_,
    code_2 = NA_character_,
    code_3 = NA_character_,
    code_4 = NA_character_,
    no_suitable_code = NA_character_,
    ontology_issue = NA_character_,
    coding_notes = NA_character_
  )

if (nrow(validation_sample) != 100L) {
  warning(
    "Validation sample contains ",
    nrow(validation_sample),
    " records rather than 100."
  )
}

# -----------------------------------------------------------------------------
# Workbook
# -----------------------------------------------------------------------------

instructions <- tibble::tribble(
  ~field, ~instruction,
  "code_1 to code_4",
  "Enter the exact ontology hierarchy path for each substantive concept. Leave unused code columns blank.",
  "no_suitable_code",
  "Enter Yes only if a substantive research question has no suitable ontology path; otherwise enter No.",
  "ontology_issue",
  "Record overlap, unclear boundaries, missing terms or an ontology revision needed.",
  "coding_notes",
  "Briefly explain difficult decisions. Do not code incidental or background-only concepts.",
  "Methods rule",
  "Use Methods only if development, validation, comparison or review of the method is the principal contribution."
)

path_list <- ontology |>
  dplyr::arrange(
    level_1,
    level_2,
    level_3
  ) |>
  dplyr::pull(hierarchy_path)

wb <- openxlsx2::wb_workbook()

wb$add_worksheet(
  "Validation 100"
)
wb$add_data(
  "Validation 100",
  validation_sample
)
wb$freeze_pane(
  sheet = "Validation 100",
  first_row = TRUE,
  first_col = TRUE
)

wb$add_worksheet(
  "Ontology"
)
wb$add_data(
  "Ontology",
  ontology |>
    dplyr::select(
      path_id,
      level_1,
      level_2,
      level_3,
      hierarchy_path,
      definition,
      include_when,
      exclude_when,
      examples
    )
)
wb$freeze_pane(
  sheet = "Ontology",
  first_row = TRUE
)

wb$add_worksheet(
  "Instructions"
)
wb$add_data(
  "Instructions",
  instructions
)

wb$add_worksheet(
  "Lists"
)
wb$add_data(
  "Lists",
  tibble::tibble(
    hierarchy_path = path_list,
    yes_no = c(
      "No",
      "Yes",
      rep(
        NA_character_,
        max(
          0L,
          length(path_list) - 2L
        )
      )
    )
  )
)

validation_rows <- nrow(validation_sample) + 1L

for (column_letter in c(
  "F",
  "G",
  "H",
  "I"
)) {
  wb$add_data_validation(
    sheet = "Validation 100",
    dims = paste0(
      column_letter,
      "2:",
      column_letter,
      validation_rows
    ),
    type = "list",
    value = paste0(
      "'Lists'!$A$2:$A$",
      nrow(ontology) + 1L
    ),
    allow_blank = TRUE
  )
}

wb$add_data_validation(
  sheet = "Validation 100",
  dims = paste0(
    "J2:J",
    validation_rows
  ),
  type = "list",
  value = "'Lists'!$B$2:$B$3",
  allow_blank = TRUE
)

# Practical widths. The abstract column remains wrapped.
wb$set_col_widths(
  sheet = "Validation 100",
  cols = 1,
  widths = 24
)
wb$set_col_widths(
  sheet = "Validation 100",
  cols = 2:3,
  widths = c(
    14,
    14
  )
)
wb$set_col_widths(
  sheet = "Validation 100",
  cols = 4,
  widths = 45
)
wb$set_col_widths(
  sheet = "Validation 100",
  cols = 5,
  widths = 90
)
wb$set_col_widths(
  sheet = "Validation 100",
  cols = 6:9,
  widths = 52
)
wb$set_col_widths(
  sheet = "Validation 100",
  cols = 10,
  widths = 16
)
wb$set_col_widths(
  sheet = "Validation 100",
  cols = 11:12,
  widths = c(
    40,
    55
  )
)

wb$add_cell_style(
  sheet = "Validation 100",
  dims = paste0(
    "A1:L",
    validation_rows
  ),
  wrap_text = TRUE,
  vertical_alignment = "top"
)

wb$add_cell_style(
  sheet = "Validation 100",
  dims = "A1:L1",
  text_decoration = "bold",
  horizontal_alignment = "center",
  vertical_alignment = "center",
  fg_fill = "#D9EAF7"
)

wb$add_cell_style(
  sheet = "Ontology",
  dims = "A1:I1",
  text_decoration = "bold",
  horizontal_alignment = "center",
  fg_fill = "#D9EAF7"
)

wb$add_cell_style(
  sheet = "Ontology",
  dims = paste0(
    "A1:I",
    nrow(ontology) + 1L
  ),
  wrap_text = TRUE,
  vertical_alignment = "top"
)

workbook_path <- fs::path(
  validation_dir,
  "topic_ontology_v2_human_validation_100.xlsx"
)

sample_csv_path <- fs::path(
  validation_dir,
  "topic_ontology_v2_human_validation_100.csv"
)

wb$save(
  workbook_path,
  overwrite = TRUE
)

readr::write_csv(
  validation_sample,
  sample_csv_path,
  na = ""
)

# -----------------------------------------------------------------------------
# Console summary
# -----------------------------------------------------------------------------

message("")
message("Ontology v2 preparation completed.")
message("Ontology terminal nodes: ", nrow(ontology))
message("Human validation records: ", nrow(validation_sample))
message("")
message("Artefacts written to:")
message(output_dir)
message("")
message("Validation workbook:")
message(workbook_path)
message("")
message("No API calls were made.")
