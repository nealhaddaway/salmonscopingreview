# Stage 1: parse and audit the screened corpus --------------------------------

source("scripts/00_setup.R")
source("R/read_corpus.R")

options(stringsAsFactors = FALSE)
set.seed(20260730)

input_records <- here::here("data_raw", "INCLUDES fixed abstracts.txt")
input_dictionary <- here::here("data_raw", "Salmon scoping review keywords - dictionary final.csv")
out_dir <- here::here("outputs", "stage_1_audit")
fs::dir_create(out_dir)

stopifnot(file.exists(input_records), file.exists(input_dictionary))

records <- read_corpus(input_records)

authors_long <- make_authors_long(records)

# Dictionary audit.
dictionary <- readr::read_csv(input_dictionary, show_col_types = FALSE, na = c("", "NA")) |>
  janitor::clean_names() |>
  dplyr::mutate(
    dplyr::across(dplyr::everything(), stringr::str_squish),
    dictionary_row = dplyr::row_number(),
    broad_topic_id = as.integer(factor(broad_topic, levels = unique(broad_topic))),
    hierarchy_path = paste(broad_topic, subtopic, feature, component, sep = " > "),
    repeated_general = component == "General" | feature == "General" | subtopic == "General",
    exact_duplicate_path = duplicated(hierarchy_path) | duplicated(hierarchy_path, fromLast = TRUE)
  )

# Summary metrics.
summary_tbl <- tibble::tibble(
  metric = c(
    "Parsed records", "Records with missing TY field", "Records with missing record ID",
    "Records with title", "Records with abstract", "Records without abstract",
    "Records with valid DOI", "Unique record IDs", "Unique normalised titles",
    "Dictionary rows", "Dictionary broad topics", "Exact duplicate dictionary paths"
  ),
  value = c(
    nrow(records), sum(records$parser_missing_type), sum(records$parser_missing_id),
    sum(records$has_title), sum(records$has_abstract), sum(!records$has_abstract),
    sum(records$has_valid_doi), dplyr::n_distinct(records$record_id, na.rm = TRUE),
    dplyr::n_distinct(records$title_normalised[records$title_normalised != ""], na.rm = TRUE),
    nrow(dictionary), dplyr::n_distinct(dictionary$broad_topic),
    sum(dictionary$exact_duplicate_path)
  )
)

missingness_tbl <- tibble::tibble(
  field = c("record_id", "title", "abstract", "doi", "year", "journal", "authors", "type"),
  missing_n = c(
    sum(is.na(records$record_id)), sum(is.na(records$title)), sum(is.na(records$abstract)),
    sum(is.na(records$doi)), sum(is.na(records$year)), sum(is.na(records$journal)),
    sum(is.na(records$authors)), sum(is.na(records$type))
  )
) |>
  dplyr::mutate(missing_percent = 100 * missing_n / nrow(records))

duplicate_ids <- records |>
  dplyr::filter(!is.na(record_id)) |>
  dplyr::add_count(record_id, name = "duplicate_n") |>
  dplyr::filter(duplicate_n > 1) |>
  dplyr::arrange(dplyr::desc(duplicate_n), record_id)

duplicate_dois <- records |>
  dplyr::filter(!is.na(doi)) |>
  dplyr::add_count(doi, name = "duplicate_n") |>
  dplyr::filter(duplicate_n > 1) |>
  dplyr::arrange(dplyr::desc(duplicate_n), doi)

duplicate_titles <- records |>
  dplyr::filter(!is.na(title_normalised), title_normalised != "") |>
  dplyr::add_count(title_normalised, name = "duplicate_n") |>
  dplyr::filter(duplicate_n > 1) |>
  dplyr::arrange(dplyr::desc(duplicate_n), title_normalised)

anomalies <- records |>
  dplyr::filter(
    parser_missing_type | parser_missing_id | !has_title | !has_abstract |
      (!is.na(year) & (year < 1900 | year > as.integer(format(Sys.Date(), "%Y")) + 1L))
  ) |>
  dplyr::select(record_sequence, record_id, type, year, title, has_abstract,
                parser_missing_type, parser_missing_id)

# Save flat files.
readr::write_csv(records, fs::path(out_dir, "clean_records.csv"), na = "")
readr::write_csv(authors_long, fs::path(out_dir, "authors_long.csv"), na = "")
readr::write_csv(dictionary, fs::path(out_dir, "dictionary_clean.csv"), na = "")

# Visual audits.
p_year <- records |>
  dplyr::filter(!is.na(year), year >= 1900, year <= as.integer(format(Sys.Date(), "%Y")) + 1L) |>
  dplyr::count(year) |>
  ggplot2::ggplot(ggplot2::aes(year, n)) +
  ggplot2::geom_col() +
  ggplot2::labs(title = "Records by publication year", x = "Year", y = "Records") +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(fs::path(out_dir, "records_by_year.png"), p_year, width = 10, height = 6, dpi = 300)

p_length <- records |>
  dplyr::filter(has_abstract) |>
  ggplot2::ggplot(ggplot2::aes(abstract_word_count)) +
  ggplot2::geom_histogram(bins = 60) +
  ggplot2::coord_cartesian(xlim = c(0, stats::quantile(records$abstract_word_count, 0.99, na.rm = TRUE))) +
  ggplot2::labs(title = "Abstract length distribution", x = "Words", y = "Records") +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(fs::path(out_dir, "abstract_length_distribution.png"), p_length, width = 10, height = 6, dpi = 300)

# Excel audit workbook.
wb <- openxlsx2::wb_workbook()
wb$add_worksheet("Summary")$add_data("Summary", summary_tbl)
wb$add_worksheet("Missingness")$add_data("Missingness", missingness_tbl)
wb$add_worksheet("Parser anomalies")$add_data("Parser anomalies", anomalies)
wb$add_worksheet("Duplicate IDs")$add_data("Duplicate IDs", duplicate_ids)
wb$add_worksheet("Duplicate DOIs")$add_data("Duplicate DOIs", duplicate_dois)
wb$add_worksheet("Duplicate titles")$add_data("Duplicate titles", duplicate_titles)
wb$add_worksheet("Dictionary")$add_data("Dictionary", dictionary)
wb$add_worksheet("Dictionary summary")$add_data(
  "Dictionary summary",
  dictionary |>
    dplyr::count(broad_topic, subtopic, name = "dictionary_rows") |>
    dplyr::arrange(broad_topic, subtopic)
)

for (sheet in wb$get_sheet_names()) {
  wb$freeze_pane(sheet, first_row = TRUE)
  #wb$add_filter(sheet, dims = wb$get_used_range(sheet))
  wb$set_col_widths(sheet, cols = 1:30, widths = "auto")
}

wb$save(fs::path(out_dir, "corpus_audit.xlsx"), overwrite = TRUE)

capture.output(sessionInfo(), file = fs::path(out_dir, "session_info.txt"))

cli::cli_alert_success("Stage 1 audit completed.")
cli::cli_alert_info("Parsed {nrow(records)} records.")
cli::cli_alert_info("Open: {fs::path(out_dir, 'corpus_audit.xlsx')}")
