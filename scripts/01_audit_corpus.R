# Stage 1: parse and audit the screened corpus --------------------------------

source("scripts/00_setup.R")

options(stringsAsFactors = FALSE)
set.seed(20260730)

input_records <- here::here("data_raw", "INCLUDES fixed abstracts.txt")
input_dictionary <- here::here("data_raw", "Salmon scoping review keywords - dictionary final.csv")
out_dir <- here::here("outputs", "stage_1_audit")
fs::dir_create(out_dir)

stopifnot(file.exists(input_records), file.exists(input_dictionary))

# Parse RIS-like records while preserving repeated fields such as authors.
parse_ris_like <- function(path) {
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  lines <- stringi::stri_enc_toutf8(lines, is_unknown_8bit = TRUE)

  end_idx <- which(stringr::str_detect(lines, "^ER\\s{2}-"))
  if (length(end_idx) == 0) stop("No ER record terminators were found.")

  start_idx <- c(1L, head(end_idx, -1L) + 1L)

  parsed <- purrr::map2_dfr(start_idx, end_idx, function(start, end) {
    block <- lines[start:end]
    field_lines <- stringr::str_detect(block, "^[A-Z0-9]{2}\\s{2}-")

    # Continuation lines are appended to the previous RIS field.
    field_no <- cumsum(field_lines)
    valid <- field_no > 0
    block <- block[valid]
    field_no <- field_no[valid]

    collapsed <- tibble::tibble(field_no = field_no, text = block) |>
      dplyr::group_by(field_no) |>
      dplyr::summarise(text = paste(text, collapse = " "), .groups = "drop")

    tags <- stringr::str_sub(collapsed$text, 1, 2)
    values <- stringr::str_replace(collapsed$text, "^[A-Z0-9]{2}\\s{2}-\\s?", "") |>
      stringr::str_squish()

    get_one <- function(tag) {
      x <- values[tags == tag]
      if (length(x) == 0) NA_character_ else paste(x, collapse = " | ")
    }

    authors <- values[tags == "AU"]

    tibble::tibble(
      record_sequence = match(end, end_idx),
      type = get_one("TY"),
      record_id = get_one("ID"),
      title = dplyr::coalesce(get_one("TI"), get_one("ST")),
      short_title = get_one("ST"),
      abstract_raw = get_one("AB"),
      authors = if (length(authors)) paste(authors, collapse = " | ") else NA_character_,
      doi_raw = get_one("DO"),
      year_raw = get_one("PY"),
      journal = get_one("T2"),
      volume = get_one("VL"),
      issue = get_one("IS"),
      pages = get_one("SP"),
      url_raw = get_one("UR"),
      raw_field_count = length(tags)
    )
  })

  parsed
}

clean_na <- function(x) {
  x <- stringr::str_squish(x)
  dplyr::na_if(x, "NA")
}

clean_html <- function(x) {
  x <- dplyr::coalesce(x, "")
  x <- stringr::str_replace_all(x, "<[^>]+>", " ")
  x <- stringr::str_replace_all(x, "&nbsp;", " ")
  x <- stringr::str_replace_all(x, "&amp;", "&")
  x <- stringr::str_replace_all(x, "&lt;", "<")
  x <- stringr::str_replace_all(x, "&gt;", ">")
  x <- stringr::str_squish(x)
  dplyr::na_if(x, "")
}

normalise_doi <- function(x) {
  x <- clean_na(x)
  x <- stringr::str_to_lower(x)
  x <- stringr::str_remove(x, "^https?://(dx\\.)?doi\\.org/")
  x <- stringr::str_remove(x, "^doi:\\s*")
  x <- stringr::str_trim(x)
  x[!stringr::str_detect(x, "^10\\.\\d{4,9}/\\S+$")] <- NA_character_
  x
}

records <- parse_ris_like(input_records) |>
  dplyr::mutate(
    dplyr::across(c(type, record_id, title, short_title, journal, volume, issue, pages), clean_na),
    abstract = clean_html(abstract_raw),
    doi = normalise_doi(doi_raw),
    year = suppressWarnings(as.integer(stringr::str_extract(year_raw, "(?:18|19|20)\\d{2}"))),
    title_normalised = title |>
      stringr::str_to_lower() |>
      stringi::stri_trans_general("Latin-ASCII") |>
      stringr::str_replace_all("[^a-z0-9]+", " ") |>
      stringr::str_squish(),
    abstract_word_count = stringr::str_count(dplyr::coalesce(abstract, ""), "\\S+"),
    title_word_count = stringr::str_count(dplyr::coalesce(title, ""), "\\S+"),
    has_title = !is.na(title),
    has_abstract = !is.na(abstract),
    has_valid_doi = !is.na(doi),
    parser_missing_type = is.na(type),
    parser_missing_id = is.na(record_id)
  )

# Long author table.
authors_long <- records |>
  dplyr::select(record_sequence, record_id, authors) |>
  tidyr::separate_longer_delim(authors, delim = " | ") |>
  dplyr::rename(author = authors) |>
  dplyr::filter(!is.na(author), author != "") |>
  dplyr::group_by(record_sequence) |>
  dplyr::mutate(author_order = dplyr::row_number()) |>
  dplyr::ungroup()

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
