# =============================================================================
# File: R/relevance_screening.R
# Purpose: Utilities for training and applying a binary salmon-farming
#          relevance classifier and for conservative multi-field deduplication.
# =============================================================================

ensure_relevance_packages <- function() {
  packages <- c(
    "dplyr", "fs", "glmnet", "here", "Matrix", "openxlsx2",
    "purrr", "quanteda", "readr", "stringdist", "stringi",
    "stringr", "tibble", "tidyr"
  )

  installed <- rownames(installed.packages())
  missing <- setdiff(packages, installed)

  if (length(missing) > 0L) {
    rspm <- Sys.getenv("RSPM", "")
    repos <- if (nzchar(rspm)) rspm else "https://cloud.r-project.org"

    install.packages(
      missing,
      repos = repos,
      Ncpus = max(1L, min(4L, parallel::detectCores(logical = TRUE)))
    )
  }

  invisible(
    lapply(
      packages,
      library,
      character.only = TRUE
    )
  )
}

normalise_screening_title <- function(x) {
  x |>
    dplyr::coalesce("") |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("&[a-z]+;", " ") |>
    stringr::str_replace_all("[^a-z0-9]+", " ") |>
    stringr::str_squish()
}

normalise_screening_doi <- function(x) {
  x |>
    dplyr::coalesce("") |>
    stringr::str_to_lower() |>
    stringr::str_remove("^https?://(dx\\.)?doi\\.org/") |>
    stringr::str_remove("^doi:\\s*") |>
    stringr::str_trim() |>
    dplyr::na_if("")
}

first_author_key <- function(x) {
  split_authors <- stringr::str_split_fixed(
    dplyr::coalesce(x, ""),
    "\\s*\\|\\s*|\\s*;\\s*",
    2
  )

  split_authors[, 1] |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", " ") |>
    stringr::str_squish()
}

make_screening_text <- function(title, abstract) {
  title <- dplyr::coalesce(title, "")
  abstract <- dplyr::coalesce(abstract, "")

  paste0(
    "TITLE_TITLE ",
    title,
    " TITLE_TITLE ",
    title,
    " ABSTRACT ",
    abstract
  ) |>
    stringr::str_squish()
}

add_screening_keys <- function(records) {
  records |>
    dplyr::mutate(
      record_id = as.character(record_id),
      title = dplyr::coalesce(as.character(title), ""),
      abstract = dplyr::coalesce(as.character(abstract), ""),
      title_key = normalise_screening_title(title),
      doi_key = normalise_screening_doi(doi),
      first_author_key = first_author_key(authors),
      screening_text = make_screening_text(title, abstract),
      has_abstract = nzchar(abstract),
      title_prefix = stringr::str_sub(title_key, 1, 24),
      title_token_key = purrr::map_chr(
        stringr::str_split(title_key, "\\s+"),
        function(tokens) {
          tokens <- tokens[nzchar(tokens)]
          paste(sort(unique(head(tokens, 8))), collapse = " ")
        }
      )
    )
}

read_ris_files <- function(files) {
  if (length(files) == 0L) {
    stop("No RIS files were supplied.")
  }

  purrr::map_dfr(
    files,
    function(file) {
      read_corpus(file) |>
        dplyr::mutate(
          source_file = basename(file),
          source_path = normalizePath(
            file,
            winslash = "/",
            mustWork = FALSE
          )
    }
  )
}

build_training_records <- function(include_file, exclude_file) {
  includes <- read_corpus(include_file) |>
    dplyr::mutate(
      eligibility = 1L,
      screening_source = "historical_include"
    )

  excludes <- read_corpus(exclude_file) |>
    dplyr::mutate(
      eligibility = 0L,
      screening_source = "historical_exclude"
    )

  dplyr::bind_rows(
    includes,
    excludes
  ) |>
    add_screening_keys() |>
    dplyr::filter(
      nzchar(title_key)
    )
}
