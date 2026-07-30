# Stage 0: install and load dependencies ---------------------------------------

required_packages <- c(
  "cli", "dplyr", "fs", "ggplot2", "here", "janitor", "openxlsx2",
  "purrr", "readr", "stringi", "stringr", "tibble", "tidyr"
)

installed <- rownames(installed.packages())
missing <- setdiff(required_packages, installed)

if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_packages, library, character.only = TRUE))

cli::cli_alert_success("R dependencies are installed and available.")
cli::cli_alert_info("Python is not used in Stage 1. We will configure reticulate before semantic modelling.")
