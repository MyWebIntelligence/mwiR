#' Initialize Environment and Install Required Packages
#'
#' @description
#' This function initializes the R environment by installing and loading the required packages.
#' It also checks the installation of the `@postlight/parser` npm package and the `trafilatura` Python module.
#' Additionally, it prompts the user to enter their SERP API key and stores it in the global environment.
#'
#' @return NULL. The function is called for its side effects.
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Checks for the required R packages and installs them if they are missing.
#'   \item Loads the required R packages.
#'   \item Checks if the `@postlight/parser` npm package is installed and provides a warning if it is not.
#'   \item Checks if the `trafilatura` Python module is available and provides a warning if it is not.
#'   \item Prompts the user to enter their SERP API key and stores it in the global environment.
#' }
#'
#' @examples
#' \dontrun{
#' initmwi()
#' }
#' @export
initmwi <- function() {
  required_packages <- c("DBI", "cld3", "ggplot2", "gridExtra","httr", "lubridate", "jsonlite", "mclust","openai","pdftools", "readr","reticulate", "RSQLite", "rvest","SnowballC",  "stringr", "stats","tesseract", "tools", "urltools", "XML", "zip", "mockery", "stringi")
  installed_packages <- rownames(installed.packages())

  # Check and install missing packages ------------------------------------------------------------
  missing_pkgs <- setdiff(required_packages, installed_packages)
  if (length(missing_pkgs)) {
    message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
    tryCatch({
      options(repos = c(CRAN = "https://cloud.r-project.org"))
      install.packages(missing_pkgs, dependencies = TRUE)
    }, error = function(e) {
      stop("Failed to install one or more packages: ", e$message)
    })
  }
  # Load packages quietly
  invisible(
    vapply(required_packages, function(pkg) {
      suppressPackageStartupMessages(require(pkg, character.only = TRUE, quietly = TRUE))
      TRUE
    }, logical(1L))
  )

  # Check if the 'trafilatura' module is available -----------------------------------------------
  if (reticulate::py_module_available("trafilatura")) {
    message("Trafilatura is installed and available.")
    assign(".trafilatura_mod",
           reticulate::import("trafilatura", delay_load = FALSE),
           envir = .GlobalEnv)
  } else {
    warning("Trafilatura is not installed. Run 'pip install trafilatura' in your Python environment.")
  }

  # Retrieve or prompt for SERP API key -----------------------------------------------------------
  serp_key <- Sys.getenv("SERPAPI_KEY", unset = NA_character_)
  if (is.na(serp_key) || serp_key == "") {
    if (interactive()) {
      serp_key <- readline(prompt = "Enter your SERP API key or press Enter: ")
    } else {
      warning("SERPAPI_KEY environment variable not set and session is non‑interactive.")
      serp_key <- ""
    }
  }
  serp_key <- gsub("\"", "", serp_key, fixed = TRUE)
  assign("serp_key", serp_key, envir = .GlobalEnv)

}
