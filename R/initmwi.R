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
#' @import DBI cld3 httr lubridate jsonlite pdftools readr reticulate RSQLite SnowballC stringr tesseract tools urltools zip mockery
#' @export
initmwi <- function() {
  required_packages <- c("DBI", "cld3", "httr", "lubridate", "jsonlite", "pdftools", "readr","reticulate", "RSQLite", "rvest","SnowballC",  "stringr","tesseract", "tools", "urltools", "XML", "zip", "mockery", "stringi")

  # Check and install missing packages
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      tryCatch({
        install.packages(pkg, repos = "https://cloud.r-project.org")
      }, error = function(e) {
        stop(paste("Failed to install package:", pkg))
      })
    }
    suppressMessages(suppressWarnings(library(pkg, character.only = TRUE)))
  }


  # Check if the 'trafilatura' module is available
  if (reticulate::py_module_available("trafilatura")) {
    message("Trafilatura is installed and available.")
    trafilatura <- reticulate::import("trafilatura")
  } else {
    warning("Trafilatura is not installed. Run 'pip install trafilatura' in your Python environment.")
  }

  # Prompt the user for the SERP API key
  serp_key <- readline(prompt = "Enter your SERP API key or press Enter: ")

  # Store the API key in the global environment
  assign("serp_key", gsub("\"", "", serp_key), envir = .GlobalEnv)

}
