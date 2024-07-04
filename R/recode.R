#' Detect Language of Text in Data Frame Columns
#'
#' @description
#' This function detects the language of text contained in specified columns of a data frame.
#' It combines the text from multiple columns and uses the 'cld3' package to perform
#' language detection on the combined text for each row.
#'
#' @param df A data frame containing the text columns to analyze.
#' @param variables A character vector specifying the names of the columns in `df`
#'   that contain the text to be analyzed.
#'
#' @return A character vector of the same length as the number of rows in `df`,
#'   where each element represents the detected language code for the corresponding row.
#'
#' @details
#' The function performs the following steps:
#' 1. Checks if all specified variables exist in the data frame.
#' 2. Combines the text from the specified columns for each row.
#' 3. Uses the 'cld3' package to detect the language of the combined text.
#'
#' Language codes are returned according to the ISO 639-1 standard (e.g., "en" for English,
#' "fr" for French, etc.).
#'
#' @note
#' This function requires the 'cld3' package to be installed and loaded.
#' If 'cld3' is not available, the function will throw an error.
#'
#' @examples
#' \dontrun{
#' library(cld3)
#'
#' # Create a sample data frame
#' df <- data.frame(
#'   title = c("Hello world", "Bonjour le monde", "Hola mundo"),
#'   description = c("This is a test", "Ceci est un test", "Esto es una prueba")
#' )
#'
#' # Detect language using both title and description columns
#' languages <- mwiR_detectLang(df, c("title", "description"))
#' print(languages)
#'
#' # Detect language using only the title column
#' languages_title <- mwiR_detectLang(df, "title")
#' print(languages_title)
#' }
#'
#' @export
#' @importFrom cld3 detect_language
mwiR_detectLang <- function(df, variables) {
  # Check if cld3 package is available
  if (!requireNamespace("cld3", quietly = TRUE)) {
    stop("Package 'cld3' is required but not installed. Please install it using install.packages('cld3').")
  }

  # Check if all specified variables exist in the data frame
  if (!all(variables %in% names(df))) {
    stop("Some specified variables do not exist in the data frame.")
  }

  # Combine the texts from the specified variables
  combined_text <- apply(df[, variables, drop = FALSE], 1, paste, collapse = " ")

  # Detect the language for all combined texts at once using the cld3 package
  lang <- cld3::detect_language(combined_text)

  return(lang)
}
