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

#' Plot Original and Log-Transformed Distributions Side by Side
#'
#' This function plots the original and log-transformed distributions of numeric variables in a data frame side by side.
#'
#' @param df A data frame containing the variables to analyze.
#' @param variables Optional. A character vector of variable names to analyze. If NULL (default), all numeric variables are analyzed.
#'
#' @return None. This function produces plots as a side effect.
#'
#' @examples
#' \dontrun{
#' # Analyze all numeric variables
#' plotlog(mtcars)
#'
#' # Analyze specific variables
#' plotlog(mtcars, c("mpg", "disp"))
#' }
#'
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @export
plotlog <- function(df, variables = NULL) {
  # Check if required packages are installed
  if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Packages 'ggplot2' and 'gridExtra' are required. Please install them.")
  }

  # Identify numeric columns
  if (is.null(variables)) {
    num_cols <- names(df)[sapply(df, is.numeric)]
  } else {
    num_cols <- intersect(variables, names(df)[sapply(df, is.numeric)])
    if (length(num_cols) == 0) {
      stop("None of the specified variables are numeric.")
    }
  }

  # Function to create a single plot
  create_plot <- function(data, title, fill_color, x_label) {
    data <- data[is.finite(data)]
    if (length(data) > 0) {
      bin_w <- diff(range(data)) / 30
      ggplot2::ggplot(data.frame(x = data), ggplot2::aes(x = x)) +
        ggplot2::geom_histogram(binwidth = bin_w, fill = fill_color, alpha = 0.7) +
        ggplot2::ggtitle(title) +
        ggplot2::xlab(x_label) +
        ggplot2::theme_minimal()
    } else {
      ggplot2::ggplot() + ggplot2::ggtitle("Insufficient data") + ggplot2::theme_minimal()
    }
  }

  # Create and display plots for each variable
  for (col_name in num_cols) {
    original_data <- df[[col_name]]
    log_data <- log(df[[col_name]][df[[col_name]] > 1])

    p_original <- create_plot(original_data,
                              paste("Original distribution of", col_name),
                              "green",
                              col_name)
    p_log <- create_plot(log_data,
                         paste("Log-transformed distribution of", col_name),
                         "blue",
                         paste("log(", col_name, ")"))

    # Arrange and print plots side by side
    gridExtra::grid.arrange(p_original, p_log, ncol = 2)
  }
}

