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
    log_data <- log1p(df[[col_name]])

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

#' Apply Power Law Scaling Transformations to Numeric Variables
#'
#' This function applies various scaling transformations to numeric variables
#' in a data frame, including log-scaling, Mclust classification, and
#' quartile-based categorization.
#'
#' @param df A data frame containing the variables to transform.
#' @param variables Optional. A character vector of variable names to transform.
#'   If NULL (default), all numeric variables in the data frame are transformed.
#'
#' @return A new data frame with the original variables and their transformed versions.
#'   For each transformed variable, four new columns are added:
#'   \itemize{
#'     \item \code{[varname]_scalecat}: Categorized log-scaled values
#'     \item \code{[varname]_log1P}: Log1p transformed values
#'     \item \code{[varname]_mclust}: Mclust classification
#'     \item \code{[varname]_IQ}: Quartile-based categorization
#'   }
#'
#' @details
#' The function performs the following transformations:
#' \itemize{
#'   \item Log-scaling and categorization into 5 levels
#'   \item Log1p transformation
#'   \item Mclust classification
#'   \item Quartile-based categorization with outlier detection
#' }
#'
#' @examples
#' # Create a sample data frame
#' df <- data.frame(
#'   var1 = rnorm(100, 50, 10),
#'   var2 = rexp(100, 1/50)
#' )
#'
#' # Apply transformations to all numeric variables
#' new_df <- powerscaled(df)
#'
#' # Apply transformations to specific variables
#' new_df <- powerscaled(df, c("var1"))
#'
#' @importFrom stats quantile
#' @importFrom mclust Mclust
#' @export
powerscaled <- function(df, variables = NULL) {
  # Check if specific variables are passed
  if (is.null(variables)) {
    # If no variable is specified, select all numeric variables
    variables <- names(df)[sapply(df, is.numeric)]
  } else {
    # Check if all specified variables exist and are numeric
    if (!all(variables %in% names(df))) {
      stop("Some specified variables do not exist in the dataframe.")
    }
    if (!all(sapply(df[variables], is.numeric))) {
      stop("Some specified variables are not numeric.")
    }
  }

  # Create a new DataFrame to store the results
  new_df <- df

  # Loop through the selected variables
  for (col_name in variables) {
    # Apply powerscaled
    new_col_name_scalecat <- paste(col_name, "scalecat", sep = "_")
    new_df[[new_col_name_scalecat]] <- as.factor(cut(scale(log1p(df[[col_name]])),
                                                     breaks = c(-Inf, -1, 1, 2, 3, Inf),
                                                     labels = c("0", "1", "2", "3", "4"),
                                                     right = FALSE))

    # Apply rkscale
    new_col_name_log1P <- paste(col_name, "log1P", sep = "_")
    new_df[[new_col_name_log1P]] <- log1p(df[[col_name]])

    # Apply fitscale
    new_col_name_fit <- paste(col_name, "mclust", sep = "_")
    fit <- Mclust(log1p(df[[col_name]]))
    new_df[[new_col_name_fit]] <- as.factor(fit$classification)

    # Add quartile-based categorization
    new_col_name_IQ <- paste(col_name, "IQ", sep = "_")
    Q1 <- median(log1p(df[[col_name]]))
    Q2 <- mean(log1p(df[[col_name]]))
    Q3 <- as.numeric(quantile(log1p(df[[col_name]]), 0.75))
    IQ <- as.numeric(Q3 - Q1)
    new_df[[new_col_name_IQ]] <- tryCatch({
      as.factor(cut(df[[col_name]],
                    breaks = c(-Inf, Q1, Q2, Q3, Q3 + (1.5 * IQ), Inf),
                    labels = c("0", "1", "2", "3", "4"),
                    right = TRUE))
    }, error = function(e) {
      NA
    })

  }

  return(new_df)
}

#' Fetch SEO Rank Data for URLs
#'
#' This function retrieves SEO rank data for a list of URLs using the SEO Rank API
#' and writes the results to a CSV file.
#'
#' @param filename A character string specifying the name of the output CSV file
#'   (without extension). If NULL, the function will stop and prompt for input.
#' @param urls A character vector of URLs to fetch SEO rank data for. If NULL,
#'   the function will stop and prompt for input.
#' @param api_key A character string containing the API key for the SEO Rank API.
#'   If NULL, the function will stop and prompt for input.
#'
#' @return This function does not return a value. It writes the fetched data to a CSV file
#'   and prints messages to the console about the progress.
#'
#' @details
#' The function performs the following steps:
#' 1. Validates input parameters.
#' 2. Initializes an empty data frame to store column names.
#' 3. For each URL, it fetches data from the SEO Rank API.
#' 4. Dynamically creates columns based on the API response.
#' 5. Writes each row of data to the specified CSV file.
#' 6. Adds a small delay between API calls to avoid overwhelming the server.
#'
#' If the API call fails for a URL, a warning message is printed, and the function
#' continues with the next URL.
#'
#' @note
#' This function requires an active internet connection and a valid API key from
#' https://seo-rank.my-addr.com/.
#'
#' @examples
#' \dontrun{
#' # Fetch data for two URLs
#' mwir_seorank("my_seo_data", c("example.com", "example.org"), "YOUR_API_KEY")
#'
#' # Fetch data for a single URL
#' mwir_seorank("single_url_data", "example.net", "YOUR_API_KEY")
#' }
#'
#' @importFrom httr GET status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom readr write_csv
#'
#' @export
mwir_seorank <- function(filename=NULL, urls=NULL, api_key=NULL) {

  # Input validation for filename
  while (is.null(filename) || !is.character(filename) || nchar(filename) == 0) {
    stop("filename needed for export. Please enter a filename ex. 'myproject' without extention")
  }

  # Input validation for urls
  while (is.null(urls) || !is.character(urls) || length(urls) == 0) {
    stop("no urls given")
  }

  # Input validation for api_key
  while (is.null(api_key) || !is.character(api_key) || nchar(api_key) == 0) {
    stop("need an api key for 'https://seo-rank.my-addr.com/' API services")
  }

  # Initialize an empty data frame to store column names
  result_template <- data.frame(url = character())

  for (url in urls) {
    api_url <- paste0("https://seo-rank.my-addr.com/api2/moz+sr+fb/", api_key, "/", url)

    tryCatch({
      response <- GET(api_url)
      if (status_code(response) == 200) {
        data <- fromJSON(content(response, "text"))

        # Create a new row with the current URL
        new_row <- data.frame(url = url)

        # Dynamically add columns based on API response
        for (col_name in names(data)) {
          new_row[[col_name]] <- as.character(data[[col_name]])
          if (!(col_name %in% names(result_template))) {
            result_template[[col_name]] <- character()
          }
        }

        # Ensure all columns are present in the new row
        for (col_name in names(result_template)) {
          if (!(col_name %in% names(new_row))) {
            new_row[[col_name]] <- NA
          }
        }

        # Write to CSV (append if file exists, create if it doesn't)
        write_csv(new_row, paste0(filename, ".csv"), append = file.exists(paste0(filename, ".csv")))

        message(paste("Data fetched and written for URL:", url))
      } else {
        warning(paste("Failed to fetch data for URL:", url, "Status code:", status_code(response)))
      }
    }, error = function(e) {
      warning(paste("Error fetching data for URL:", url, "Error:", e$message))
    })

    # Add a small delay to avoid overwhelming the API
    Sys.sleep(1)
  }

  message("Data fetching and CSV writing completed.")
}

#' Update Database Table with Externally Modified Data
#'
#' @description
#' This function allows reinsertion of externally modified data into the project's database.
#' It's particularly useful in collaborative or open science workflows where data might be
#' modified outside the main project environment, such as by collaborators or in open science initiatives.
#'
#' @param dataplus A data frame containing the modified data to be inserted into the database.
#' @param table A character string specifying the name of the table to be updated in the database.
#' @param champ A character string specifying the name of the field (column) to be updated.
#' @param by A character string specifying the name of the key field used to match rows for updating.
#' @param labase A character string specifying the name of the SQLite database file. Default is "mwi.db".
#'
#' @return This function does not return a value. It updates the specified table in the database
#' and prints a success message upon completion.
#'
#' @details
#' The function performs the following steps:
#' 1. Establishes a connection to the specified SQLite database.
#' 2. Verifies the existence of the specified table and fields.
#' 3. Prepares an SQL UPDATE statement.
#' 4. Executes the update in a transaction for improved performance and data integrity.
#' 5. Commits the changes if successful, or rolls back if an error occurs.
#'
#' This function is designed to facilitate workflows where data might be exported, modified
#' externally (e.g., by collaborators or in spreadsheet software), and then reintegrated
#' into the main project database. It ensures that externally processed data can be
#' seamlessly incorporated back into the project's central data store.
#'
#' @note
#' - Ensure that the structure of `dataplus` matches the database table, particularly
#'   the columns specified by `champ` and `by`.
#' - The function uses transactions, so either all updates are applied, or none are
#'   (in case of an error), maintaining database consistency.
#' - It's recommended to backup your database before performing large-scale updates.
#'
#' @examples
#' \dontrun{
#' # Assuming 'modified_data' is a data frame with updated information
#' AnnotatedData(dataplus = modified_data,
#'               table = "expression",
#'               champ = "description",
#'               by = "id",
#'               labase = "mwi.db")
#' }
#'
#' @importFrom RSQLite dbConnect dbDisconnect dbExecute dbExistsTable dbGetQuery dbBegin dbCommit dbRollback SQLite
#'
#' @export
annotatedData <- function(dataplus, table, champ, by, labase = "mwi.db") {
  # Establish database connection
  con <- dbConnect(RSQLite::SQLite(), dbname = labase)
  on.exit(dbDisconnect(con), add = TRUE)  # Ensure connection is closed even if an error occurs

  tryCatch({
    # Check if table exists
    if (!dbExistsTable(con, table)) {
      stop(paste("Error: The specified table", table, "does not exist in the database. Available tables:",
                 paste(dbListTables(con), collapse = ", ")))
    }

    # Get table structure
    table_info <- dbGetQuery(con, sprintf("PRAGMA table_info(%s)", table))
    fields <- table_info$name

    # Check if fields exist
    if (!(champ %in% fields)) {
      stop(paste("Error: The specified field", champ, "does not exist in the table", table,
                 ". Available fields:", paste(fields, collapse = ", ")))
    }
    if (!(by %in% fields)) {
      stop(paste("Error: The specified key", by, "does not exist in the table", table,
                 ". Available fields:", paste(fields, collapse = ", ")))
    }

    # Prepare the update statement
    query <- sprintf("UPDATE %s SET %s = ? WHERE %s = ?", table, champ, by)

    # Start a transaction for better performance
    dbBegin(con)

    # Update the table
    update_data <- dataplus[, c(champ, by)]
    dbExecute(con, query, params = as.list(update_data))

    # Commit the transaction
    dbCommit(con)

    message(paste("Table", table, "updated successfully!"))
  }, error = function(e) {
    dbRollback(con)
    stop(paste("An error occurred:", e$message))
  })
}

#' GPT_Recode
#'
#' A function to recode a cell value using the GPT-3.5-turbo model by providing a prompt.
#'
#' @param prompt A character string. The prompt to provide to the model.
#' @param cell A character string. The cell value to be recoded.
#' @param sysprompt A character string. The system prompt to guide the model's behavior. Default is "You are a helpful assistant.".
#' @param model A character string. The model to use for the completion. Default is "gpt-3.5-turbo". Most famous are "gpt-4o"
#' @param temperature A numeric value. The temperature setting for the model's response. Default is 0.2.
#'
#' @return A character string. The recoded cell value based on the provided prompt.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Load the necessary library
#'   library(openai)
#'
#'   # Set the API key for OpenAI
#'   Sys.setenv(OPENAI_API_KEY = "sk-proj-XXXXXXXXXXXX")
#'
#'   # Use the GPT_Recode function
#'   result <- GPT_Recode(prompt = "Translate to French", cell = "Hello, how are you?")
#'   print(result)
#' }
GPT_Recode <- function(prompt, cell, sysprompt= "You are a helpful assistant.", model = "gpt-3.5-turbo", temperature = 0.2) {
  response <- create_chat_completion(
    model = model,
    messages = list(
      list(role = "system", content = sysprompt),
      list(role = "user", content = paste0(prompt, ":\n\n", cell))
    ),
    temperature = temperature
  )

  return(response$choices$message.content[1])
}
