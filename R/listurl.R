##' @import httr jsonlite lubridate data.table
##' Retrieve SerpAPI key
##'
##' Internal helper returning the key from the global variable or
##' environment; if missing, prompts the user.
##' @return Character scalar containing the API key.
##' @keywords internal
get_serp_key <- function() {
  key <- get0("serp_key",
              ifnotfound = Sys.getenv("SERPAPI_KEY", unset = NA_character_),
              envir = .GlobalEnv)
  if (is.na(key) || key == "") {
    key <- readline(prompt = "SerpAPI key is not set. Please enter your API key: ")
    if (key == "") stop("Error: SerpAPI key is required.")
    assign("serp_key", gsub("\"", "", key, fixed = TRUE), envir = .GlobalEnv)
  }
  key
}

#' Retrieve Related Search Queries from Google using SerpAPI
#'
#' This function retrieves related search queries from Google using the SerpAPI service.
#' It constructs an API request based on the provided query, language, and country parameters,
#' then processes the response to extract and return the related searches.
#'
#' @param query A character string representing the search query.
#' @param lang A character string representing the language for the search (default is "fr").
#' @param country A character string representing the country for the search (default is "France").
#'
#' @return A data frame containing the related searches, with columns:
#' \describe{
#'   \item{block_position}{Integer indicating the position of the block in the search results.}
#'   \item{query}{Character string of the related query.}
#'   \item{link}{Character string of the link to the related query.}
#'   \item{serpapi_link}{Character string of the SerpAPI link for the related query.}
#' }
#' If no related searches are found, the function returns NULL.
#'
#' @examples
#' \dontrun{
#' related_searches <- related_query("data science", lang = "en", country = "United States")
#' }
#'
#' @export
related_query <- function(query, lang = "fr", country = "France") {
  api_key <- get_serp_key()

  # Convert the query to lowercase and replace spaces with '+'
  query <- tolower(query)
  query <- gsub(" ", "+", query)

  related_searches_df <- data.frame(block_position = integer(), query = character(), link = character(), serpapi_link = character(), stringsAsFactors = FALSE)

  # Basic parameters
  google_domain <- paste("google.", lang, sep = "")
  gl <- lang
  hl <- lang
  lr <- paste("lang_", lang, sep = "")
  safe <- "off"
  num <- 100
  start <- 0
  sort_by <- "date"
  location <- country

  # Construct the API URL
  api_url <- modify_url("https://serpapi.com/search",
                        query = list(api_key = api_key,
                                     engine = "google",
                                     q = query,
                                     location = location,
                                     google_domain = google_domain,
                                     gl = gl,
                                     hl = hl,
                                     lr = lr,
                                     safe = safe,
                                     num = num,
                                     start = start))

  # Perform the API request
  response <- GET(api_url)

  # Check if an error is returned
  if (http_status(response)$category != "Success") {
    stop("Error: Failed to retrieve data from SerpAPI: ", http_status(response)$message)
  }

  # Extract and process JSON data
  json_content <- fromJSON(content(response, "text"))

  # Check if related_searches is empty
  if (length(json_content$related_searches) == 0) {
    return(NULL)
  }

  return(json_content$related_searches)
}

#' Query to URLs with SerpAPI Google Search
#'
#' This function retrieves URLs from Google search results using the SerpAPI service.
#' It constructs API requests based on the provided query, date range, and language parameters,
#' then processes the response to extract and save the URLs in a text file.
#'
#' @param query A character string representing the search query.
#' @param datestart A character string representing the start date for the search in "yyyy-mm-dd" format.
#' @param dateend A character string representing the end date for the search in "yyyy-mm-dd" format.
#' @param timestep A character string representing the time step for the date sequence (default is "week").
#' @param sleep_seconds A numeric value representing the sleep time between API requests to avoid rate-limiting (default is 1).
#' @param lang A character string representing the language for the search (default is "fr").
#'
#' @return This function does not return a value. It writes the search results to a text file.
#'
#' @examples
#' \dontrun{
#' urlist_Google("data science", "2023-01-01", "2023-03-01", timestep = "month", sleep_seconds = 2, lang = "en")
#' }
#'
#' @export
urlist_Google <- function(query, datestart, dateend,
                          timestep = "week", sleep_seconds = 1, lang = "fr") {

  api_key <- get_serp_key()

  # Convert the query to lowercase and replace spaces with '+'
  query <- tolower(query)
  query <- gsub(" ", "+", query)

  # Create a filename based on the query
  filename <- paste0(gsub("[^a-zA-Z0-9\\.]", "_", query), ".txt")

  # Create a dataframe for the search dates
  date_seq <- seq(from = as.Date(datestart), to = as.Date(dateend), by = timestep)
  datesearch <- data.frame(datestart = date_seq, dateend = date_seq + period(1, units = timestep) - days(1))

  # Loop through all dates in the datesearch dataframe
  for (i in 1:nrow(datesearch)) {

    # Convert dates to mm/dd/yyyy format
    datestart_str <- format(datesearch$datestart[i], "%m/%d/%Y")
    dateend_str <- format(datesearch$dateend[i], "%m/%d/%Y")

    # Basic parameters
    google_domain <- paste0("google.", lang)
    gl <- lang
    hl <- lang
    lr <- paste("lang_", lang, sep = "")
    safe <- "off"
    tbs <- paste0("cdr:1,cd_min:", datestart_str, ",cd_max:", dateend_str)
    num <- 100
    start <- 0
    sort_by <- "date"

    # Loop to paginate through results until an error is returned
    while (TRUE) {

      # Construct the API URL
      api_url <- modify_url("https://serpapi.com/search",
                            query = list(api_key = api_key,
                                         engine = "google",
                                         q = query,
                                         google_domain = google_domain,
                                         gl = gl,
                                         hl = hl,
                                         lr = lr,
                                         safe = safe,
                                         tbs = tbs,
                                         num = num,
                                         start = start,
                                         sort_by = sort_by))

      # Perform the API request
      response <- GET(api_url)

      # Check if an error is returned
      if (http_status(response)$category != "Success") {
        break
      }

      # Extract and process JSON data
      json_content <- fromJSON(content(response, "text"))

      # Check if organic_results is empty
      if (length(json_content$organic_results) == 0) {
        break
      }

      # Check if the file already exists
      file_exists <- file.exists(filename)

      # Loop through each item in 'organic_results'
      for (j in 1:nrow(json_content$organic_results)) {
        # Extract data from the current item
        current_entry <- json_content$organic_results[j,]

        # Check and assign values, setting a default value if necessary
        position <- ifelse(is.null(current_entry$position), NA, current_entry$position)
        title <- ifelse(is.null(current_entry$title), NA, current_entry$title)
        link <- ifelse(is.null(current_entry$link), NA, current_entry$link)
        date <- ifelse(is.null(current_entry$date), NA, current_entry$date)

        # Create a data row as a dataframe
        new_row <- data.frame(
          position = position,
          title = title,
          link = link,
          date = date,
          stringsAsFactors = FALSE
        )

        # Write the new row to the file
        # Include column names only if the file does not already exist
        write.table(new_row, file = filename, append = TRUE, sep = ",",
                    col.names = !file_exists, row.names = FALSE)

        # After the first write, ensure headers are not rewritten
        if (!file_exists) {
          file_exists <- TRUE
        }
      }

      # Update the start point for the next page
      start <- start + num

      # Pause to avoid rate-limiting
      Sys.sleep(sleep_seconds * runif(1, min = 0.80, max = 1.20))
    }
  }
}

#' Retrieve URLs with SerpAPI DuckDuckGo Search
#'
#' This function retrieves URLs from DuckDuckGo search results using the SerpAPI service.
#' It constructs API requests based on the provided query and language parameters,
#' then processes the response to extract and save the URLs in a text file.
#'
#' @param query A character string representing the search query.
#' @param filename A character string representing the name of the file where the results will be saved.
#' If NULL, a filename will be generated based on the query (default is NULL).
#' @param sleep_seconds A numeric value representing the sleep time between API requests to avoid rate-limiting (default is 1).
#' @param lang A character string representing the language for the search (default is "fr").
#'
#' @return This function does not return a value. It writes the search results to a text file.
#'
#' @examples
#' \dontrun{
#' urlist_Duck("data science", filename = "data_science_results.txt", sleep_seconds = 2, lang = "en")
#' }
#'
#' @export
urlist_Duck <- function(query, filename = NULL, sleep_seconds = 1, lang = "fr") {

  api_key <- get_serp_key()

  # Create a filename based on the query
  if (is.null(filename)) {
    filename <- paste0(gsub("[^a-zA-Z0-9\\.]", "_", query), ".txt")
  }

  start <- 0

  # Loop to paginate through results until an error is returned
  while (TRUE) {
    querylist = list(api_key = api_key,
                     engine = "duckduckgo",
                     q = query,
                     kl = paste(lang, "-", lang, sep = ""),
                     safe = "-2",
                     start = start,
                     no_cache = "true")

    # Construct the API URL
    api_url <- modify_url("https://serpapi.com/search.json",
                          query = querylist)

    # Perform the API request
    response <- GET(api_url)

    # Check if an error is returned
    if (http_status(response)$category != "Success") {
      break
    }

    # Extract and process JSON data
    json_content <- fromJSON(content(response, "text"))

    # Check if organic_results is empty
    if ("error" %in% names(json_content)) {
      message("Error: ", json_content$error)
      break
    }

    if (length(json_content$organic_results) == 0) {
      break
    }

    # Loop through each item in 'organic_results'
    for (j in 1:nrow(json_content$organic_results)) {
      # Extract data from the current item
      current_entry <- json_content$organic_results[j,]

      # Check and assign values, setting a default value if necessary
      position <- ifelse(is.null(current_entry$position), NA, current_entry$position)
      title <- ifelse(is.null(current_entry$title), NA, current_entry$title)
      link <- ifelse(is.null(current_entry$link), NA, current_entry$link)
      date <- ifelse(is.null(current_entry$date_raw), NA, current_entry$date_raw)

      # Create a data row as a dataframe
      new_row <- data.frame(
        position = position,
        title = title,
        link = link,
        date = date,
        stringsAsFactors = FALSE
      )

      # Add the new row to the results list
      write.table(new_row, file = filename, append = TRUE, sep = ",", col.names = !file.exists(filename), row.names = FALSE)
    }

    # Update the start point for the next page
    start <- start + 50
    # Pause to avoid rate-limiting
    Sys.sleep(sleep_seconds * runif(1, min = 0.80, max = 1.20)) # Pause between each time interval

  }
}

#' Retrieve URLs with SerpAPI Bing Search
#'
#' This function retrieves URLs from Bing search results using the SerpAPI service.
#' It constructs API requests based on the provided query and language parameters,
#' then processes the response to extract and save the URLs in a text file.
#'
#' @param query A character string representing the search query.
#' @param filename A character string representing the name of the file where the results will be saved.
#' If NULL, a filename will be generated based on the query (default is NULL).
#' @param sleep_seconds A numeric value representing the sleep time between API requests to avoid rate-limiting (default is 1).
#' @param lang A character string representing the language for the search (default is "fr").
#'
#' @return This function does not return a value. It writes the search results to a text file.
#'
#' @examples
#' \dontrun{
#' urlist_Bing("data science", filename = "data_science_results.txt", sleep_seconds = 2, lang = "en")
#' }
#'
#' @export
urlist_Bing <- function(query, filename = NULL, sleep_seconds = 1, lang = "fr") {

  api_key <- get_serp_key()

  # Create a filename based on the query
  if (is.null(filename)) {
    filename <- paste0(gsub("[^a-zA-Z0-9\\.]", "_", query), ".txt")
  }

  first <- 0

  # Loop to paginate through results until an error is returned
  while (TRUE) {
    querylist <- list(api_key = api_key,
                      engine = "bing",
                      q = query,
                      hl = lang,
                      gl = lang,
                      location = "Paris,+Ile-de-France,+France",
                      count = "50",
                      safeSearch = "off",
                      first = first,
                      no_cache = "true")

    # Construct the API URL
    api_url <- modify_url("https://serpapi.com/search.json", query = querylist)

    # Perform the API request
    response <- GET(api_url)

    # Check if an error is returned
    if (http_status(response)$category != "Success") {
      break
    }

    # Extract and process JSON data
    json_content <- fromJSON(content(response, "text"))

    # Check if organic_results is empty
    if ("error" %in% names(json_content)) {
      message("Error: ", json_content$error)
      break
    }

    if (length(json_content$organic_results) == 0) {
      break
    }

    # Loop through each item in 'organic_results'
    for (j in 1:nrow(json_content$organic_results)) {
      # Extract data from the current item
      current_entry <- json_content$organic_results[j,]

      # Check and assign values, setting a default value if necessary
      position <- ifelse(is.null(current_entry$position), NA, current_entry$position)
      title <- ifelse(is.null(current_entry$title), NA, current_entry$title)
      link <- ifelse(is.null(current_entry$link), NA, current_entry$link)
      date <- ifelse(is.null(current_entry$date), NA, current_entry$date)

      # Create a data row as a dataframe
      new_row <- data.frame(
        position = position,
        title = title,
        link = link,
        date = date,
        stringsAsFactors = FALSE
      )

      # Add the new row to the results list
      write.table(new_row, file = filename, append = TRUE, sep = ",", col.names = !file.exists(filename), row.names = FALSE)
    }

    # Update the start point for the next page
    first <- first + 50
    # Pause to avoid rate-limiting
    Sys.sleep(sleep_seconds * runif(1, min = 0.80, max = 1.20)) # Pause between each time interval

  }
}
