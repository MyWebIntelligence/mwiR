#' Set up the SQLite database
#'
#' This function establishes a connection to the specified SQLite database
#' and creates the necessary tables if they do not already exist. The tables
#' created are: Land, Domain, Expression, ExpressionLink, Word, LandDictionary,
#' Media, Tag, and TaggedContent.
#'
#' @param db_name A string specifying the name of the SQLite database file. Default is "mwi.db".
#' @return None. This function is called for its side effects.
#' @details
#' The function creates the following tables:
#' - Land: Stores information about research projects.
#' - Domain: Stores domain-related metadata.
#' - Expression: Stores URLs and associated metadata.
#' - ExpressionLink: Stores links between expressions.
#' - Word: Stores terms and their lemmas.
#' - LandDictionary: Stores associations between lands and terms.
#' - Media: Stores media associated with expressions.
#' - Tag: Stores tags for categorizing expressions.
#' - TaggedContent: Stores tagged content within expressions.
#'
#' @examples
#' \dontrun{
#' db_setup("my_database.db")
#' }
#' @export
db_setup <- function(db_name = "mwi.db") {
  # Establish a connection to the SQLite database
  con <- dbConnect(SQLite(), dbname = db_name)

  # Define SQL commands for creating the tables
  sql_commands <- c(
    "CREATE TABLE IF NOT EXISTS Land (
       id INTEGER PRIMARY KEY,
       name TEXT UNIQUE,
       description TEXT,
       lang TEXT,
       created_at TEXT
     )",

    "CREATE TABLE IF NOT EXISTS Domain (
       id INTEGER PRIMARY KEY,
       name TEXT UNIQUE,
       http_status TEXT,
       title TEXT,
       description TEXT,
       keywords TEXT,
       created_at TEXT,
       fetched_at TEXT
     )",

    "CREATE TABLE IF NOT EXISTS Expression (
       id INTEGER PRIMARY KEY,
       land_id INTEGER,
       url TEXT,
       domain_id INTEGER,
       http_status TEXT,
       lang TEXT,
       title TEXT,
       description TEXT,
       keywords TEXT,
       readable TEXT,
       created_at TEXT,
       published_at TEXT,
       fetched_at TEXT,
       approved_at TEXT,
       readable_at TEXT,
       relevance INTEGER,
       depth INTEGER,
       FOREIGN KEY(land_id) REFERENCES Land(id),
       FOREIGN KEY(domain_id) REFERENCES Domain(id)
     )",

    "CREATE TABLE IF NOT EXISTS ExpressionLink (
       source_id INTEGER,
       target_id INTEGER,
       PRIMARY KEY(source_id, target_id),
       FOREIGN KEY(source_id) REFERENCES Expression(id),
       FOREIGN KEY(target_id) REFERENCES Expression(id)
     )",

    "CREATE TABLE IF NOT EXISTS Word (
       id INTEGER PRIMARY KEY,
       term TEXT,
       lemma TEXT
     )",

    "CREATE TABLE IF NOT EXISTS LandDictionary (
       land_id INTEGER,
       word_id INTEGER,
       PRIMARY KEY(land_id, word_id),
       FOREIGN KEY(land_id) REFERENCES Land(id),
       FOREIGN KEY(word_id) REFERENCES Word(id)
     )",

    "CREATE TABLE IF NOT EXISTS Media (
       id INTEGER PRIMARY KEY,
       expression_id INTEGER,
       url TEXT,
       type TEXT,
       FOREIGN KEY(expression_id) REFERENCES Expression(id)
     )",

    "CREATE TABLE IF NOT EXISTS Tag (
       id INTEGER PRIMARY KEY,
       land_id INTEGER,
       parent_id INTEGER,
       name TEXT,
       sorting INTEGER,
       color TEXT,
       FOREIGN KEY(land_id) REFERENCES Land(id),
       FOREIGN KEY(parent_id) REFERENCES Tag(id)
     )",

    "CREATE TABLE IF NOT EXISTS TaggedContent (
       tag_id INTEGER,
       expression_id INTEGER,
       text TEXT,
       from_char INTEGER,
       to_char INTEGER,
       FOREIGN KEY(tag_id) REFERENCES Tag(id),
       FOREIGN KEY(expression_id) REFERENCES Expression(id)
     )"
  )

  # Execute all table-creation commands atomically
  dbWithTransaction(con, {
    lapply(sql_commands, function(cmd) dbExecute(con, cmd))
  })
  dbDisconnect(con)
  message("Database setup completed.")
}

#' Return the id of a land (helper)
#'
#' Internal utility returning the numeric id of a land, stops if it does not exist.
#' @param con A DBI connection.
#' @param land_name Character scalar.
#' @return Integer id.
#' @keywords internal
#' @import DBI
.get_land_id <- function(con, land_name) {
  res <- dbGetQuery(con, "SELECT id FROM Land WHERE name = ?", params = list(land_name))$id
  if (length(res) == 0 || is.na(res)) stop(sprintf("Land '%s' not found.", land_name))
  res[1]
}

#' Create a new research project (land)
#'
#' This function creates a new entry in the Land table, representing a new research project.
#' If a land with the same name already exists, a message is displayed and no new entry is created.
#'
#' @param name A string specifying the name of the land.
#' @param desc A string providing a description of the land.
#' @param lang A string specifying the language of the land. Default is "en".
#' @param db_name A string specifying the name of the SQLite database file. Default is "mwi.db".
#' @return None. This function is called for its side effects.
#' @details
#' The function first checks if a land with the same name already exists in the database.
#' If it does, a message is displayed. Otherwise, a new entry is inserted into the Land table
#' with the provided name, description, language, and the current timestamp.
#'
#' @examples
#' \dontrun{
#' create_land("Climate Research", "Research on climate change impacts", "en", "my_database.db")
#' }
#' @export
create_land <- function(name, desc, lang = "en", db_name = "mwi.db") {
  # Establish a connection to the SQLite database
  con <- dbConnect(SQLite(), dbname = db_name)

  # Use prepared SQL commands to avoid SQL injection
  query_check <- "SELECT * FROM Land WHERE name = ?"
  query_insert <- "INSERT INTO Land (name, description, lang, created_at) VALUES (?, ?, ?, ?)"

  # Check if a land with the same name already exists
  existing_land <- dbGetQuery(con, query_check, params = list(name))

  if (nrow(existing_land) > 0) {
    message("A land with this name already exists.")
  } else {
    dbWithTransaction(con, {
      dbExecute(con, query_insert,
                params = list(name, desc, lang, format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    })
    message(sprintf("Land '%s' created", name))
  }

  # Close the connection
  dbDisconnect(con)
}

#' List research projects (lands)
#'
#' This function lists all lands or a specific land from the SQLite database.
#' It retrieves and displays information such as the name, creation date, description,
#' terms in the dictionary, total number of expressions, number of expressions remaining to be fetched,
#' and HTTP status codes of the expressions.
#'
#' @param land_name A string specifying the name of the land to list. If NULL, all lands are listed. Default is NULL.
#' @param db_name A string specifying the name of the SQLite database file. Default is "mwi.db".
#' @return None. This function is called for its side effects.
#' @details
#' The function retrieves and displays the following information for each land:
#' - Name of the land
#' - Creation date
#' - Description
#' - Terms in the dictionary
#' - Total number of expressions
#' - Number of expressions remaining to be fetched
#' - HTTP status codes of the expressions
#'
#' @examples
#' \dontrun{
#' listlands()
#' listlands("Climate Research", "my_database.db")
#' }
#' @export
listlands <- function(land_name = NULL, db_name = "mwi.db") {
  # Establish a connection to the SQLite database
  con <- dbConnect(SQLite(), dbname = db_name)

  # Prepare the SQL query to retrieve basic information about the lands
  if (is.null(land_name)) {
    sql_query <- "SELECT * FROM Land"
    lands <- dbGetQuery(con, sql_query)
  } else {
    sql_query <- "SELECT * FROM Land WHERE name = ?"
    lands <- dbGetQuery(con, sql_query, params = list(land_name))
  }

  if (nrow(lands) > 0) {
    for (i in 1:nrow(lands)) {
      land <- lands[i,]

      # Retrieve additional metrics for each land
      terms_query <- "SELECT term FROM Word INNER JOIN LandDictionary ON Word.id = LandDictionary.word_id WHERE LandDictionary.land_id = ?"
      terms <- dbGetQuery(con, terms_query, params = list(land$id))$term

      expressions_query <- "SELECT COUNT(*) FROM Expression WHERE land_id = ?"
      num_expressions <- dbGetQuery(con, expressions_query, params = list(land$id))[[1]]

      remaining_query <- "SELECT COUNT(*) FROM Expression WHERE land_id = ? AND fetched_at IS NULL"
      num_remaining <- dbGetQuery(con, remaining_query, params = list(land$id))[[1]]

      http_status_query <- "SELECT http_status, COUNT(*) FROM Expression WHERE land_id = ? GROUP BY http_status"
      http_statuses <- dbGetQuery(con, http_status_query, params = list(land$id))

      # Display the information
      cat(sprintf("Land name: %s\n", land$name))
      cat(sprintf("Creation date: %s\n", land$created_at))
      cat(sprintf("Description: %s\n", land$description))

      if (length(terms) > 0) {
        cat("Terms in the dictionary: ", paste(terms, collapse = ", "), "\n")
      } else {
        cat("No terms in the dictionary.\n")
      }

      cat(sprintf("Total number of expressions: %d\n", num_expressions))
      cat(sprintf("Number of expressions remaining to be fetched: %d\n", num_remaining))
      cat("HTTP status codes: ")
      for (j in 1:nrow(http_statuses)) {
        cat(sprintf("%s: %d ", http_statuses$http_status[j], http_statuses$`COUNT(*)`[j]))
      }
      cat("\n\n")
    }
  } else {
    if (is.null(land_name)) {
      cat("No lands found in the database.\n")
    } else {
      cat(sprintf("No land named '%s' found.\n", land_name))

      # Retrieve and display the list of existing lands
      all_lands_query <- "SELECT name FROM Land"
      all_lands <- dbGetQuery(con, all_lands_query)$name
      if (length(all_lands) > 0) {
        cat("Here is the list of existing lands: ", paste(all_lands, collapse = ", "), "\n")
      } else {
        cat("No lands found in the database.\n")
      }
    }
  }

  # Close the connection
  dbDisconnect(con)
}

#' Add terms to a research project (land)
#'
#' This function adds one or more terms to a specified land in the SQLite database.
#' If the terms do not already exist in the Word table, they are added along with their stemmed lemmas.
#' If the terms already exist, they are simply associated with the specified land in the LandDictionary table.
#'
#' @param land_name A string specifying the name of the land.
#' @param terms A comma-separated string of terms to add.
#' @return Integer 1 if successful, 0 otherwise.
#' @details
#' The function performs the following steps:
#' 1. Checks if the specified land exists in the Land table.
#' 2. Splits the terms string into individual terms.
#' 3. Converts each term to lowercase and checks if it already exists in the Word table.
#' 4. If a term does not exist, it is stemmed and added to the Word table along with its lemma.
#' 5. Each term is then associated with the specified land in the LandDictionary table.
#'
#' @examples
#' \dontrun{
#' addterm("Climate Research", "global warming", "climate", "change")
#' }
#' @export
addterm <- function(land_name, terms) {
  # Establish a connection to the SQLite database
  con <- dbConnect(SQLite(), dbname = "mwi.db")

  # Prepare SQL queries
  query_check_land <- "SELECT * FROM Land WHERE name = ?"
  query_check_term <- "SELECT * FROM Word WHERE term = ?"
  query_insert_term <- "INSERT INTO Word (term, lemma) VALUES (?, ?)"
  query_check_entry <- "SELECT * FROM LandDictionary WHERE land_id = ? AND word_id = ?"
  query_insert_entry <- "INSERT INTO LandDictionary (land_id, word_id) VALUES (?, ?)"

  # Check if the "land" exists
  res <- dbGetQuery(con, query_check_land, params = list(land_name))

  if (nrow(res) == 0) {
    message("Land ", land_name, " not found")
    dbDisconnect(con)
    return(0)
  }

  land_id <- res$id[1]
  land_lang <- res$lang[1]  # Retrieve the language from the database

  # Split the terms
  term_list <- strsplit(terms, ",")[[1]]
  term_list <- trimws(term_list)

  dbWithTransaction(con, {
    for (term in term_list) {
      # Convert the term to lowercase
      term <- tolower(term)

      # Check if the term already exists
      existing_term <- dbGetQuery(con, query_check_term, params = list(term))

      if (nrow(existing_term) == 0) {
        # Stem each word in the term
        words <- unlist(strsplit(term, " "))
        stemmed_words <- sapply(words, stem_word, language = land_lang)
        lemma <- paste(stemmed_words, collapse = " ")

        # Insert into the Word table
        dbExecute(con, query_insert_term, params = list(term, lemma))
        word_id <- dbGetQuery(con, "SELECT last_insert_rowid()")[1, 1]  # Get the ID of the last inserted row
      } else {
        word_id <- existing_term$id[1]  # Get the ID of the existing word
      }

      # Check if the combination of land_id and word_id already exists in LandDictionary
      existing_entry <- dbGetQuery(con, query_check_entry, params = list(land_id, word_id))

      if (nrow(existing_entry) == 0) {
        # Insert into the LandDictionary table
        dbExecute(con, query_insert_entry, params = list(land_id, word_id))
        message("Term added to land ", land_name)
      }
    }
  })

  # Close the connection to the database
  dbDisconnect(con)
  return(1)
}

#' Add URLs to a research project (land)
#'
#' This function adds one or more URLs to a specified land in the SQLite database.
#' URLs can be provided directly as a comma-separated string, read from a file, or imported interactively.
#'
#' When importing from a CSV file (e.g., from urlist_Google/Duck/Bing), the function will also
#' import title and date columns if available. The expected column names are:
#' - `link` or `url`: the URL (required)
#' - `title`: the page title (optional)
#' - `date`: the publication date (optional)
#'
#' @param land_name A string specifying the name of the land.
#' @param urls A comma-separated string of URLs to add. Default is NULL.
#' @param path A string specifying the path to a file containing URLs. Default is NULL.
#' @param db_name A string specifying the name of the SQLite database file. Default is "mwi.db".
#' @return Integer 1 if successful, 0 otherwise.
#' @details
#' The function performs the following steps:
#' 1. Checks if the specified land exists in the Land table.
#' 2. Retrieves the current date and time.
#' 3. Reads URLs from the provided list or file (CSV with link/title/date columns supported).
#' 4. Adds the URLs to the Expression table if they do not already exist.
#' 5. If title and date are available in the file, they are also added to the Expression table.
#'
#' @examples
#' \dontrun{
#' addurl("Climate Research", "http://example.com, http://example.org")
#' addurl("Climate Research", path = "urls.txt")
#' addurl("Climate Research", path = "serp_results.csv")  # CSV with link, title, date columns
#' }
#' @export
addurl <- function(land_name, urls = NULL, path = NULL, db_name = "mwi.db") {
  # Establish a connection to the SQLite database
  con <- dbConnect(SQLite(), dbname = db_name)
  on.exit(dbDisconnect(con), add = TRUE)

  # Check if the "land" exists
  query <- "SELECT id FROM Land WHERE name = ?"
  res <- dbGetQuery(con, query, params = list(land_name))

  if (nrow(res) == 0) {
    message("Land ", land_name, " not found")
    return(0)
  }

  land_id <- res$id[1]

  # Get the current date and time
  current_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # Prepare data frame for URLs with optional title and date
  url_data <- NULL

  if (!is.null(urls)) {
    # Direct URL input as comma-separated string
    url_list <- strsplit(urls, ",")[[1]]
    url_list <- trimws(url_list)
    url_data <- data.frame(link = url_list, title = NA_character_, date = NA_character_, stringsAsFactors = FALSE)
  } else if (!is.null(path)) {
    # Read from file
    ext <- tolower(tools::file_ext(path))
    if (ext %in% c("csv", "txt")) {
      # Try to read as CSV first (with headers)
      tryCatch({
        file_data <- read.csv(path, stringsAsFactors = FALSE, header = TRUE)
        # Check for expected columns (link or url)
        if ("link" %in% names(file_data)) {
          url_col <- "link"
        } else if ("url" %in% names(file_data)) {
          url_col <- "url"
        } else {
          # No recognizable URL column, treat as simple URL list
          url_list <- readLines(path, warn = FALSE)
          url_list <- trimws(url_list[url_list != ""])
          url_data <- data.frame(link = url_list, title = NA_character_, date = NA_character_, stringsAsFactors = FALSE)
          url_col <- NULL
        }

        if (!is.null(url_col)) {
          url_data <- data.frame(
            link = file_data[[url_col]],
            title = if ("title" %in% names(file_data)) file_data$title else NA_character_,
            date = if ("date" %in% names(file_data)) file_data$date else NA_character_,
            stringsAsFactors = FALSE
          )
        }
      }, error = function(e) {
        # If CSV parsing fails, read as simple text file
        url_list <- readLines(path, warn = FALSE)
        url_list <- trimws(url_list[url_list != ""])
        url_data <<- data.frame(link = url_list, title = NA_character_, date = NA_character_, stringsAsFactors = FALSE)
      })
    } else {
      # Unknown extension, read as text
      url_list <- readLines(path, warn = FALSE)
      url_list <- trimws(url_list[url_list != ""])
      url_data <- data.frame(link = url_list, title = NA_character_, date = NA_character_, stringsAsFactors = FALSE)
    }
  } else {
    # Interactive import
    imported <- importFile()
    if (is.data.frame(imported)) {
      if ("link" %in% names(imported)) {
        url_data <- data.frame(
          link = imported$link,
          title = if ("title" %in% names(imported)) imported$title else NA_character_,
          date = if ("date" %in% names(imported)) imported$date else NA_character_,
          stringsAsFactors = FALSE
        )
      } else if ("url" %in% names(imported)) {
        url_data <- data.frame(
          link = imported$url,
          title = if ("title" %in% names(imported)) imported$title else NA_character_,
          date = if ("date" %in% names(imported)) imported$date else NA_character_,
          stringsAsFactors = FALSE
        )
      } else {
        url_data <- data.frame(link = as.character(imported[[1]]), title = NA_character_, date = NA_character_, stringsAsFactors = FALSE)
      }
    } else {
      url_data <- data.frame(link = as.character(imported), title = NA_character_, date = NA_character_, stringsAsFactors = FALSE)
    }
  }

  # Remove empty URLs
  url_data <- url_data[!is.na(url_data$link) & url_data$link != "", ]

  # Check if we have title/date columns with data
  has_titles <- !all(is.na(url_data$title))
  has_dates <- !all(is.na(url_data$date))

  if (has_titles || has_dates) {
    message("Importing with metadata: title=", has_titles, ", date=", has_dates)
  }

  # Initialize the counter for new URLs
  new_urls_count <- 0

  dbWithTransaction(con, {
    for (i in seq_len(nrow(url_data))) {
      url <- url_data$link[i]
      title <- url_data$title[i]
      pub_date <- url_data$date[i]

      # Check if the URL already exists for this land
      query <- "SELECT 1 FROM Expression WHERE land_id = ? AND url = ?"
      existing_url <- dbGetQuery(con, query, params = list(land_id, url))

      if (nrow(existing_url) == 0) {
        # Build INSERT query with available fields
        if (!is.na(title) && !is.na(pub_date)) {
          dbExecute(con, "INSERT INTO Expression (land_id, url, title, published_at, created_at, depth) VALUES (?, ?, ?, ?, ?, 1)",
                    params = list(land_id, url, title, pub_date, current_time))
        } else if (!is.na(title)) {
          dbExecute(con, "INSERT INTO Expression (land_id, url, title, created_at, depth) VALUES (?, ?, ?, ?, 1)",
                    params = list(land_id, url, title, current_time))
        } else if (!is.na(pub_date)) {
          dbExecute(con, "INSERT INTO Expression (land_id, url, published_at, created_at, depth) VALUES (?, ?, ?, ?, 1)",
                    params = list(land_id, url, pub_date, current_time))
        } else {
          dbExecute(con, "INSERT INTO Expression (land_id, url, created_at, depth) VALUES (?, ?, ?, 1)",
                    params = list(land_id, url, current_time))
        }
        # Increment the counter for new URLs
        new_urls_count <- new_urls_count + 1
        # Print the message for the new URL
        message("URL added: ", url)
      }
    }
  })

  # Print the total number of new URLs added
  message("Total number of new URLs added: ", new_urls_count)
  message("URLs added to land ", land_name)
  return(1)
}

#' Import URLs from a file
#'
#' This function allows the user to choose a file and import URLs from it.
#' The file can be in CSV or TXT format.
#'
#' @return Data frame containing the imported URLs.
#' @details
#' The function performs the following steps:
#' 1. Allows the user to choose a file interactively.
#' 2. Detects the file extension.
#' 3. Reads the file based on the extension (CSV or TXT).
#' 4. Returns the data frame containing the imported URLs.
#'
#' @examples
#' \dontrun{
#' urls <- importFile()
#' }
#' @export
importFile <- function() {
  file_path <- file.choose()

  # Detect the file extension
  extension <- tolower(tools::file_ext(file_path))

  # Read the file based on the extension
  switch(extension,
         "csv" = {
           # Read a CSV file
           data <- tryCatch({
             read.csv(file_path, header = TRUE, sep = ",", stringsAsFactors = FALSE)
           }, error = function(e) {
             stop("Error reading the CSV file: ", e$message)
           })
         },
         "txt" = {
           # Read a TXT file
           # Adaptation for a TXT file with tab delimiter by default
           data <- tryCatch({
             read.delim(file_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE, col.names = c("url"))
           }, error = function(e) {
             stop("Error reading the TXT file: ", e$message)
           })
         },
         {
           stop("The file is neither a CSV nor a TXT.")
         }
  )

  return(data)
}

#' Delete a research project (land)
#'
#' This function deletes a specified land and its associated records from the SQLite database.
#' If a maximum relevance is specified, only the expressions with relevance less than or equal to this value are deleted.
#'
#' @param land_name A string specifying the name of the land to delete.
#' @param maxrel An integer specifying the maximum relevance for expressions to delete. Default is NULL.
#' @param db_name A string specifying the name of the SQLite database file. Default is "mwi.db".
#' @return Integer 1 if successful, 0 otherwise.
#' @details
#' The function performs the following steps:
#' 1. Checks if the specified land exists in the Land table.
#' 2. If a maximum relevance is specified, deletes expressions with relevance less than or equal to this value.
#' 3. If no maximum relevance is specified, deletes the land and all its associated records.
#'
#' @examples
#' \dontrun{
#' deleteland("Climate Research")
#' deleteland("Climate Research", maxrel = 2)
#' }
#' @export
deleteland <- function(land_name, maxrel = NULL, db_name = "mwi.db") {
  con <- dbConnect(SQLite(), dbname = db_name)

  on.exit(dbDisconnect(con), add = TRUE)

  # Check if the "land" exists
  query_land <- "SELECT id FROM Land WHERE name = ?"
  res <- dbGetQuery(con, query_land, params = list(land_name))

  if (nrow(res) == 0) {
    message("Land ", land_name, " not found")
    return(0)
  }

  land_id <- res$id[1]

  tryCatch({
    dbBegin(con)

    if (is.null(maxrel)) {
      # Delete the "land" and all its associated records
      dbExecute(con, "DELETE FROM ExpressionLink WHERE source_id IN (SELECT id FROM Expression WHERE land_id = ?)", params = list(land_id))
      dbExecute(con, "DELETE FROM Media WHERE expression_id IN (SELECT id FROM Expression WHERE land_id = ?)", params = list(land_id))
      dbExecute(con, "DELETE FROM TaggedContent WHERE expression_id IN (SELECT id FROM Expression WHERE land_id = ?)", params = list(land_id))
      dbExecute(con, "DELETE FROM Expression WHERE land_id = ?", params = list(land_id))
      dbExecute(con, "DELETE FROM LandDictionary WHERE land_id = ?", params = list(land_id))
      dbExecute(con, "DELETE FROM Tag WHERE land_id = ?", params = list(land_id))
      dbExecute(con, "DELETE FROM Land WHERE id = ?", params = list(land_id))
      message("Land ", land_name, " and all its associated records have been deleted")
    } else {
      # Delete only expressions with relevance <= maxrel and their associated records
      dbExecute(con, "DELETE FROM ExpressionLink WHERE source_id IN (SELECT id FROM Expression WHERE land_id = ? AND relevance <= ?)", params = list(land_id, maxrel))
      dbExecute(con, "DELETE FROM Media WHERE expression_id IN (SELECT id FROM Expression WHERE land_id = ? AND relevance <= ?)", params = list(land_id, maxrel))
      dbExecute(con, "DELETE FROM TaggedContent WHERE expression_id IN (SELECT id FROM Expression WHERE land_id = ? AND relevance <= ?)", params = list(land_id, maxrel))
      dbExecute(con, "DELETE FROM Expression WHERE land_id = ? AND relevance <= ?", params = list(land_id, maxrel))
      message("Expressions with relevance <=", maxrel, " and all their associated records have been deleted from land ", land_name)
    }

    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop("An error occurred: ", e$message)
  })

  return(1)
}

#' Stem a word
#'
#' This function stems a word using the specified language.
#'
#' @param word A string specifying the word to stem.
#' @param language A string specifying the language for stemming. Default is "en".
#' @return The stemmed word.
#' @details
#' The function converts the word to lowercase and then applies a stemming algorithm based on the specified language.
#'
#' @examples
#' \dontrun{
#' stem_word("running", "en")
#' }
#' @export
stem_word <- function(word, language = "en") {
  return(wordStem(tolower(word), language = language))
}

#' Tokenize a phrase
#'
#' This function tokenizes a phrase by splitting it into tokens based on commas.
#'
#' @param text A string specifying the phrase to tokenize.
#' @return A vector of tokens.
#' @details
#' The function splits the input text into tokens using commas as delimiters.
#'
#' @examples
#' \dontrun{
#' phrase_tokenizer("term1, term2, term3")
#' }
#' @export
phrase_tokenizer <- function(text) {
  unlist(strsplit(text, ","))
}

#' List domains for a research project (land)
#'
#' This function lists all unique domains associated with a specified land in the SQLite database.
#'
#' @param land_name A string specifying the name of the land.
#' @param db_name A string specifying the name of the SQLite database file. Default is "mwi.db".
#' @return A vector of domain names.
#' @details
#' The function performs the following steps:
#' 1. Establishes a connection to the SQLite database.
#' 2. Constructs and executes an SQL query to retrieve distinct domain names associated with the specified land.
#' 3. Returns a vector of domain names.
#'
#' @examples
#' \dontrun{
#' list_domain("Climate Research")
#' }
#' @export
list_domain <- function(land_name, db_name="mwi.db") {
  # Establish a connection to the SQLite database, with the dynamic filename
  conn <- dbConnect(RSQLite::SQLite(), db_name)

  # Construct the SQL query taking into account the table structure
  query <- "SELECT DISTINCT Domain.name FROM Domain
            JOIN Expression ON Domain.id = Expression.domain_id
            JOIN Land ON Expression.land_id = Land.id
            WHERE Land.name = ?"

  # Execute the query and retrieve the results
  results <- dbGetQuery(conn, query, params = list(land_name))

  # Close the connection
  dbDisconnect(conn)

  # Return the results
  return(results$name)
}

