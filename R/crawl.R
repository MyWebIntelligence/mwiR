#' Crawl a URL and extract content
#'
#' This function attempts to crawl a given URL using various methods, including
#' mercury-parser, trafilatura, and alternative methods to extract content.
#' It handles different content types, including PDFs.
#'
#' @param url A character string representing the URL to be crawled.
#' @return A data frame containing the extracted content from the URL.
#' @import reticulate
.trafilatura_mod <- reticulate::import("trafilatura", delay_load = FALSE)
#' @export
crawl <- function(url) {
  # Importation de Trafilatura
  trafilatura <- .trafilatura_mod

  # Fonction pour extraire le contenu
  readFull <- tryCatch({
    downloaded <- trafilatura$fetch_url(url)
    if (!is.null(downloaded)) {
      extracted_content <- trafilatura$extract(
        filecontent = downloaded,
        url = url,
        include_links = TRUE,
        output_format = "json",
        with_metadata = TRUE
      )
      if (!is.null(extracted_content) && extracted_content != "") {
        # Convertir le JSON en structure R
        message("Extraction avec Trafilutara")
        jsonlite::fromJSON(extracted_content, simplifyVector = TRUE)
      } else {
        stop("Extraction échouée ou contenu vide.")
      }
    } else {
      stop("Téléchargement échoué ou URL invalide.")
    }
  }, error = function(e) {
    message("Erreur lors de la récupération ou de l'extraction avec Trafilatura : ", e$message)
    NULL
  })

  # Si l'extraction principale échoue, essayer avec Archive.org
  if (is.null(readFull$title)) {
    message("Tentative avec Archive.org")
    urlarchive <- get_last_memento_url(url)
    if (!is.null(urlarchive)) {
      tryCatch({
        message("Extraction fallback avec Archive.org")
        downloaded <- trafilatura$fetch_url(urlarchive)
        if (!is.null(downloaded)) {
          extracted_content <- trafilatura$extract(
            filecontent = downloaded,
            url = urlarchive,
            include_links = TRUE,
            output_format = "json",
            with_metadata = TRUE
          )
          if (!is.null(extracted_content) && extracted_content != "") {
            message("Extraction avec Archive.org")
            readFull <- jsonlite::fromJSON(extracted_content, simplifyVector = TRUE)
          } else {
            stop("Extraction échouée ou contenu vide pour le memento.")
          }
        } else {
          stop("Téléchargement du memento échoué ou URL invalide.")
        }
      }, error = function(e) {
        message("Erreur lors de la récupération avec Archive.org : ", e$message)
      })
    } else {
      message("Aucun memento disponible pour l'URL originale")
    }
  }

  # Gestion des métadonnées et renvoi sous forme de DataFrame
  if (!is.null(readFull) && !is.null(readFull$title)) {
    if (!"date" %in% names(readFull) || is.null(readFull$date)) {
      readFull$date <- "Date inconnue"
    }
    message("Extraction reussie de : ", readFull$title)
    return(as.data.frame(t(unlist(readFull))))
  }

  # Si tout échoue, utilisation de httr::GET pour des solutions alternatives
  response <- tryCatch({
    message("Tentative avec GET")
    httr::GET(url, httr::timeout(10))
  }, error = function(e) {
    NULL
  })

  if (!is.null(response)) {
    content_type <- httr::http_type(response)
    if (grepl("application/pdf", content_type, ignore.case = TRUE)) {
      message("Tentative de lecture d'un PDF")
      pdf_content <- PDFtoText(url) # Fonction personnalisée pour extraire du texte PDF
      if (!is.null(pdf_content)) {
        title <- unlist(strsplit(pdf_content, "\n"))[1]
        excerpt <- substr(pdf_content, nchar(title) + 2, nchar(title) + 651)
        return(data.frame(
          title = trimws(title),
          date = "Date inconnue",
          text = trimws(pdf_content),
          excerpt = trimws(excerpt),
          hostname = httr::parse_url(url)$hostname,
          stringsAsFactors = FALSE
        ))
      }
    } else {
      # Extraction HTML classique
      tryCatch({
        message("Tentative avec GET HTML")
        parsed_content <- xml2::read_html(httr::content(response, as = "text"))
        title <- xml2::xml_text(xml2::xml_find_first(parsed_content, "//title"))
        body_text <- xml2::xml_text(xml2::xml_find_first(parsed_content, "//body"))
        domain <- httr::parse_url(url)$hostname
        date_published <- httr::headers(response)$`last-modified` %||% "Date non disponible"
        return(data.frame(
          title = title,
          date = date_published,
          text = body_text,
          excerpt = NA,
          hostname = domain,
          stringsAsFactors = FALSE
        ))
      }, error = function(e) {
        message("Extraction HTML échouée pour l'URL : ", url)
        return(NULL)
      })
    }
  } else {
    message("Toutes les méthodes ont échoué pour l'URL : ", url)
  }
  return(NULL)
}



#' Extract text from a PDF URL
#'
#' This function downloads a PDF from a given URL and attempts to extract
#' the text using `pdftools` and OCR if necessary.
#'
#' @param url A character string representing the URL of the PDF.
#' @return A character string containing the extracted text from the PDF.
#' @export
# Fonction pour extraire le texte d'un PDF à partir d'une URL
PDFtoText <- function(url) {
  # Download the PDF
  temp_file <- tempfile(fileext = ".pdf")
  download <- httr::GET(url, httr::write_disk(temp_file))

  if (httr::http_status(download)$category != "Success") {
    return(paste("Download error: ", httr::http_status(download)$message))
  }

  # Attempt to extract text with pdftools
  text <- pdftools::pdf_text(temp_file)

  # If the text is empty or contains non-printable characters,
  # use OCR to extract the text
  if (length(text) == 0) {
    ocr_text <- tesseract::ocr(temp_file)
    return(ocr_text)
  } else {
    return(paste(text, collapse = "\n"))
  }
}


#' Clean the extracted URL
#'
#' This function removes fragment identifiers and keeps only valid URL characters.
#'
#' @param url A character string representing the URL to be cleaned.
#' @return A cleaned URL as a character string.
#' @importFrom stringr str_replace str_extract
#' @export
clean_url <- function(url) {
  # Remove fragment identifiers (anything after a #)
  url <- str_replace(url, "#.*$", "")

  # Keep only valid URL characters
  url <- str_extract(url, "https?://[A-Za-z0-9-._~:/?#\\[\\]@!$&'()*+,;=]+")

  return(url)
}

#' Check if a link is a media link
#'
#' This function checks if a given link points to a media file based on its extension.
#'
#' @param link A character string representing the link to be checked.
#' @return A logical value indicating whether the link is a media link.
#' @importFrom stringr str_detect
#' @export
is_media_link <- function(link) {
  if (is.na(link)) {
    return(FALSE)
  }

  media_extensions <- c('.jpg$', '.jpeg$', '.png$', '.bmp$', '.webp$', '.txt$', '.csv$', '.xls$', '.xlsx$', '.doc$', '.docx$', '.mp3$', '.mp4$', '.avi$', '.mov$')
  for (ext in media_extensions) {
    if (str_detect(link, ext)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Detect links in content and add to database
#'
#' This function extracts links from content, cleans them, and adds them to the database.
#' It handles both media and non-media links.
#'
#' @param con A database connection object.
#' @param content A character string containing the content to be parsed for links.
#' @param parent_id An integer representing the ID of the parent record in the database.
#' @param land_id An integer representing the ID of the land associated with the links.
#' @param urlmax An integer specifying the maximum number of URLs to be processed.
#' @import DBI
#' @importFrom stringr str_extract_all str_extract
#' @export
detect_links_and_add <- function(con, content, parent_id, land_id, urlmax=50) {
  # Refined regex pattern for URL extraction
  links <- str_extract_all(content, "(?<=\\s)https?://[^\\s]+(?=\\s)|(?<=\\()https?://[^\\)]+(?=\\)\\s)")

  # Counter for non-media links
  non_media_count <- 0

  for (link in links[[1]]) {
    # If the counter reaches urlmax, exit the loop
    if (non_media_count >= urlmax) {
      break
    }

    # Clean the extracted link
    clean_link <- clean_url(link)

    if (!is.na(clean_link) && is_media_link(clean_link)) {
      # Retrieve the media extension
      media_type <- str_extract(clean_link, "\\.([a-zA-Z]+)(?=[?#]|$)")

      # Add to the Media table
      dbExecute(con, "INSERT INTO Media (url, expression_id, type) VALUES (?, ?, ?)", params = list(clean_link, parent_id, media_type))
    } else if (!is.na(clean_link)) {
      # Increment the counter for non-media links
      non_media_count <- non_media_count + 1

      res <- dbGetQuery(con, "SELECT * FROM Expression WHERE url = ?", params = list(clean_link))

      if (nrow(res) == 0) {
        parent_depth <- as.integer(dbGetQuery(con, "SELECT depth FROM Expression WHERE id = ?", params = list(parent_id)))

        # Insert the new URL into the "Expression" table
        dbExecute(con, "INSERT INTO Expression (url, land_id, created_at, depth) VALUES (?, ?, ?, ?)", params = list(clean_link, land_id, format(Sys.time(), format = "%Y-%m-%dT%H:%M:%OS3Z"), parent_depth + 1))

        new_url_id <- dbGetQuery(con, "SELECT last_insert_rowid()")[1, 1]
      } else {
        new_url_id <- res$id[1]
      }

      # Check if the link already exists in ExpressionLink
      link_exists <- dbGetQuery(con, "SELECT * FROM ExpressionLink WHERE source_id = ? AND target_id = ?", params = list(parent_id, new_url_id))

      if (nrow(link_exists) == 0) {
        # Insert the link into the "ExpressionLink" table
        dbExecute(con, "INSERT INTO ExpressionLink (source_id, target_id) VALUES (?, ?)", params = list(parent_id, new_url_id))
      }
    }
  }
}

#' Stem a word
#'
#' This function performs stemming on a given word using the specified language.
#'
#' @param word A character string representing the word to be stemmed.
#' @param language A character string specifying the language for stemming (default is "fr").
#' @return The stemmed version of theword.
#' @import SnowballC
#' @export
stem_word <- function(word, language = "fr") {
  return(wordStem(tolower(word), language = language))
}

#' Tokenize phrases
#'
#' This function splits a text into phrases based on commas.
#'
#' @param text A character string representing the text to be tokenized.
#' @return A character vector of tokenized phrases.
#' @export
phrase_tokenizer <- function(text) {
  unlist(strsplit(text, ","))
}

#' Create a dictionary from text
#'
#' This function creates a dictionary by tokenizing and stemming phrases from the input text.
#'
#' @param text A character string representing the input text.
#' @param language A character string specifying the language for stemming (default is "fr").
#' @return A data frame containing the stemmed phrases as the dictionary.
#' @export
mkdictionary <- function(text, language = "fr") {
  phrases <- phrase_tokenizer(text)
  stemmed_phrases <- lapply(phrases, function(phrase) {
    words <- unlist(strsplit(phrase, " "))
    stemmed_words <- lapply(words, function(w) stem_word(w, language))
    paste(stemmed_words, collapse = " ")
  })
  return(data.frame(lemma = unlist(stemmed_phrases)))
}

#' Tokenize words
#'
#' This function splits a text into words.
#'
#' @param text A character string representing the text to be tokenized.
#' @return A character vector of tokenized words.
#' @importFrom stringr str_split boundary
#' @export
word_tokenizer <- function(text) {
  unlist(str_split(text, boundary("word")))
}

#' Calculate relevance of text
#'
#' This function calculates the relevance of a text based on a dictionary of terms.
#'
#' @param text A character string representing the text to be analyzed.
#' @param weight A numeric value representing the weight of the relevance.
#' @param dictionary A character vector of terms to be used for relevance calculation.
#' @param language A character string specifying the language for stemming (default is "fr").
#' @return A numeric vector of relevance scores.
#' @importFrom stringr str_count
#' @export
get_relevance <- function(text, weight, dictionary, language = "fr") {
  stems <- unlist(lapply(word_tokenizer(text), function(w) stem_word(w, language)))
  stemmed_text <- paste(stems, collapse = " ")
  lemma_counts <- sapply(dictionary, function(lemma) {
    sum(str_count(stemmed_text, paste0("\\b", lemma, "\\b")))
  })
  return(lemma_counts * weight)
}

#' Calculate expression relevance
#'
#' This function calculates the relevance of an expression based on its title and text.
#'
#' @param dictionary A character vector of terms to be used for relevance calculation.
#' @param expression A data frame containing the title and text of the expression.
#' @param language A character string specifying the language for stemming (default is "fr").
#' @return A numeric value representing the relevance score.
#' @export
expression_relevance <- function(dictionary, expression, language = "fr") {
  # Convert title and text to lowercase
  expression$title <- tolower(expression$title)
  expression$text <- tolower(expression$text)

  title_relevance <- 0
  content_relevance <- 0

  tryCatch({
    title_relevance <- sum(get_relevance(expression$title, 10, dictionary, language))
    content_relevance <- sum(get_relevance(expression$text, 1, dictionary, language))
  }, error = function(e) {
    print(paste("Error:", e$message))
  })

  return(title_relevance + content_relevance)
}

#' Crawl URLs for a specific land
#'
#' This function crawls URLs for a specific land, updates the database, and calculates relevance scores.
#'
#' @param land_name A character string representing the name of the land.
#' @param urlmax An integer specifying the maximum number of URLs to be processed (default is 50).
#' @param limit An optional integer specifying the limit on the number of URLs to crawl.
#' @param http_status An optional character string specifying the HTTP status to filter URLs.
#' @param db_name A character string representing the name of the database (default is "mwi.db").
#' @return An integer indicating the number of URLs processed.
#' @import DBI RSQLite
#' @export
crawlurls <- function(land_name, urlmax=50, limit = NULL, http_status = NULL, db_name = "mwi.db") {

  con <- dbConnect(SQLite(), dbname = db_name)
  dbBegin(con)

  res <- dbGetQuery(con, "SELECT id, lang FROM Land WHERE name = ?", params = list(land_name))

  if (nrow(res) == 0) {
    message("Land ", land_name, " not found")
    dbDisconnect(con)
    return(0)
  }

  land_id <- res$id[1]

  # Extract the language associated with the land from the Land table
  language <- res$lang[1]
  if (is.null(language) || language == "") {
    language <- "fr"
  }

  # Extract keywords associated with the land from the database
  dictionary <- dbGetQuery(con, "SELECT lemma FROM LandDictionary JOIN Word ON word_id = id WHERE land_id = ?", params = list(land_id))

  sql_query <- "SELECT id, url FROM Expression WHERE approved_at IS NULL AND fetched_at IS NULL AND land_id = ?"

  params <- list(land_id)

  if (!is.null(http_status)) {
    sql_query <- paste0(sql_query, " AND http_status = ?")
    params <- c(params, http_status)
  }

  # Add ORDER BY
  sql_query <- paste0(sql_query, " ORDER BY depth ASC")

  if (!is.null(limit)) {
    sql_query <- paste0(sql_query, " LIMIT ?")
    params <- c(params, limit)
  }

  urls_to_crawl <- dbGetQuery(con, sql_query, params = params)

  timestamp_now <- format(Sys.time(), format = "%Y-%m-%dT%H:%M:%OS3Z")
  for (i in 1:nrow(urls_to_crawl)) {
    tryCatch({
      message("start : ", urls_to_crawl$url[i])
      dbExecute(con, "UPDATE Expression SET fetched_at = ? WHERE id = ?", params = list(timestamp_now, urls_to_crawl$id[i]))

      url_data <- crawl(urls_to_crawl$url[i])

      relevance_score <- expression_relevance(dictionary$lemma, url_data, language)

      if (relevance_score > 0) {
        if (!is.null(url_data$hostname)) {
          existing_domain <- dbGetQuery(con, "SELECT id, name FROM Domain WHERE name = ?", params = list(url_data$hostname))
          detected_lang <- detect_language(as.character(url_data$text[1]))

          if (nrow(existing_domain) == 0) {
            dbExecute(con, "INSERT INTO Domain (name) VALUES (?)", params = list(url_data$hostname))
            domain_id <- dbGetQuery(con, "SELECT last_insert_rowid()")[1, 1]
          } else {
            domain_id <- existing_domain$id[1]
          }

          dbExecute(con, "UPDATE Expression SET title = ?, readable = ?, domain_id = ?, description = ?, published_at = ?, approved_at = ?, lang = ?, relevance = ? WHERE id = ?", params = list(url_data$title, url_data$text[1], domain_id, url_data$excerpt, url_data$date, timestamp_now, detected_lang, relevance_score, urls_to_crawl$id[i]))

          # add links to data base
          detect_links_and_add(con, url_data$text[1], urls_to_crawl$id[i], land_id, urlmax)

          # Placeholder for the detect_links_and_add function
          message("Crawled URL: ", urls_to_crawl$url[i])
        } else {
          message("Both primary and fallback crawls failed for URL: ", urls_to_crawl$url[i])
        }
      }
    }, error = function(e) {
      message("Error processing URL: ", urls_to_crawl$url[i], " Error: ", e)
    })
  }

  dbCommit(con)
  dbDisconnect(con)

  message("Crawling completed for land ", land_name)
  return(1)
}

#' Crawl and extract details from a URL
#'
#' This function crawls a URL and extracts detailed information such as meta descriptions,
#' Open Graph descriptions, Twitter descriptions, keywords, and header tags.
#'
#' @param url A character string representing the URL to be crawled.
#' @return A character string containing the extracted details.
#' @export
crawlDetails <- function(url) {
  if (is.null(url) || url == "") {
    return("")
  }

  response <- try(httr::GET(url), silent = TRUE)
  if (inherits(response, "try-error")) {
    return(paste("Get URL Domain make an error:", url))
  }

  content <- httr::content(response, as = "text")
  doc <- XML::htmlParse(content, asText = TRUE)

  result <- ""

  # Get text from HTML
  get_nodes_text <- function(xpath) {
    nodes <- XML::xpathSApply(doc, xpath, XML::xmlValue)
    if (!is.null(nodes)) {
      return(paste(nodes, collapse = " "))
    }
    return("")
  }

  # Get text from attributes nodes form HTML
  get_attribute_value <- function(xpath) {
    attributes <- XML::xpathSApply(doc, xpath, XML::xmlGetAttr, "content")
    if (!is.null(attributes)) {
      return(paste(attributes, collapse = " "))
    }
    return("")
  }

  # Meta Description
  result <- paste(result, get_attribute_value('//meta[@name="description"]'))

  # Open Graph Description
  result <- paste(result, get_attribute_value('//meta[@property="og:description"]'))

  # Twitter Description
  result <- paste(result, get_attribute_value('//meta[@name="twitter:description"]'))

  # Meta Keywords
  result <- paste(result, get_attribute_value('//meta[@name="keywords"]'))

  # H1 Tags
  result <- paste(result, get_nodes_text('//h1'))

  # H2 Tags
  result <- paste(result, get_nodes_text('//h2'))

  # Bold/Strong Text
  result <- paste(result, get_nodes_text('//strong | //b'))

  # Alt Text from Images
  altTexts <- xpathSApply(doc, '//img/@alt')
  result <- paste(result, paste(altTexts[!is.na(altTexts)], collapse = " "))

  # Truncate if too long
  if (nchar(result) > 1200) {
    result <- substr(result, 1, 1200)
  }

  return(trimws(result)) # Trim the final string to remove any trailing space
}

#' Crawl and update the Domain table
#'
#' This function crawls domains and updates the Domain table with the fetched data.
#'
#' @param nburl An integer specifying the number of URLs to be crawled (default is 100).
#' @param db_name A character string representing the name of the database (default is "mwi.db").
#' @import DBI RSQLite
#' @export
crawlDomain <- function(nburl = 100, db_name = "mwi.db") {
  # connexion to Data Base SQLite
  con <- dbConnect(SQLite(), dbname = db_name)
  dbBegin(con)

  # Extract nburl URLs from Domain table of DB
  sql_query <- paste0("SELECT * FROM Domain WHERE fetched_at IS NULL LIMIT ", nburl)
  urls_to_crawl <- dbGetQuery(con, sql_query)

  # Update each row of Domain table of Data Base
  for (i in 1:nrow(urls_to_crawl)) {
    tryCatch({
      update_query <- "UPDATE Domain SET fetched_at = ? WHERE id = ?"
      dbExecute(con, update_query, params = list(
        format(Sys.time(), format = "%Y-%m-%d %H:%M:%S"),
        urls_to_crawl$id[i]
      ))

      url_data <- tryCatch({
        crawl(paste("https://", urls_to_crawl$name[i], sep = ""))
      }, error = function(e) {
        message(paste("Error crawling main URL:", urls_to_crawl$name[i], "-", e$message))
        NULL
      })

      url_datadetail <- tryCatch({
        crawlDetails(paste("https://", urls_to_crawl$name[i], sep = ""))
      }, error = function(e) {
        message(paste("Error crawling details URL:", urls_to_crawl$name[i], "-", e$message))
        NULL
      })

      if (!is.null(url_data) && !is.null(url_data$title) && length(url_data$title) == 1) {
        # Update moment
        update_query <- "UPDATE Domain SET title = ?, description = ? WHERE id = ?"
        dbExecute(con, update_query, params = list(
          url_data$title,
          paste(url_data$excerpt, url_datadetail, sep = "\n\n"),
          urls_to_crawl$id[i]
        ))

        message(paste(urls_to_crawl$name[i], "domain crawled"))
      } else {
        #Alert for error on update
        message(paste(urls_to_crawl$name[i], "domain could not be crawled due to insufficient data"))
      }
    }, error = function(e) {
      message(paste("Error processing URL:", urls_to_crawl$name[i], "-", e$message))
    })
  }

  # Never forget to close connexion
  dbCommit(con)
  dbDisconnect(con)

  message("The update of Domain table is succed. ")
}


#' Force crawl for a specific land
#'
#' This function forces a crawl by resetting the fetched status for URLs that have not been approved. So next time the crawl will rework on this url.
#'
#' @param land_name A character string representing the name of the land.
#' @param db_name A character string representing the name of the database (default is "mwi.db").
#' @import DBI RSQLite
#' @export
crawlForce <- function(land_name, db_name = "mwi.db") {

  # Connect to the SQLite database
  con <- dbConnect(RSQLite::SQLite(), db_name)

  # Set the fetched_at column to NULL where approved_at is NULL
  update_query <- "UPDATE Expression SET fetched_at = NULL WHERE approved_at IS NULL AND fetched_at IS NOT NULL AND land_id = (SELECT id FROM Land WHERE name = ?)"

  dbExecute(con, update_query, params = list(land_name))

  # Close the database connection
  dbDisconnect(con)
}

#' Check URL availability in Archive.org
#'
#' This function checks if a given URL is available in Archive.org.
#'
#' @param url A character string representing the URL to be checked.
#' @return A list containing the closest snapshot details if available, otherwise NULL.
#' @export
archive_available <- function(url) {
  # Construct the API URL to check availability
  api_url <- paste0("https://archive.org/wayback/available?url=", url)

  # Try the GET request with error handling
  tryCatch({
    response <- httr::GET(api_url)
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, as = "text", encoding = "UTF-8")
      json_content <- jsonlite::fromJSON(content)
      return(json_content$archived_snapshots$closest)
    } else {
      stop("Error: Unable to check the availability of the URL.")
    }
  }, error = function(e) {
    message("Error during the GET request to check availability: ", e)
    return(NULL)
  })
}


#' Get the most recent archived URL from Archive.org
#'
#' This function retrieves the most recent archived URL from Archive.org for a given URL.
#'
#' @param url A character string representing the URL to be archived.
#' @return A character string containing the most recent archived URL, or NULL if not available.
#' @export
get_last_memento_url <- function(url) {
  isAvailable <- archive_available(url)

  if (!is.null(isAvailable) && isAvailable$available) {
    timestamp <- isAvailable$timestamp
    url_archive <- paste0("http://web.archive.org/web/", timestamp, "/", url)
    return(url_archive)
  } else {
    return(NULL)
  }
}
