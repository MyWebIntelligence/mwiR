#' Crawl a URL and extract content
#'
#' This function attempts to crawl a given URL using various methods, including
#' mercury-parser, trafilatura, and alternative methods to extract content.
#' It handles different content types, including PDFs.
#'
#' @param url A character string representing the URL to be crawled.
#' @return A data frame containing the extracted content from the URL.
#' @import httr jsonlite rvest
#' @export
crawl <- function(url) {

  # Tentative de crawl avec mercury-parser
  readFull <- tryCatch({
    # Télécharger le contenu de la page
    downloaded <- trafilatura$fetch_url(url)

    # Extraire le contenu principal au format Markdown
    readFull <- parse_json(trafilatura$extract(downloaded, include_links=TRUE, output_format = 'json'), simplifyVector = T)

  }, error = function(e) {
    message("Erreur lors du téléchargement ou de l'extraction du contenu avec TRAFILATURA : ")
    NULL
  })

  if (is.null(readFull)) {

    urlarchive <- get_last_memento_url(url)

    # Télécharger le contenu de la page
    tryCatch({
      downloaded <- trafilatura$fetch_url(urlarchive)
      if (!is.null(downloaded) && downloaded != "") {
        extracted_content <- trafilatura$extract(downloaded, include_links = TRUE, output_format = 'json')
        if (!is.null(extracted_content) && extracted_content != "") {
          readFull <- jsonlite::fromJSON(extracted_content, simplifyVector = TRUE)
          ifelse(!is.null(readFull$hostname), return(readFull), readFull <- NULL)
        } else {
          stop("Erreur: Contenu extrait non valide ou vide.")
        }
      } else {
        stop("Erreur: Téléchargement non valide ou vide.")
      }
    }, error = function(e) {
      message("Erreur lors du téléchargement ou de l'extraction du contenu avec ARCHIVE.ORG : ")
      return(NULL)
    })
  }


  # Si parser réussit et le titre n'est pas null
  if (!is.null(readFull) && !is.null(readFull$title)) {
    #verifie si date_published existe
    if (!"date" %in% names(readFull) || is.null(readFull$date)) {
      readFull$date <- "date inconnue"
    }
    return(as.data.frame(t(unlist(readFull))))
  }

  response <- tryCatch({
    GET(url, timeout(10))
  }, error = function(e) {
    NULL
  })

  if (is.null(response)) {
    message("ALL parser and GET failed for URL:", url)
    return(NULL)
  }

  content_type <- httr::http_type(response)

  # Si le contenu est de type PDF
  if (grepl("application/pdf", content_type, ignore.case = TRUE)) {
    pdf_content <- PDFtoText(url)  # Remplacez par votre fonction réelle
    title <- unlist(strsplit(pdf_content, "\n"))[1]  # La première ligne comme titre
    content <- pdf_content  # Contenu total
    excerpt <- substr(content, nchar(title) + 2, nchar(title) + 651)  # Extrait après la première ligne

    parsed_url <- url_parse(url)
    domain <- parsed_url$domain
    message("PDF crawled")
    return(data.frame(
      title = trimws(title),
      date = "Date inconnue",
      text = trimws(content),
      excerpt = trimws(excerpt),
      hostname = domain,
      stringsAsFactors = FALSE
    ))
  } else {
    # Crawl alternatif
    tryCatch({


      content <- content(response, as = "text")
      parsed_content <- read_html(content)

      # Extraire le titre et la description
      title <- html_nodes(parsed_content, "title") %>% html_text(trim = TRUE)
      description <- html_nodes(parsed_content, "meta[name='description']") %>% html_attr("content")
      body_text <- html_nodes(parsed_content, "body") %>% html_text(trim = TRUE)

      # Si les balises ne sont pas trouvées, utilisez un texte par défaut
      title <- ifelse(length(title) == 0, "Titre non disponible", title)
      description <- ifelse(length(description) == 0, "Description non disponible", description)

      parsed_url <- url_parse(url)
      domain <- parsed_url$domain

      # Extraire la date de dernière modification si disponible
      last_modified <- headers(response)$`last-modified`
      if (is.null(last_modified)) {
        date_published <- "Date non disponible"
      } else {
        date_published <- last_modified
      }
      message("GET Direct crawled")
      return(data.frame(
        title = title,
        date = last_modified,
        text = body_text,
        excerpt = description,
        hostname = domain,
        stringsAsFactors = FALSE
      ))
      print(paste("Crawl alternatif de", url))
    }, error = function(e) {
      warning("ALL GET failed for URL:", url)
      return(NULL)
    })
  }
}

#' Extract text from a PDF URL
#'
#' This function downloads a PDF from a given URL and attempts to extract
#' the text using `pdftools` and OCR if necessary.
#'
#' @param url A character string representing the URL of the PDF.
#' @return A character string containing the extracted text from the PDF.
#' @import httr pdftools tesseract
#' @export
# Fonction pour extraire le texte d'un PDF à partir d'une URL
PDFtoText <- function(url) {
  # Télécharger le PDF
  temp_file <- tempfile(fileext = ".pdf")
  download <- GET(url, write_disk(temp_file))

  if (http_status(download)$category != "Success") {
    return(paste("Erreur de téléchargement: ", http_status(download)$message))
  }

  # Tenter d'extraire du texte avec pdftools
  text <- pdf_text(temp_file)

  # Si le texte est vide ou contient des caractères non imprimables,
  # utiliser OCR pour extraire le texte
  if (length(text) == 0) {
    ocr_text <- ocr(temp_file)
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
#' @import stringr
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
#' @import stringr
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
#' @import DBI stringr
#' @export
detect_links_and_add <- function(con, content, parent_id, land_id, urlmax=50) {
  # Refined regex pattern for URL extraction
  links <- str_extract_all(content, "(?<=\\s)https?://[^\\s]+(?=\\s)|(?<=\\()https?://[^\\)]+(?=\\)\\s)")

  # Compteur pour les liens non-médias
  non_media_count <- 0

  for (link in links[[1]]) {
    # Si le compteur atteint urlmax, sortir de la boucle
    if (non_media_count >= urlmax) {
      break
    }

    # Clean the extracted link
    clean_link <- clean_url(link)

    if (!is.na(clean_link) && is_media_link(clean_link)) {
      # Récupérer l'extension du média
      media_type <- str_extract(clean_link, "\\.([a-zA-Z]+)(?=[?#]|$)")

      # Ajouter à la table Media
      dbExecute(con, "INSERT INTO Media (url, expression_id, type) VALUES (?, ?, ?)", params = list(clean_link, parent_id, media_type))
    } else if (!is.na(clean_link)) {
      # Incrémenter le compteur pour les liens non-médias
      non_media_count <- non_media_count + 1

      res <- dbGetQuery(con, "SELECT * FROM Expression WHERE url = ?", params = list(clean_link))

      if (nrow(res) == 0) {
        parent_depth <- as.integer(dbGetQuery(con, "SELECT depth FROM Expression WHERE id = ?", params = list(parent_id)))

        # Insère le nouvel URL dans la table "Expression"
        dbExecute(con, "INSERT INTO Expression (url, land_id, created_at, depth) VALUES (?, ?, ?, ?)", params = list(clean_link, land_id, format(Sys.time(), format = "%Y-%m-%dT%H:%M:%OS3Z"), parent_depth + 1))

        new_url_id <- dbGetQuery(con, "SELECT last_insert_rowid()")[1, 1]
      } else {
        new_url_id <- res$id[1]
      }

      # Check if the link already exists in ExpressionLink
      link_exists <- dbGetQuery(con, "SELECT * FROM ExpressionLink WHERE source_id = ? AND target_id = ?", params = list(parent_id, new_url_id))

      if (nrow(link_exists) == 0) {
        # Insère le lien dans la table "ExpressionLink"
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
#' @param language A character string specifying the language for stemming (default is "en").
#' @return The stemmed version of the word.
#' @import SnowballC
#' @export
stem_word <- function(word, language = "en") {
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
#' @param language A character string specifying the language for stemming (default is "en").
#' @return A data frame containing the stemmed phrases as the dictionary.
#' @import dplyr stringr
#' @export
mkdictionary <- function(text, language = "en") {
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
#' @import stringr
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
#' @param language A character string specifying the language for stemming (default is "en").
#' @return A numeric vector of relevance scores.
#' @import stringr
#' @export
get_relevance <- function(text, weight, dictionary, language = "en") {
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
#' @param language A character string specifying the language for stemming (default is "en").
#' @return A numeric value representing the relevance score.
#' @import stringr
#' @export
expression_relevance <- function(dictionary, expression, language = "en") {
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

  trafilatura <- import("trafilatura")

  con <- dbConnect(SQLite(), dbname = db_name)

  res <- dbGetQuery(con, "SELECT id, lang FROM Land WHERE name = ?", params = list(land_name))

  if (nrow(res) == 0) {
    print(paste("Land", land_name, "not found"))
    dbDisconnect(con)
    return(0)
  }

  land_id <- res$id[1]

  # Extract the language associated with the land from the Land table
  language <- res$lang[1]

  # Extract keywords associated with the land from the database
  dictionary <- dbGetQuery(con, "SELECT lemma FROM LandDictionary JOIN Word ON word_id = id WHERE land_id = ?", params = list(land_id))

  sql_query <- "SELECT id, url FROM Expression WHERE approved_at IS NULL AND fetched_at IS NULL AND land_id = ?"

  params <- list(land_id)

  if (!is.null(http_status)) {
    sql_query <- paste0(sql_query, " AND http_status = ?")
    params <- c(params, http_status)
  }

  # Ajout de la clause ORDER BY
  sql_query <- paste0(sql_query, " ORDER BY depth ASC")

  if (!is.null(limit)) {
    sql_query <- paste0(sql_query, " LIMIT ?")
    params <- c(params, limit)
  }

  urls_to_crawl <- dbGetQuery(con, sql_query, params = params)

  for (i in 1:nrow(urls_to_crawl)) {
    tryCatch({
      print(paste("start : ", urls_to_crawl$url[i], sep=" "))
      dbExecute(con, "UPDATE Expression SET fetched_at = ? WHERE id = ?", params = list(format(Sys.time(), format = "%Y-%m-%dT%H:%M:%OS3Z"), urls_to_crawl$id[i]))

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

          dbExecute(con, "UPDATE Expression SET title = ?, readable = ?, domain_id = ?, description = ?, published_at = ?, approved_at = ?, lang = ?, relevance = ? WHERE id = ?", params = list(url_data$title, url_data$text[1], domain_id, url_data$excerpt, url_data$date, format(Sys.time(), format = "%Y-%m-%dT%H:%M:%OS3Z"), detected_lang, relevance_score, urls_to_crawl$id[i]))

          # Nouvelle fonction pour détecter les liens et les ajouter à la base de données
          detect_links_and_add(con, url_data$text[1], urls_to_crawl$id[i], land_id, urlmax)

          # Placeholder for the detect_links_and_add function
          print(paste("Crawled URL:", urls_to_crawl$url[i]))
        } else {
          print(paste("Both primary and fallback crawls failed for URL:", urls_to_crawl$url[i]))
        }
      }
    }, error = function(e) {
      print(paste("Error processing URL:", urls_to_crawl$url[i], "Error:", e))
    })
  }

  dbDisconnect(con)

  print(paste("Crawling completed for land", land_name))
  return(1)
}

#' Crawl and extract details from a URL
#'
#' This function crawls a URL and extracts detailed information such as meta descriptions,
#' Open Graph descriptions, Twitter descriptions, keywords, and header tags.
#'
#' @param url A character string representing the URL to be crawled.
#' @return A character string containing the extracted details.
#' @import httr XML
#' @export
crawlDetails <- function(url) {
  if (is.null(url) || url == "") {
    return("URL non fournie")
  }

  response <- try(GET(url), silent = TRUE)
  if (inherits(response, "try-error")) {
    return(paste("Erreur lors de la récupération de l'URL:", url))
  }

  content <- content(response, as = "text")
  doc <- htmlParse(content, asText = TRUE)

  result <- ""

  # Fonction d'aide pour obtenir le texte des noeuds HTML
  get_nodes_text <- function(xpath) {
    nodes <- xpathSApply(doc, xpath, xmlValue)
    if (!is.null(nodes)) {
      return(paste(nodes, collapse = " "))
    }
    return("")
  }

  # Fonction d'aide pour obtenir la valeur des attributs
  get_attribute_value <- function(xpath) {
    attributes <- xpathSApply(doc, xpath, xmlGetAttr, "content")
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
  # Établir une connexion à la base de données SQLite
  con <- dbConnect(SQLite(), dbname = db_name)

  # Extraire nburl URL de la table Domain où fetched_at est NULL
  sql_query <- paste0("SELECT * FROM Domain WHERE fetched_at IS NULL LIMIT ", nburl)
  urls_to_crawl <- dbGetQuery(con, sql_query)

  # Parcourir chaque URL et mettre à jour les champs correspondants dans la table Domain
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
        # Mettre à jour les champs de la table Domain
        update_query <- "UPDATE Domain SET title = ?, description = ? WHERE id = ?"
        dbExecute(con, update_query, params = list(
          url_data$title,
          paste(url_data$excerpt, url_datadetail, sep = "\n\n"),
          urls_to_crawl$id[i]
        ))

        message(paste(urls_to_crawl$name[i], "domain crawled"))
      } else {
        # Si crawl échoue ou si les données sont incomplètes, indiquer que le domaine n'a pas pu être crawlé
        message(paste(urls_to_crawl$name[i], "domain could not be crawled due to insufficient data"))
      }
    }, error = function(e) {
      message(paste("Error processing URL:", urls_to_crawl$name[i], "-", e$message))
    })
  }

  # Fermer la connexion
  dbDisconnect(con)

  message("Crawling et mise à jour de la table Domain terminés.")
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

  # Connecter à la base de données SQLite
  con <- dbConnect(RSQLite::SQLite(), db_name)

  # Mettre à NULL la colonne fetched_at où approved_at est NULL
  update_query <- "UPDATE Expression SET fetched_at = NULL WHERE approved_at IS NULL AND fetched_at IS NOT NULL AND land_id = (SELECT id FROM Land WHERE name = ?)"

  dbExecute(con, update_query, params = list(land_name))

  # Fermer la connexion à la base de données
  dbDisconnect(con)


}

#' Check URL availability in Archive.org
#'
#' This function checks if a given URL is available in Archive.org.
#'
#' @param url A character string representing the URL to be checked.
#' @return A list containing the closest snapshot details if available, otherwise NULL.
#' @import httr jsonlite
#' @export
archive_available <- function(url) {
  # Construire l'URL de l'API pour vérifier la disponibilité
  api_url <- paste0("https://archive.org/wayback/available?url=", url)

  # Essayer la requête GET avec gestion des erreurs
  tryCatch({
    response <- GET(api_url)
    if (status_code(response) == 200) {
      content <- content(response, as = "text", encoding = "UTF-8")
      json_content <- jsonlite::fromJSON(content)
      return(json_content$archived_snapshots$closest)
    } else {
      stop("Erreur: Impossible de vérifier la disponibilité de l'URL.")
    }
  }, error = function(e) {
    message("Erreur lors de la requête GET pour vérifier la disponibilité : ", e)
    return(NULL)
  })
}

#' Get the most recent archived URL from Archive.org
#'
#' This function retrieves the most recent archived URL from Archive.org for a given URL.
#'
#' @param url A character string representing the URL to be archived.
#' @return A character string containing the most recent archived URL, or NULL if not available.
#' @import httr jsonlite
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
