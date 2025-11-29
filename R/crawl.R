#' Extract a safe scalar value from a field
#'
#' This helper function extracts a single scalar value from a field that may be
#' NULL, NA, or a vector of length > 1. Used for safe SQL parameter binding.
#'
#' @param x The value to extract from (can be NULL, NA, vector, or scalar).
#' @param default The default value to return if x is NULL or empty (default: NA_character_).
#' @return A single character value safe for SQL binding.
#' @export
safe_scalar <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0) return(default)
  val <- x[1]
  if (is.na(val)) return(default)
  as.character(val)
}

#' Crawl a URL and extract content
#'
#' This function attempts to crawl a given URL using various methods, including
#' mercury-parser, trafilatura, and alternative methods to extract content.
#' It handles different content types, including PDFs.
#'
#' @param url A character string representing the URL to be crawled.
#' @return A data frame containing the extracted content from the URL, including http_status.
#' @import reticulate
#' @export
crawl <- function(url) {
  # Phase 4: Resolve redirects (DOI, short URLs, etc.)
  original_url <- url
  tryCatch({
    head_check <- httr::HEAD(url, httr::timeout(10), httr::config(followlocation = TRUE))
    final_url <- head_check$url
    if (!is.null(final_url) && final_url != url) {
      message("URL redirected: ", url, " -> ", final_url)
      url <- final_url
    }
  }, error = function(e) {
    # Keep original URL if redirect check fails
  })

  # Obtenir le code HTTP status et content-type
  http_status <- NA
  content_type <- NA
  tryCatch({
    head_response <- httr::HEAD(url, httr::timeout(10))
    http_status <- httr::status_code(head_response)
    content_type <- httr::headers(head_response)$`content-type`
  }, error = function(e) {
    # Si HEAD échoue, essayer GET
    tryCatch({
      get_response <- httr::GET(url, httr::timeout(10))
      http_status <<- httr::status_code(get_response)
      content_type <<- httr::http_type(get_response)
    }, error = function(e2) {
      http_status <<- NA
    })
  })

  # Phase 3: Early PDF detection - go directly to PDF extraction
  if (!is.na(content_type) && grepl("application/pdf", content_type, ignore.case = TRUE)) {
    message("PDF detected early, direct extraction")
    pdf_content <- PDFtoText(url)
    if (!is.null(pdf_content) && !grepl("^Download error", pdf_content)) {
      title <- trimws(unlist(strsplit(pdf_content, "\n"))[1])
      excerpt <- substr(pdf_content, nchar(title) + 2, nchar(title) + 651)
      return(data.frame(
        title = title,
        date = "Date inconnue",
        text = trimws(pdf_content),
        excerpt = trimws(excerpt),
        hostname = httr::parse_url(url)$hostname,
        http_status = http_status,
        stringsAsFactors = FALSE
      ))
    }
  }

  # Importation de Trafilatura (réimporté à chaque appel pour éviter les objets invalides entre sessions)
  trafilatura <- reticulate::import("trafilatura", delay_load = FALSE)

  # Double extraction: JSON for metadata + Markdown for text with proper links
  readFull <- tryCatch({
    downloaded <- trafilatura$fetch_url(url)
    if (!is.null(downloaded)) {
      # 1. Extract metadata from JSON
      json_content <- trafilatura$extract(
        filecontent = downloaded,
        url = url,
        include_links = FALSE,
        output_format = "json",
        with_metadata = TRUE
      )

      # 2. Extract text with links in markdown format
      markdown_content <- trafilatura$extract(
        filecontent = downloaded,
        url = url,
        include_links = TRUE,
        output_format = "markdown"
      )

      if (!is.null(json_content) && json_content != "") {
        # Parse metadata from JSON
        readFull <- jsonlite::fromJSON(json_content, simplifyVector = TRUE)

        # Replace text with markdown version (contains [text](url) links)
        if (!is.null(markdown_content) && markdown_content != "") {
          readFull$text <- markdown_content
          message("Extraction avec Trafilatura (JSON metadata + Markdown links)")
        } else {
          message("Extraction avec Trafilatura (JSON only)")
        }
        readFull
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
        downloaded_archive <- trafilatura$fetch_url(urlarchive)
        if (!is.null(downloaded_archive)) {
          # 1. Extract metadata from JSON
          json_content_archive <- trafilatura$extract(
            filecontent = downloaded_archive,
            url = urlarchive,
            include_links = FALSE,
            output_format = "json",
            with_metadata = TRUE
          )

          # 2. Extract text with links in markdown format
          markdown_content_archive <- trafilatura$extract(
            filecontent = downloaded_archive,
            url = urlarchive,
            include_links = TRUE,
            output_format = "markdown"
          )

          if (!is.null(json_content_archive) && json_content_archive != "") {
            readFull_archive <- jsonlite::fromJSON(json_content_archive, simplifyVector = TRUE)

            # Filter out "Wayback Machine" error pages from Archive.org
            if (!is.null(readFull_archive$title) &&
                grepl("^Wayback Machine", readFull_archive$title, ignore.case = TRUE)) {
              message("Archive.org returned error page (Wayback Machine), skipping")
              readFull_archive <- NULL
            } else {
              # Replace text with markdown version (contains [text](url) links)
              if (!is.null(markdown_content_archive) && markdown_content_archive != "") {
                readFull_archive$text <- markdown_content_archive
                message("Extraction avec Archive.org (JSON metadata + Markdown links)")
              } else {
                message("Extraction avec Archive.org (JSON only)")
              }
              readFull <- readFull_archive
            }
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
    result_df <- as.data.frame(t(unlist(readFull)))
    result_df$http_status <- http_status
    return(result_df)
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
          http_status = http_status,
          stringsAsFactors = FALSE
        ))
      }
    } else {
      # Extraction HTML classique avec xml2 (équivalent BeautifulSoup)
      tryCatch({
        message("Tentative avec GET HTML (xml2)")
        parsed_content <- xml2::read_html(httr::content(response, as = "text"))

        # Extract metadata with multiple fallback strategies
        metadata <- extract_html_metadata(parsed_content, response, url)

        # Extract body text
        body_node <- xml2::xml_find_first(parsed_content, "//body")
        body_text <- xml2::xml_text(body_node)

        # Extract links and format as markdown [text](url)
        link_nodes <- xml2::xml_find_all(parsed_content, "//a[@href]")
        if (length(link_nodes) > 0) {
          links_md <- sapply(link_nodes, function(node) {
            href <- xml2::xml_attr(node, "href")
            text <- trimws(xml2::xml_text(node))
            # Only process http/https links
            if (!is.na(href) && grepl("^https?://", href) && nchar(text) > 0) {
              paste0("[", text, "](", href, ")")
            } else {
              NULL
            }
          })
          links_md <- links_md[!sapply(links_md, is.null)]
          # Append links section to body text
          if (length(links_md) > 0) {
            body_text <- paste0(body_text, "\n\n## Links\n", paste(links_md, collapse = "\n"))
          }
        }

        message("Extraction avec GET HTML (xml2) - metadata: title=", !is.na(metadata$title),
                ", date=", metadata$date != "Date non disponible",
                ", excerpt=", !is.na(metadata$excerpt),
                ", author=", !is.na(metadata$author))

        return(data.frame(
          title = metadata$title,
          date = metadata$date,
          text = body_text,
          excerpt = metadata$excerpt,
          author = metadata$author,
          sitename = metadata$sitename,
          hostname = metadata$hostname,
          http_status = http_status,
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



#' Extract metadata from HTML with multiple fallback strategies
#'
#' This function extracts metadata from parsed HTML content using multiple
#' sources in priority order: standard meta tags, Open Graph, Twitter Cards,
#' Schema.org JSON-LD, and HTTP headers.
#'
#' @param parsed_content xml2 parsed HTML document
#' @param response httr response object (for HTTP headers fallback)
#' @param url The original URL (for hostname extraction)
#' @return A list with title, date, excerpt, author, hostname
#' @keywords internal
extract_html_metadata <- function(parsed_content, response, url) {

  # Helper to get first non-empty value from multiple XPath queries
  get_first_match <- function(xpaths) {
    for (xpath in xpaths) {
      node <- xml2::xml_find_first(parsed_content, xpath)
      if (!is.na(node)) {
        value <- xml2::xml_text(node)
        if (!is.na(value) && nchar(trimws(value)) > 0) {
          return(trimws(value))
        }
        # Try content attribute for meta tags
        content <- xml2::xml_attr(node, "content")
        if (!is.na(content) && nchar(trimws(content)) > 0) {
          return(trimws(content))
        }
      }
    }
    return(NA_character_)
  }

  # ===== TITLE =====
  # Priority: og:title > twitter:title > <title> > schema name
  title <- get_first_match(c(
    "//meta[@property='og:title']",
    "//meta[@name='twitter:title']",
    "//title",
    "//meta[@itemprop='name']",
    "//h1"
  ))

  # ===== DATE =====
  # Priority: article:published_time > og:published_time > datePublished >
  #           meta date > time[datetime] > Last-Modified header
  date <- get_first_match(c(
    "//meta[@property='article:published_time']",
    "//meta[@property='og:published_time']",
    "//meta[@itemprop='datePublished']",
    "//meta[@name='date']",
    "//meta[@name='DC.date']",
    "//meta[@name='pubdate']",
    "//meta[@name='publishdate']",
    "//time[@datetime]/@datetime",
    "//time[@pubdate]/@datetime"
  ))
  # Fallback to HTTP Last-Modified header
  if (is.na(date) && !is.null(response)) {
    last_modified <- httr::headers(response)$`last-modified`
    if (!is.null(last_modified) && nchar(last_modified) > 0) {
      date <- last_modified
    }
  }
  if (is.na(date)) date <- "Date non disponible"

  # ===== EXCERPT / DESCRIPTION =====
  # Priority: meta description > og:description > twitter:description > schema description
  excerpt <- get_first_match(c(
    "//meta[@name='description']",
    "//meta[@property='og:description']",
    "//meta[@name='twitter:description']",
    "//meta[@itemprop='description']",
    "//meta[@name='DC.description']"
  ))

  # ===== AUTHOR =====
  # Priority: meta author > article:author > og:author > DC.creator > schema author
  author <- get_first_match(c(
    "//meta[@name='author']",
    "//meta[@property='article:author']",
    "//meta[@property='og:author']",
    "//meta[@name='DC.creator']",
    "//meta[@itemprop='author']",
    "//a[@rel='author']",
    "//span[@itemprop='author']"
  ))

  # ===== SITENAME =====
  sitename <- get_first_match(c(
    "//meta[@property='og:site_name']",
    "//meta[@name='application-name']"
  ))

  # ===== HOSTNAME =====
  hostname <- httr::parse_url(url)$hostname

  # ===== TRY JSON-LD SCHEMA.ORG =====
  # Look for structured data in <script type="application/ld+json">
  tryCatch({
    jsonld_nodes <- xml2::xml_find_all(parsed_content, "//script[@type='application/ld+json']")
    if (length(jsonld_nodes) > 0) {
      for (node in jsonld_nodes) {
        jsonld_text <- xml2::xml_text(node)
        if (!is.na(jsonld_text) && nchar(jsonld_text) > 0) {
          jsonld <- tryCatch(jsonlite::fromJSON(jsonld_text, simplifyVector = TRUE), error = function(e) NULL)
          if (!is.null(jsonld)) {
            # Handle @graph structure
            if (!is.null(jsonld$`@graph`)) {
              for (item in jsonld$`@graph`) {
                if (is.na(title) && !is.null(item$headline)) title <- item$headline
                if (is.na(title) && !is.null(item$name)) title <- item$name
                if (is.na(date) || date == "Date non disponible") {
                  if (!is.null(item$datePublished)) date <- item$datePublished
                }
                if (is.na(excerpt) && !is.null(item$description)) excerpt <- item$description
                if (is.na(author)) {
                  if (!is.null(item$author$name)) author <- item$author$name
                  else if (is.character(item$author)) author <- item$author
                }
              }
            } else {
              # Direct structure
              if (is.na(title) && !is.null(jsonld$headline)) title <- jsonld$headline
              if (is.na(title) && !is.null(jsonld$name)) title <- jsonld$name
              if ((is.na(date) || date == "Date non disponible") && !is.null(jsonld$datePublished)) {
                date <- jsonld$datePublished
              }
              if (is.na(excerpt) && !is.null(jsonld$description)) excerpt <- jsonld$description
              if (is.na(author)) {
                if (!is.null(jsonld$author$name)) author <- jsonld$author$name
                else if (is.character(jsonld$author)) author <- jsonld$author
              }
            }
          }
        }
      }
    }
  }, error = function(e) {
    # JSON-LD parsing failed, continue with what we have
  })

  return(list(
    title = title,
    date = date,
    excerpt = excerpt,
    author = author,
    sitename = sitename,
    hostname = hostname
  ))
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
  # Ensure temp file is cleaned up when function exits (Phase 6: prevent disk leakage)
  on.exit(unlink(temp_file), add = TRUE)

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


#' Validate and normalize URL
#'
#' This function validates a URL to ensure it's complete and well-formed.
#' Rejects truncated URLs like "https://www." or "https://example."
#'
#' @param url A character string representing the URL to validate.
#' @return The validated URL or NULL if invalid.
#' @importFrom stringr str_detect
#' @export
validate_url <- function(url) {
  if (is.na(url) || is.null(url) || url == "") return(NULL)

  # Reject URLs too short (minimum: https://a.bc = 12 chars)
  if (nchar(url) < 12) return(NULL)

  # Must start with http:// or https://
 if (!grepl("^https?://", url)) return(NULL)

  # Reject truncated URLs ending with just a dot or incomplete domain
  if (grepl("^https?://[^/]*\\.$", url)) return(NULL)
  if (grepl("^https?://www\\.?$", url)) return(NULL)
  if (grepl("^https?://[^./]+$", url)) return(NULL)

  # Must have a valid TLD (at least 2 characters after last dot before path)
  # Extract the domain part
  domain_match <- regmatches(url, regexpr("^https?://([^/]+)", url))
  if (length(domain_match) == 0) return(NULL)

  domain <- sub("^https?://", "", domain_match)
  # Remove port if present
  domain <- sub(":[0-9]+$", "", domain)

  # Check for valid TLD (must have at least one dot with 2+ chars after it)
  if (!grepl("\\.[a-zA-Z]{2,}$", domain) && !grepl("\\.[a-zA-Z]{2,}[:/]", url)) {
    return(NULL)
  }

  return(url)
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
  url <- str_extract(url, "https?://[A-Za-z0-9-._~:/?#\\[\\]@!$&'()*+,;=%]+")

  return(url)
}

#' Check if a link is a media link
#'
#' This function checks if a given link points to a media file based on its extension.
#' Handles URLs with query strings (e.g., image.jpg?size=large) and is case-insensitive.
#'
#' @param link A character string representing the link to be checked.
#' @return A logical value indicating whether the link is a media link.
#' @importFrom stringr str_detect
#' @export
is_media_link <- function(link) {
  if (is.na(link) || is.null(link) || link == "") {
    return(FALSE)
  }

  # Media extensions - case insensitive, handles query strings and fragments
  # Images
  image_ext <- c("jpg", "jpeg", "png", "gif", "bmp", "webp", "svg", "ico", "tiff", "tif")
  # Documents
  doc_ext <- c("pdf", "txt", "csv", "xls", "xlsx", "doc", "docx", "ppt", "pptx", "odt", "ods", "odp", "rtf")
  # Audio
  audio_ext <- c("mp3", "wav", "ogg", "flac", "aac", "wma", "m4a")
  # Video
  video_ext <- c("mp4", "avi", "mov", "mkv", "webm", "wmv", "flv", "m4v", "mpeg", "mpg")
  # Archives
  archive_ext <- c("zip", "rar", "tar", "gz", "7z", "bz2")

  all_extensions <- c(image_ext, doc_ext, audio_ext, video_ext, archive_ext)

  # Build regex pattern: matches .ext at end of path (before ? or # or end of string)
  # Case insensitive
  pattern <- paste0("\\.(?i)(", paste(all_extensions, collapse = "|"), ")(?=[?#]|$)")

  return(grepl(pattern, link, perl = TRUE))
}

#' Detect links in content and add to database
#'
#' This function extracts links from content, cleans them, validates them,
#' and adds them to the database. It handles both media and non-media links.
#' Uses multiple extraction strategies: markdown links, then raw URL regex.
#'
#' @param con A database connection object.
#' @param content A character string containing the content to be parsed for links.
#' @param parent_id An integer representing the ID of the parent record in the database.
#' @param land_id An integer representing the ID of the land associated with the links.
#' @param urlmax An integer specifying the maximum number of URLs to be processed.
#' @import DBI
#' @importFrom stringr str_extract_all str_extract str_match_all
#' @export
detect_links_and_add <- function(con, content, parent_id, land_id, urlmax=50) {
  if (is.null(content) || is.na(content) || content == "") {
    return(invisible(NULL))
  }

  # Strategy 1: Extract markdown-style links [text](url)
  markdown_matches <- str_match_all(content, "\\[([^\\]]+)\\]\\(([^\\)]+)\\)")[[1]]
  markdown_urls <- character(0)
  if (nrow(markdown_matches) > 0) {
    markdown_urls <- markdown_matches[, 3]  # Column 3 contains the URLs
    markdown_urls <- markdown_urls[grepl("^https?://", markdown_urls)]
  }

  # Strategy 2: Improved regex for raw URLs
  # This regex captures URLs more completely, including query strings and paths
  raw_url_pattern <- "https?://[A-Za-z0-9][-A-Za-z0-9+&@#/%?=~_|!:,.;]*[-A-Za-z0-9+&@#/%=~_|]"
  raw_urls <- str_extract_all(content, raw_url_pattern)[[1]]

  # Combine both strategies, prioritizing markdown URLs
  all_links <- unique(c(markdown_urls, raw_urls))

  # Counter for non-media links
  non_media_count <- 0
  added_count <- 0

  for (link in all_links) {
    # If the counter reaches urlmax, exit the loop
    if (non_media_count >= urlmax) {
      break
    }

    # Clean the extracted link
    clean_link <- clean_url(link)

    # Skip if cleaning failed
    if (is.na(clean_link) || is.null(clean_link)) {
      next
    }

    # Validate the URL to reject truncated URLs
    validated_link <- validate_url(clean_link)
    if (is.null(validated_link)) {
      message("Rejected invalid/truncated URL: ", clean_link)
      next
    }

    if (is_media_link(validated_link)) {
      # Retrieve the media extension (without the dot, lowercase)
      media_match <- regmatches(validated_link, regexpr("\\.([a-zA-Z0-9]+)(?=[?#]|$)", validated_link, perl = TRUE))
      media_type <- if (length(media_match) > 0) {
        tolower(sub("^\\.", "", media_match))  # Remove leading dot and lowercase
      } else {
        NA_character_
      }

      # Add to the Media table
      tryCatch({
        dbExecute(con, "INSERT INTO Media (url, expression_id, type) VALUES (?, ?, ?)",
                  params = list(validated_link, parent_id, media_type))
      }, error = function(e) {
        # Ignore duplicate errors
      })
    } else {
      # Increment the counter for non-media links
      non_media_count <- non_media_count + 1

      res <- dbGetQuery(con, "SELECT * FROM Expression WHERE url = ?", params = list(validated_link))

      if (nrow(res) == 0) {
        parent_depth <- as.integer(dbGetQuery(con, "SELECT depth FROM Expression WHERE id = ?", params = list(parent_id)))

        # Insert the new URL into the "Expression" table
        dbExecute(con, "INSERT INTO Expression (url, land_id, created_at, depth) VALUES (?, ?, ?, ?)",
                  params = list(validated_link, land_id, format(Sys.time(), format = "%Y-%m-%dT%H:%M:%OS3Z"), parent_depth + 1))

        new_url_id <- dbGetQuery(con, "SELECT last_insert_rowid()")[1, 1]
        added_count <- added_count + 1
      } else {
        new_url_id <- res$id[1]
      }

      # Check if the link already exists in ExpressionLink
      link_exists <- dbGetQuery(con, "SELECT * FROM ExpressionLink WHERE source_id = ? AND target_id = ?",
                                params = list(parent_id, new_url_id))

      if (nrow(link_exists) == 0) {
        # Insert the link into the "ExpressionLink" table
        dbExecute(con, "INSERT INTO ExpressionLink (source_id, target_id) VALUES (?, ?)",
                  params = list(parent_id, new_url_id))
      }
    }
  }

  if (added_count > 0) {
    message("Added ", added_count, " new URLs from content")
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

  # Vérifier qu'il y a des URLs à crawler

  if (nrow(urls_to_crawl) == 0) {
    message("No URLs to crawl for land ", land_name)
    dbCommit(con)
    dbDisconnect(con)
    return(0)
  }

  timestamp_now <- format(Sys.time(), format = "%Y-%m-%dT%H:%M:%OS3Z")
  for (i in 1:nrow(urls_to_crawl)) {
    # Vérifier que l'URL n'est pas NA ou vide
    current_url <- urls_to_crawl$url[i]
    if (is.na(current_url) || current_url == "") {
      message("Skipping invalid URL (NA or empty) at row ", i)
      next
    }

    # Validate URL structure before crawling
    validated_url <- validate_url(current_url)
    if (is.null(validated_url)) {
      message("Skipping invalid/malformed URL: ", current_url)
      next
    }
    current_url <- validated_url

    tryCatch({
      message("start : ", current_url)
      dbExecute(con, "UPDATE Expression SET fetched_at = ? WHERE id = ?", params = list(timestamp_now, urls_to_crawl$id[i]))

      url_data <- crawl(current_url)

      relevance_score <- expression_relevance(dictionary$lemma, url_data, language)

      if (relevance_score > 0) {
        if (!is.null(url_data$hostname)) {
          existing_domain <- dbGetQuery(con, "SELECT id, name FROM Domain WHERE name = ?", params = list(url_data$hostname))
          detected_lang <- cld3::detect_language(as.character(url_data$text[1]))

          if (nrow(existing_domain) == 0) {
            dbExecute(con, "INSERT INTO Domain (name) VALUES (?)", params = list(url_data$hostname))
            domain_id <- dbGetQuery(con, "SELECT last_insert_rowid()")[1, 1]
          } else {
            domain_id <- existing_domain$id[1]
          }

          # Récupérer http_status du résultat du crawl
          crawl_http_status <- if (!is.null(url_data$http_status)) url_data$http_status[1] else NA

          # Use safe_scalar to ensure all params are single values (fixes "Parameter X does not have length 1" error)
          dbExecute(con, "UPDATE Expression SET title = ?, readable = ?, domain_id = ?, description = ?, published_at = ?, approved_at = ?, lang = ?, relevance = ?, http_status = ? WHERE id = ?",
                    params = list(
                      safe_scalar(url_data$title),
                      safe_scalar(url_data$text),
                      domain_id,
                      safe_scalar(url_data$excerpt),
                      safe_scalar(url_data$date),
                      timestamp_now,
                      safe_scalar(detected_lang),
                      relevance_score,
                      crawl_http_status,
                      urls_to_crawl$id[i]
                    ))

          # add links to data base
          detect_links_and_add(con, url_data$text[1], urls_to_crawl$id[i], land_id, urlmax)

          # Placeholder for the detect_links_and_add function
          message("Crawled URL: ", current_url)
        } else {
          message("Both primary and fallback crawls failed for URL: ", current_url)
        }
      }
    }, error = function(e) {
      message("Error processing URL: ", current_url, " Error: ", e$message)
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

  rows_affected <- dbExecute(con, update_query, params = list(land_name))

  # Close the database connection
  dbDisconnect(con)

  message("Reset ", rows_affected, " URLs for re-crawling in land '", land_name, "'")
  return(invisible(rows_affected))
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
