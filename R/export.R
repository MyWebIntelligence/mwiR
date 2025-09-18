#' Establish a Database Connection
#'
#' This function establishes a connection to the specified SQLite database.
#'
#' @param db_name A character string specifying the name of the database file. Default is "mwi.db".
#' @return A database connection object.
#' @import RSQLite
#' @export

connect_db <- function(db_name = "mwi.db") {
  con <- dbConnect(RSQLite::SQLite(), dbname = db_name)
  return(con)
}

#' Get Land ID
#'
#' Utility function returning the numeric id of a land, or stops with an explicit error
#' if the land name does not exist in the database.
#'
#' @param con A database connection object.
#' @param land_name A character string specifying the land name.
#' @return An integer scalar containing the land id.
#' @keywords internal
#' @import DBI
get_land_id <- function(con, land_name) {
  land_id <- dbGetQuery(con,
                        "SELECT id FROM Land WHERE name = ?",
                        params = list(land_name))$id
  if (length(land_id) == 0 || is.na(land_id)) {
    stop("The specified land does not exist.")
  }
  return(land_id)
}

#' Export Page CSV
#'
#' This function exports data from the specified land to a CSV file based on a minimum relevance score.
#'
#' @param con A database connection object.
#' @param land_name A character string specifying the name of the land.
#' @param minimum_relevance A numeric value specifying the minimum relevance score for inclusion in the export.
#' @param filename A character string specifying the name of the output CSV file.
#' @return An integer indicating the number of rows written to the CSV file.
#' @import DBI
#' @export
export_pagecsv <- function(con, land_name, minimum_relevance, filename) {
  # Retrieve the land ID from the name
  land_id <- get_land_id(con, land_name)

  # Define the column mapping and SQL query
  pagecsv_col_map <- list(
    'id' = 'e.id',
    'url' = 'e.url',
    'title' = 'e.title',
    'description' = 'e.description',
    'keywords' = 'e.keywords',
    'relevance' = 'e.relevance',
    'depth' = 'e.depth',
    'published_at' = 'e.published_at',
    'domain_id' = 'e.domain_id',
    'domain_name' = 'd.name',
    'domain_description' = 'd.description',
    'domain_keywords' = 'd.keywords',
    'tags' = 'GROUP_CONCAT(DISTINCT t.name)'
  )

  pagecsv_sql <- "
  SELECT
      %s
  FROM expression AS e
  JOIN domain AS d ON d.id = e.domain_id
  LEFT JOIN taggedcontent tc ON tc.expression_id = e.id
  LEFT JOIN tag t ON t.id = tc.tag_id
  WHERE e.land_id = ? AND e.relevance >= ?
  GROUP BY e.id
  "

  # Write the file
  cols <- paste0(sapply(names(pagecsv_col_map), function(x) paste0(pagecsv_col_map[[x]], " AS ", x)), collapse = ", ")
  full_sql <- sprintf(pagecsv_sql, cols)
  result <- dbGetQuery(con, full_sql, params = list(land_id, minimum_relevance))

  data.table::fwrite(result, file = filename)

  return(nrow(result))
}



#' Export Full Page CSV
#'
#' This function exports detailed data from the specified land to a CSV file based on a minimum relevance score.
#'
#' @param con A database connection object.
#' @param land_name A character string specifying the name of the land.
#' @param minimum_relevance A numeric value specifying the minimum relevance score for inclusion in the export.
#' @param filename A character string specifying the name of the output CSV file.
#' @return An integer indicating the number of rows written to the CSV file.
#' @import DBI
#' @export
export_fullpagecsv <- function(con, land_name, minimum_relevance, filename) {
  # Retrieve the land ID from the name
  land_id <- get_land_id(con, land_name)

  # Define the column mapping and SQL query
  fullpagecsv_col_map <- list(
    'id' = 'e.id',
    'url' = 'e.url',
    'title' = 'e.title',
    'description' = 'e.description',
    'keywords' = 'e.keywords',
    'readable' = 'e.readable',
    'relevance' = 'e.relevance',
    'depth' = 'e.depth',
    'published_at' = 'e.published_at',
    'domain_id' = 'e.domain_id',
    'domain_name' = 'd.name',
    'domain_description' = 'd.description',
    'domain_keywords' = 'd.keywords',
    'tags' = 'GROUP_CONCAT(DISTINCT t.name)'
  )

  fullpagecsv_sql <- "
  SELECT
      %s
  FROM expression AS e
  JOIN domain AS d ON d.id = e.domain_id
  LEFT JOIN taggedcontent tc ON tc.expression_id = e.id
  LEFT JOIN tag t ON t.id = tc.tag_id
  WHERE e.land_id = ? AND e.relevance >= ?
  GROUP BY e.id
  "

  # Write the file
  cols <- paste0(sapply(names(fullpagecsv_col_map), function(x) paste0(fullpagecsv_col_map[[x]], " AS ", x)), collapse = ", ")
  full_sql <- sprintf(fullpagecsv_sql, cols)
  result <- dbGetQuery(con, full_sql, params = list(land_id, minimum_relevance))

  data.table::fwrite(result, file = filename)

  return(nrow(result))
}


#' Export Node CSV
#'
#' This function exports domain data to a CSV file, including the number of expressions and average relevance score.
#'
#' @param con A database connection object.
#' @param land_name A character string specifying the name of the land.
#' @param minimum_relevance A numeric value specifying the minimum relevance score for inclusion in the export.
#' @param filename A character string specifying the name of the output CSV file.
#' @return An integer indicating the number of rows written to the CSV file.
#' @import DBI
#' @export
export_nodecsv <- function(con, land_name, minimum_relevance, filename) {
  # Retrieve the land ID from the name
  land_id <- get_land_id(con, land_name)

  # Define the column mapping and SQL query
  nodecsv_col_map <- list(
    'id' = 'd.id',
    'name' = 'd.name',
    'title' = 'd.title',
    'description' = 'd.description',
    'keywords' = 'd.keywords',
    'expressions' = 'COUNT(*)',
    'average_relevance' = 'ROUND(AVG(e.relevance), 2)'
  )

  nodecsv_sql <- "
  SELECT
      %s
  FROM domain AS d
  JOIN expression AS e ON e.domain_id = d.id
  WHERE land_id = ? AND e.relevance >= ?
  GROUP BY d.id
  "

  # Write the file
  cols <- paste0(sapply(names(nodecsv_col_map), function(x) paste0(nodecsv_col_map[[x]], " AS ", x)), collapse = ", ")
  full_sql <- sprintf(nodecsv_sql, cols)
  result <- dbGetQuery(con, full_sql, params = list(land_id, minimum_relevance))

  data.table::fwrite(result, file = filename)

  return(nrow(result))
}


#' Export Media CSV
#'
#' This function exports media data related to expressions in the specified land to a CSV file based on a minimum relevance score.
#'
#' @param con A database connection object.
#' @param land_name A character string specifying the name of the land.
#' @param minimum_relevance A numeric value specifying the minimum relevance score for inclusion in the export.
#' @param filename A character string specifying the name of the output CSV file.
#' @return An integer indicating the number of rows written to the CSV file.
#' @import DBI
#' @export
export_mediacsv <- function(con, land_name, minimum_relevance, filename) {
  # Retrieve the land ID from the name
  land_id <- get_land_id(con, land_name)

  # Define the column mapping and SQL query
  mediacsv_col_map <- list(
    'id' = 'm.id',
    'expression_id' = 'm.expression_id',
    'url' = 'm.url',
    'type' = 'm.type'
  )

  mediacsv_sql <- "
  SELECT
      %s
  FROM media AS m
  JOIN expression AS e ON e.id = m.expression_id
  WHERE e.land_id = ? AND e.relevance >= ?
  GROUP BY m.id
  "

  # Write the file
  cols <- paste0(sapply(names(mediacsv_col_map), function(x) paste0(mediacsv_col_map[[x]], " AS ", x)), collapse = ", ")
  full_sql <- sprintf(mediacsv_sql, cols)
  result <- dbGetQuery(con, full_sql, params = list(land_id, minimum_relevance))

  data.table::fwrite(result, file = filename)

  return(nrow(result))
}


#' Clean String
#'
#' This function removes control characters, newlines, and tabs from a string.
#'
#' @param str A character string to be cleaned.
#' @return A cleaned character string.
#' @export
clean_string <- function(str) {
  cleaned_str <- gsub("[\r\n\t]", "", str)  # Remove line breaks, tabs

  cleaned_str <- gsub("[[:cntrl:]]", "", cleaned_str)  # Remove other control characters
  return(cleaned_str)
}


#' Export Page GEXF
#'
#' This function exports expression data from the specified land to a GEXF file for network analysis.
#'
#' @param con A database connection object.
#' @param land_name A character string specifying the name of the land.
#' @param minimum_relevance A numeric value specifying the minimum relevance score for inclusion in the export.
#' @param filename A character string specifying the name of the output GEXF file.
#' @import DBI
#' @importFrom XML newXMLNode saveXML
#' @export
export_pagegexf <- function(con, land_name, minimum_relevance, filename) {
  # Retrieve the land ID from the name
  land_id <- get_land_id(con, land_name)

  # Define GEXF attributes without undesired fields
  gexf_attributes <- c(
    'id' = 'e.id',
    'url' = 'e.url',
    'title' = 'e.title',
    'relevance' = 'e.relevance',
    'depth' = 'e.depth',
    'published_at' = 'e.published_at',
    'domain_id' = 'e.domain_id',
    'domain_name' = 'd.name'
  )

  # Initialize the GEXF file
  gexf <- newXMLNode("gexf", namespaceDefinitions=c(gexf="http://www.gexf.net/1.2draft"), attrs=c(version="1.2"))
  graph <- newXMLNode("graph", attrs=c(mode="static", defaultedgetype="directed"), parent=gexf)
  nodes <- newXMLNode("nodes", parent=graph)
  edges <- newXMLNode("edges", parent=graph)

  # Construct the SQL query for nodes excluding undesired fields
  cols <- paste0(sapply(names(gexf_attributes), function(x) paste0(gexf_attributes[[x]], " AS ", x)), collapse = ", ")
  node_sql <- sprintf("
    SELECT
        %s
    FROM expression AS e
    JOIN domain AS d ON d.id = e.domain_id
    WHERE e.land_id = ? AND e.relevance >= ?
    GROUP BY e.id
  ", cols)

  nodes_result <- dbGetQuery(con, node_sql, params = list(land_id, minimum_relevance))

  # Clean the 'title' field of control characters
  nodes_result$title <- gsub("[[:cntrl:]]", "", nodes_result$title)

  # Add nodes to the GEXF file
  for(i in 1:nrow(nodes_result)) {
    node <- newXMLNode("node", attrs=c(id=nodes_result$id[i], label=nodes_result$url[i]), parent=nodes)
    attributes <- newXMLNode("attributes", parent=node)
    for(attr_name in names(gexf_attributes)) {
      # Condition to create an attribute node only if the column exists in the result
      if(attr_name %in% colnames(nodes_result)) {
        newXMLNode("attribute", attrs=c(id=attr_name, title=attr_name, type="string", value=nodes_result[[attr_name]][i]), parent=attributes)
      }
    }
  }

  # SQL query to retrieve edges
  edges_sql <- "
  WITH idx(x) AS (
    SELECT
      id
    FROM expression
    WHERE land_id = ? AND relevance >= ?
  )
  SELECT
    link.source_id, e1.domain_id AS source_domain_id, link.target_id, e2.domain_id AS target_domain_id
  FROM expressionlink AS link
  JOIN expression AS e1 ON e1.id = link.source_id
  JOIN expression AS e2 ON e2.id = link.target_id
  WHERE
    source_id IN idx
    AND target_id IN idx
    AND source_domain_id != target_domain_id
  "

  edges_result <- dbGetQuery(con, edges_sql, params = list(land_id, minimum_relevance))

  # Add edges to the GEXF file
  for(i in 1:nrow(edges_result)) {
    newXMLNode("edge", attrs=c(id=paste0("e", i), source=edges_result$source_id[i], target=edges_result$target_id[i], weight=1), parent=edges)
  }

  # Write the GEXF file
  saveXML(gexf, file=filename)
}


#' Convert Date String to Date Object
#'
#' This function attempts to convert a date string into a Date object using multiple formats.
#'
#' @param date_str A character string representing the date.
#' @return A Date object, or NA if conversion fails.
#' @export
convert_date <- function(date_str) {
  formats <- c("%Y-%m-%dT%H:%M:%S.000Z", "%a, %d %b %Y %H:%M:%S GMT")

  for (fmt in formats) {
    tryCatch({
      date <- as.Date(as.POSIXct(date_str, format=fmt, tz="GMT"), origin="1970-1-1")
      if (!is.na(date)) {
        return(date)
      }
    }, error = function(e) NULL) # If an error occurs, do nothing and try the next format
  }
  return(NA)  # Return NA if no match is found or if an error occurs
}


#' Export Node GEXF
#'
#' This function exports domain data from the specified land to a GEXF file for network analysis.
#'
#' @param con A database connection object.
#' @param land_name A character string specifying the name of the land.
#' @param minimum_relevance A numeric value specifying the minimum relevance score for inclusion in the export.
#' @param filename A character string specifying the name of the output GEXF file.
#' @import DBI
#' @importFrom XML newXMLNode saveXML
#' @export
export_nodegexf <- function(con, land_name, minimum_relevance, filename) {
  # Retrieve the land ID from the name
  land_id <- get_land_id(con, land_name)

  # Define GEXF attributes
  gexf_attributes <- list(
    'name' = 'string',
    'description' = 'string',
    'keywords' = 'string',
    'nbexpressions' = 'integer',
    'average_relevance' = 'float',
    'first_expression_date' = 'string',
    'last_expression_date' = 'string'
  )

  # Initialize the GEXF file
  gexf <- newXMLNode("gexf", namespaceDefinitions = c(gexf = "http://www.gexf.net/1.3"), attrs = c(version = "1.3"))
  graph <- newXMLNode("graph", attrs = c(mode = "dynamic", defaultedgetype = "directed", timeformat="datetime"), parent = gexf)

  # Declare graph attributes
  attributes_graph <- newXMLNode("attributes", attrs = c(class = "node"), parent = graph)
  for (attr_name in names(gexf_attributes)) {
    newXMLNode("attribute", attrs = c(id = attr_name, title = attr_name, type = gexf_attributes[[attr_name]]), parent = attributes_graph)
  }
  nodes <- newXMLNode("nodes", parent = graph)
  edges <- newXMLNode("edges", parent = graph)

  # SQL query to retrieve nodes
  node_sql <- "
  SELECT
      d.id, d.name, d.title, d.description, d.keywords, COUNT(*) as nbexpressions, ROUND(AVG(e.relevance), 2) as average_relevance,
      MIN(e.published_at) as first_expression_date, MAX(e.published_at) as last_expression_date
  FROM domain AS d
  JOIN expression AS e ON e.domain_id = d.id
  WHERE e.land_id = ? AND e.relevance >= ?
  GROUP BY d.name
  "

  nodes_result <- dbGetQuery(con, node_sql, params = list(land_id, minimum_relevance))

  # Convert the character strings first_expression_date and last_expression_date to POSIXct objects
  nodes_result$first_expression_date <- as.Date(sapply(nodes_result$first_expression_date, convert_date), origin="1970-1-1")
  nodes_result$last_expression_date <- as.Date(sapply(nodes_result$last_expression_date, convert_date), origin="1970-1-1") + 3

  nodes_result <- as.data.frame(lapply(nodes_result, clean_string))

  # Add nodes to the GEXF file
  for (i in 1:nrow(nodes_result)) {
    node <- newXMLNode("node", attrs = c(id = nodes_result$id[i], label = nodes_result$name[i], start = nodes_result$first_expression_date[i], end = nodes_result$last_expression_date[i]), parent = nodes)
    attvalues <- newXMLNode("attvalues", parent = node)
    for (attr_name in names(gexf_attributes)) {
      newXMLNode("attvalue", attrs = c("for" = attr_name, value = nodes_result[[attr_name]][i]), parent = attvalues)
    }
  }

  # SQL query to retrieve edges
  edges_sql <- "
  WITH idx(x) AS (
    SELECT
      id
    FROM expression
    WHERE land_id = ? AND relevance >= ?
  )
  SELECT
    link.source_id, e1.domain_id AS source_domain_id, link.target_id, e2.domain_id AS target_domain_id, COUNT(*)
  FROM expressionlink AS link
  JOIN expression AS e1 ON e1.id = link.source_id
  JOIN expression AS e2 ON e2.id = link.target_id
  WHERE
    source_id IN idx
    AND target_id IN idx
    AND source_domain_id != target_domain_id
  GROUP BY source_domain_id, target_domain_id
  "

  edges_result <- dbGetQuery(con, edges_sql, params = list(land_id, minimum_relevance))

  edges_result <- as.data.frame(lapply(edges_result, clean_string))

  # Add edges to the GEXF file
  for (i in 1:nrow(edges_result)) {
    newXMLNode("edge", attrs = c(id = paste0("e", i), source = edges_result$source_domain_id[i], target = edges_result$target_domain_id[i], weight = edges_result$COUNT[i]), parent = edges)
  }

  # Write the GEXF file
  saveXML(gexf, file = filename)
}


#' Slugify a String
#'
#' This function creates a URL-friendly slug from a given string.
#'
#' @param string A character string to be slugified.
#' @return A slugified character string.
#' @export
slugify <- function(string) {
  slug <- stringi::stri_trans_general(string, "Latin-ASCII")
  slug <- tolower(slug)
  slug <- gsub("[^a-z0-9]+", "-", slug)
  slug <- gsub("^-|-$", "", slug)
  return(slug)
}

#' Convert to Metadata
#'
#' This function creates a metadata string from the given data frame row.
#'
#' @param lignes A data frame row containing the fields for metadata.
#' @return A character string containing the metadata.
#' @export
to_metadata <- function(lignes) {
  metadata <- sprintf("---\n
Title: \"%s\"\n
Creator: \"\"\n
Contributor: \"\"\n
Coverage: \"\"\n
Date: \"\"\n
Description: \"%s\"\n
Subject: \"\"\n
Type: \"\"\n
Format: \"\"\n
Identifier: \"%s\"\n
Language: \"\"\n
Publisher: \"%s\"\n
Relation: \"\"\n
Rights: \"\"\n
Source: \"%s\"\n
---\n", gsub("-", "", gsub("\"", "_", lignes$title)) , gsub("-", "", gsub("\"", "_", lignes$description)), lignes$id, lignes$name, lignes$url)
  return(metadata)
}

#' Export Corpus
#'
#' This function exports the corpus data from the specified land, including metadata, to text files and compresses them into a zip file.
#'
#' @param con A database connection object.
#' @param land_name A character string specifying the name of the land.
#' @param minimum_relevance A numeric value specifying the minimum relevance score for inclusion in the export.
#' @import DBI utils
#' @export
export_corpus <- function(con, land_name, minimum_relevance) {
  # Retrieve the land ID from the name
  land_id <- get_land_id(con, land_name)

  # Create a directory to store the corpus files
  dir_name <- paste0(land_name, "_corpus")
  dir.create(dir_name, recursive = TRUE, showWarnings = FALSE)

  # SQL query to retrieve data
  col_map <- list(
    'e.id' = 'e.id',
    'e.url' = 'e.url',
    'e.title' = 'e.title',
    'e.description' = 'e.description',
    'e.readable' = 'e.readable',
    'd.name' = 'd.name'
  )
  sql <- sprintf("
    SELECT %s
    FROM expression AS e
    JOIN domain AS d ON d.id = e.domain_id
    WHERE e.land_id = ? AND e.relevance >= ?
    GROUP BY e.id
  ", paste(names(col_map), collapse = ", "))

  result <- dbGetQuery(con, sql, params = list(land_id, minimum_relevance))

  # Write each record to a text file
  for (i in 1:nrow(result)) {
    lignes <- result[i,]
    file_name <- paste0(slugify(paste(land_name, "_", lignes$id, sep="")), ".txt")
    file_path <- file.path(dir_name, file_name)
    metadata <- to_metadata(lignes)
    content <- paste(metadata, lignes$readable)
    write(content, file = file_path)
  }

  # Compress all text files into a zip file
  # List of all files in the directory
  file_paths <- list.files(dir_name, full.names = TRUE)

  # Create a zip file containing all the files
  zip::zip(zipfile = paste0(dir_name, ".zip"), files = file_paths)
}


#' Export Land Data
#'
#' This function manages the exportation of land data based on the specified export type.
#'
#' @param land_name A character string specifying the name of the land.
#' @param export_type A character string specifying the type of export ("pagecsv", "fullpagecsv", "nodecsv", "mediacsv", "pagegexf", "nodegexf", or "corpus").
#' @param minimum_relevance A numeric value specifying the minimum relevance score for inclusion in the export. Default is 1.
#' @param labase A character string specifying the name of the database file. Default is "mwi.db".
#' @import DBI RSQLite
#' @export
export_land <- function(land_name, export_type, minimum_relevance = 1, labase = "mwi.db") {
  con <- dbConnect(SQLite(), labase)


  if (export_type == 'pagecsv') {
    filename <- paste0(land_name, "_", export_type,".csv")
    export_pagecsv(con, land_name, minimum_relevance, filename)
  } else if (export_type == 'fullpagecsv') {
    filename <- paste0(land_name, "_", export_type,".csv")
    export_fullpagecsv(con, land_name, minimum_relevance, filename)
  } else if (export_type == 'nodecsv') {
    filename <- paste0(land_name, "_", export_type,".csv")
    export_nodecsv(con, land_name, minimum_relevance, filename)
  } else if (export_type == 'mediacsv') {
    filename <- paste0(land_name, "_", export_type,".csv")
    export_mediacsv(con, land_name, minimum_relevance, filename)
  } else if (export_type == 'pagegexf') {
    filename <- paste0(land_name, "_", export_type,".gexf")
    export_pagegexf(con, land_name, minimum_relevance, filename)
  } else if (export_type == 'nodegexf') {
    filename <- paste0(land_name, "_", export_type,".gexf")
    export_nodegexf(con, land_name, minimum_relevance, filename)
  } else if (export_type == 'corpus') {
    export_corpus(con, land_name, minimum_relevance)
  } else {
    message("Unknown export type. Waz it that ?")
  }

  # dont forget to close the con
  dbDisconnect(con)
}
