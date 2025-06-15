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
  # Validate inputs
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a data frame")
  }
  if (!is.character(variables) || length(variables) == 0) {
    stop("'variables' must be a non-empty character vector")
  }

  # Check if cld3 package is available
  if (!requireNamespace("cld3", quietly = TRUE)) {
    stop("Package 'cld3' is required but not installed. Please install it using install.packages('cld3').")
  }

  # Check if all specified variables exist in the data frame
  missing_vars <- setdiff(variables, names(df))
  if (length(missing_vars) > 0) {
    stop(paste("The following variables do not exist in the data frame:",
               paste(missing_vars, collapse = ", ")))
  }

  # Combine the texts from the specified variables more efficiently
  combined_text <- do.call(paste, c(df[variables], sep = " "))

  # Detect the language with error handling
  tryCatch({
    lang <- cld3::detect_language(combined_text)
    # Handle NULL returns from cld3
    if (is.null(lang)) lang <- rep(NA_character_, length(combined_text))
    return(lang)
  }, error = function(e) {
    warning(paste("Language detection failed:", e$message))
    return(rep(NA_character_, nrow(df)))
  })
}

#' Visualize Original and Transformed Distributions
#'
#' @description
#' This function creates side-by-side histograms comparing the original and
#' transformed distributions of numeric variables. It supports multiple
#' transformation types and provides extensive customization options.
#'
#' @param df A data frame containing the variables to visualize.
#' @param variables Optional character vector of variable names to analyze.
#'   If NULL (default), all numeric variables are analyzed.
#' @param trans_type Transformation type: "log1p" (default), "log", "sqrt", or "none".
#' @param bins Number of bins for histograms. If NULL (default), uses Sturges' rule.
#' @param colors Character vector of length 2 specifying colors for original and
#'   transformed plots. Default: c("#1f77b4", "#ff7f0e").
#' @param alpha Transparency level (0-1). Default: 0.7.
#' @param theme ggplot2 theme to use. Default: theme_minimal().
#' @param save Logical indicating whether to save plots to files. Default: FALSE.
#' @param save_dir Directory to save plots if save=TRUE. Default: "plotlog_output".
#' @param save_format File format: "png", "pdf", or "jpg". Default: "png".
#' @param save_dpi Resolution for saved images. Default: 300.
#'
#' @return Invisibly returns a list of ggplot objects. Primarily produces plots as a side effect.
#'
#' @details
#' The function performs the following steps:
#' 1. Validates all input parameters
#' 2. Identifies numeric variables to plot
#' 3. Creates comparison histograms for each variable
#' 4. Handles missing/infinite values gracefully
#' 5. Provides options to save output
#'
#' @examples
#' \dontrun{
#' # Basic usage with all numeric variables
#' plotlog(mtcars)
#'
#' # Specific variables with custom colors
#' plotlog(mtcars, c("mpg", "wt"), colors = c("darkgreen", "purple"))
#'
#' # Save plots as PDF
#' plotlog(iris, save = TRUE, save_format = "pdf")
#' }
#'
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @importFrom grDevices png pdf jpeg dev.off
#' @importFrom stats na.omit
#' @export
plotlog <- function(df, variables = NULL, trans_type = "log1p", bins = NULL,
                   colors = c("#1f77b4", "#ff7f0e"), alpha = 0.7,
                   theme = ggplot2::theme_minimal(), save = FALSE,
                   save_dir = "plotlog_output", save_format = "png",
                   save_dpi = 300) {
  
  # Input validation
  if (!is.data.frame(df)) stop("'df' must be a data frame")
  if (!is.null(variables) && !is.character(variables)) {
    stop("'variables' must be NULL or a character vector")
  }
  if (!trans_type %in% c("log1p", "log", "sqrt", "none")) {
    stop("'trans_type' must be one of: 'log1p', 'log', 'sqrt', 'none'")
  }
  if (length(colors) != 2 || !all(sapply(colors, is.character))) {
    stop("'colors' must be a character vector of length 2")
  }
  
  # Check for required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed")
  }
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package 'gridExtra' is required but not installed")
  }
  
  # Create save directory if needed
  if (save && !dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Identify numeric columns
  if (is.null(variables)) {
    num_cols <- names(df)[sapply(df, is.numeric)]
  } else {
    num_cols <- intersect(variables, names(df)[sapply(df, is.numeric)])
    if (length(num_cols) == 0) stop("No numeric variables found")
  }
  
  # Apply transformation
  apply_trans <- function(x, type) {
    switch(type,
           "log1p" = log1p(x),
           "log" = log(x),
           "sqrt" = sqrt(x),
           "none" = x)
  }
  
  # Create plots
  plot_list <- list()
  
  for (col_name in num_cols) {
    # Handle missing values
    clean_data <- na.omit(df[[col_name]])
    if (length(clean_data) == 0) {
      warning(paste("Skipping", col_name, "- no valid data"))
      next
    }
    
    # Apply transformation
    trans_data <- tryCatch({
      apply_trans(clean_data, trans_type)
    }, error = function(e) {
      warning(paste("Transformation failed for", col_name, ":", e$message))
      return(NULL)
    })
    
    if (is.null(trans_data)) next
    
    # Create plots
    p1 <- ggplot2::ggplot(data.frame(x = clean_data), ggplot2::aes(x = x)) +
      ggplot2::geom_histogram(fill = colors[1], alpha = alpha, bins = bins) +
      ggplot2::ggtitle(paste("Original:", col_name)) +
      theme
    
    trans_title <- switch(trans_type,
                         "log1p" = paste("log1p(", col_name, ")"),
                         "log" = paste("log(", col_name, ")"),
                         "sqrt" = paste("sqrt(", col_name, ")"),
                         "none" = col_name)
    
    p2 <- ggplot2::ggplot(data.frame(x = trans_data), ggplot2::aes(x = x)) +
      ggplot2::geom_histogram(fill = colors[2], alpha = alpha, bins = bins) +
      ggplot2::ggtitle(trans_title) +
      theme
    
    # Arrange and display
    gridExtra::grid.arrange(p1, p2, ncol = 2)
    plot_list[[col_name]] <- list(original = p1, transformed = p2)
    
    # Save if requested
    if (save) {
      filename <- file.path(save_dir, paste0(col_name, ".", save_format))
      switch(save_format,
             "png" = png(filename, width = 10, height = 5, units = "in", res = save_dpi),
             "pdf" = pdf(filename, width = 10, height = 5),
             "jpg" = jpeg(filename, width = 10, height = 5, units = "in", res = save_dpi))
      gridExtra::grid.arrange(p1, p2, ncol = 2)
      dev.off()
    }
  }
  
  invisible(plot_list)
}

# ------------------------------------------------------------------------------
# 1. DIAGNOSTIC DE VARIABLE
# ------------------------------------------------------------------------------

#' @title Diagnostiquer une variable numérique
#' @description Calcule les statistiques descriptives clés pour évaluer la
#' distribution d'une variable.
#' @param x Vecteur numérique.
#' @return Un tibble avec skewness, kurtosis, et le p-value du test de
#'   Shapiro-Wilk.
#' @export
diagnose_variable <- function(x) {
  stopifnot(is.numeric(x))
  
  x_finite <- x[is.finite(x)]
  
  if (length(x_finite) < 3) {
    return(tibble::tibble(
      n_valid = length(x_finite),
      skewness = NA_real_,
      kurtosis = NA_real_,
      shapiro_wilk_p = NA_real_,
      comment = "Pas assez de données valides."
    ))
  }
  
  # Le test de Shapiro-Wilk est limité à 5000 observations.
  shapiro_sample <- if (length(x_finite) > 5000) {
    sample(x_finite, 5000)
  } else {
    x_finite
  }
  
  tibble::tibble(
    n_valid = length(x_finite),
    skewness = moments::skewness(x_finite),
    kurtosis = moments::kurtosis(x_finite), # Excès de kurtosis
    shapiro_wilk_p = stats::shapiro.test(shapiro_sample)$p.value
  )
}


# ------------------------------------------------------------------------------
# 2. TRANSFORMATION
# ------------------------------------------------------------------------------

#' @title Transformer un vecteur numérique
#' @description Applique une transformation pour stabiliser la variance ou
#'   normaliser la distribution.
#' @param x Vecteur numérique.
#' @param method Une de "none", "log1p", "log", "boxcox", "yeojohnson".
#' @param ... Arguments additionnels passés aux fonctions de `bestNormalize`
#'   (ex: `standardize = FALSE`).
#' @return Une liste contenant `$val` (vecteur transformé) et `$param` (objet
#'   contenant les paramètres du fit, utile pour `predict`).
#' @export
transform_variable <- function(x, method = "yeojohnson", ...) {
  stopifnot(is.numeric(x),
            method %in% c("none", "log1p", "log", "boxcox", "yeojohnson"))
  
  if (stats::var(x, na.rm = TRUE) == 0) {
    warning("La variance de x est nulle. La transformation n'a pas d'effet.")
    return(list(val = x, param = NULL))
  }

  switch(
    method,
    none = list(val = x, param = NULL),
    log1p = list(val = log1p(pmax(x, 0)), param = NULL),
    log = {
      y <- x
      if (any(y <= 0, na.rm = TRUE)) {
        warning("Les valeurs <= 0 ont été mises à NA pour la transformation log.")
        y[y <= 0] <- NA_real_
      }
      list(val = log(y), param = NULL)
    },
    boxcox = {
      tf <- bestNormalize::boxcox(x, ...)
      list(val = tf$x.t, param = tf)
    },
    yeojohnson = {
      tf <- bestNormalize::yeojohnson(x, ...)
      list(val = tf$x.t, param = tf)
    }
  )
}


# ------------------------------------------------------------------------------
# 3. DISCRÉTISATION
# ------------------------------------------------------------------------------

#' @title Discrétiser un vecteur numérique
#' @description Transforme un vecteur numérique en facteur. Il est
#'   recommandé d'appliquer cette fonction sur la variable BRUTE pour une
#'   meilleure interprétabilité.
#' @param x Vecteur numérique.
#' @param method Une de "equal_width" (intervalles égaux), "equal_freq"
#'   (effectifs égaux), "manual".
#' @param bins Nombre de classes (ignoré si `method = "manual"`).
#' @param breaks Vecteur de seuils (utilisé si `method = "manual"`).
#' @return Un facteur.
#' @examples
#' # Obtenir des quartiles (4 groupes d'effectifs égaux)
#' # discretize_variable(rnorm(100), method = "equal_freq", bins = 4)
#' @export
discretize_variable <- function(x, method = "equal_freq", bins = 5, breaks = NULL) {
  stopifnot(is.numeric(x),
            method %in% c("equal_width", "equal_freq", "manual"))
  
  if (method == "manual") {
    stopifnot("Pour la méthode 'manual', l'argument 'breaks' doit être fourni." = !is.null(breaks))
    brks <- breaks
  } else {
    stopifnot("L'argument 'bins' doit être un entier >= 2." = (is.numeric(bins) && bins >= 2))
    brks <- switch(
      method,
      equal_width = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = bins + 1),
      equal_freq = stats::quantile(x, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE)
    )
  }
  
  # Gérer les seuils non-uniques dus à des distributions asymétriques
  brks <- unique(brks)
  if (length(brks) < 2) {
    warning("Impossible de créer les classes, pas assez de seuils uniques. Retour de NA.")
    return(factor(rep(NA, length(x))))
  }
  
  cut(x, breaks = brks, include.lowest = TRUE, right = FALSE)
}


# ------------------------------------------------------------------------------
# 4. CLUSTERING (1D)
# ------------------------------------------------------------------------------

#' @title Identifier des clusters dans un vecteur numérique
#' @description Utilise des modèles de mélange gaussiens pour trouver des
#'   groupes latents.
#' @details Il est souvent plus pertinent d'appliquer le clustering sur une
#'   variable **transformée** pour éviter que les outliers ne dominent la
#'   solution.
#' @param x Vecteur numérique.
#' @param max_G Nombre maximum de clusters à tester.
#' @param auto_select_model TRUE pour laisser Mclust choisir le meilleur modèle
#'   (structure de covariance) via BIC. FALSE pour forcer `modelNames`.
#' @param modelNames Modèles à tester si `auto_select_model = FALSE`.
#' @return Liste contenant `$fit_object` (l'objet mclust complet pour plots, etc.),
#'   `$best_model`, `$n_clusters`, et `$classification`.
#' @export
find_clusters <- function(x, max_G = 9, auto_select_model = TRUE, modelNames = "V") {
  x_finite <- x[is.finite(x)]
  
  if (length(unique(x_finite)) < 2L) {
    stop("Le vecteur doit avoir au moins 2 valeurs finies uniques.")
  }
  
  # On ne peut pas chercher plus de clusters qu'il y a de valeurs uniques.
  max_G_adj <- min(max_G, length(unique(x_finite)))
  
  fit <- mclust::Mclust(
    x_finite,
    G = 1:max_G_adj,
    modelNames = if (auto_select_model) NULL else modelNames
  )
  
  # Reconstruire le vecteur de classification pour matcher la longueur de x
  classification_full <- rep(NA_integer_, length(x))
  classification_full[is.finite(x)] <- fit$classification
  
  list(
    fit_object = fit,
    best_model = fit$modelName,
    n_clusters = fit$G,
    classification = factor(classification_full)
  )
}

# ------------------------------------------------------------------------------
# 5. ANALYSE DE LOI DE PUISSANCE
# ------------------------------------------------------------------------------

#' @title Tester l'hypothèse d'une loi de puissance
#' @description Analyse si la queue d'une distribution suit une loi de
#'   puissance. Ce n'est PAS une fonction de transformation.
#' @param x Vecteur de données numériques positives.
#' @param type "discrete" pour les comptes (entiers), "continuous" pour les
#'   mesures continues. Ce choix est crucial pour la méthode d'estimation.
#' @return Liste avec $xmin, $alpha, $gof_p (p-value du test de
#'   bootstrap goodness-of-fit), et l'objet poweRlaw complet.
#' @export
analyse_powerlaw <- function(x, type = "discrete") {
  stopifnot("Le type doit être 'discrete' ou 'continuous'" = type %in% c("discrete", "continuous"))
  
  x_pos <- x[is.finite(x) & x > 0]
  if (length(x_pos) < 50) {
    stop("Taille d'échantillon insuffisante (< 50) pour une analyse de loi de puissance fiable.")
  }

  pl_model <- switch(type,
    discrete = poweRlaw::displ$new(x_pos),
    continuous = poweRlaw::conpl$new(x_pos)
  )
  
  est <- poweRlaw::estimate_xmin(pl_model)
  pl_model$setXmin(est)
  
  # Un p-value élevé suggère que les données sont compatibles avec une loi de puissance.
  # Pour une analyse plus rigoureuse, comparer avec d'autres distributions
  # (ex: log-normale) avec poweRlaw::compare_distributions().
  gof <- poweRlaw::bootstrap_p(pl_model, no_of_sims = 500, threads = 2)
  
  list(
    xmin = pl_model$getXmin(),
    alpha = pl_model$getPars(),
    gof_p_value = gof$p,
    fit_object = pl_model
  )
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

#' Recode Cell Values Using GPT
#'
#' @description
#' This function uses OpenAI's GPT models to recode/transform cell values based on a provided prompt.
#' It includes robust error handling, input validation, and rate limiting.
#'
#' @param prompt A character string specifying the transformation prompt (e.g., "Translate to French").
#' @param cell A character string containing the value to be recoded.
#' @param sysprompt Optional system prompt to guide the model's behavior. 
#'   Default: "You are a helpful assistant that recodes dataframe values. Return only the transformed value."
#' @param model The GPT model to use. Default: "gpt-4o". Alternatives: "gpt-3.5-turbo".
#' @param temperature Controls randomness (0-2). Lower = more deterministic. Default: 0.8.
#' @param max_tokens Maximum length of response. Default: 1000.
#' @param max_retries Maximum API retry attempts on failure. Default: 3.
#' @param retry_delay Seconds between retries. Default: 1.
#'
#' @return A character string containing the recoded value, or NA if the operation fails.
#'
#' @details
#' The function performs the following steps:
#' 1. Validates all input parameters
#' 2. Checks for OpenAI API key in environment variables
#' 3. Makes API call with retry logic
#' 4. Handles various error conditions gracefully
#' 5. Returns transformed value or NA on failure
#'
#' @note
#' Requires the 'openai' package and a valid OPENAI_API_KEY environment variable.
#' For bulk operations, consider implementing additional rate limiting.
#'
#' @examples
#' \dontrun{
#' # Set API key
#' Sys.setenv(OPENAI_API_KEY = "your-key-here")
#'
#' # Simple translation
#' GPT_Recode("Translate to French", "Hello world")
#'
#' # More complex transformation
#' GPT_Recode("Extract the main verb", "The cat sat on the mat")
#' }
#'
#' @importFrom openai create_chat_completion
#' @export
GPT_Recode <- function(prompt, cell, 
                      sysprompt = "You are a helpful assistant that recodes dataframe values. Return only the transformed value.",
                      model = "gpt-4o", 
                      temperature = 0.8,
                      max_tokens = 1000,
                      max_retries = 3,
                      retry_delay = 1,
                      validate = TRUE) {
  
  # Input validation
  if (!is.character(prompt) || length(prompt) != 1 || nchar(prompt) == 0) {
    stop("'prompt' must be a non-empty character string")
  }
  if (!is.character(cell) || length(cell) != 1) {
    stop("'cell' must be a character string")
  }
  if (!is.numeric(temperature) || temperature < 0 || temperature > 2) {
    stop("'temperature' must be between 0 and 2")
  }
  
  # Check for API key
  if (Sys.getenv("OPENAI_API_KEY") == "") {
    stop("OPENAI_API_KEY environment variable not set. Please set it with Sys.setenv(OPENAI_API_KEY = 'your-key')")
  }
  
  # Prepare messages
  messages <- list(
    list(role = "system", content = sysprompt),
    list(role = "user", content = paste0(prompt, ":\n\n", cell))
  )
  
  # Try with retries
  for (i in 1:max_retries) {
    tryCatch({
      response <- openai::create_chat_completion(
        model = model,
        messages = messages,
        temperature = temperature,
        max_tokens = max_tokens
      )
      
      # Validate response
      if (is.null(response$choices) || length(response$choices) == 0) {
        stop("Empty response from API")
      }
      
      transformed <- response$choices[[1]]$message$content
      
      # Optional validation
      if (validate) {
        valid <- nchar(transformed) > 0 && 
                 nchar(transformed) < (nchar(cell) * 10) &&
                 !grepl("sorry|error|cannot", transformed, ignore.case = TRUE)
        if (!valid) {
          warning("GPT returned potentially invalid response")
          return(NA_character_)
        }
      }
      return(transformed)
    }, error = function(e) {
      if (i == max_retries) {
        warning(paste("GPT recoding failed after", max_retries, "attempts:", e$message))
        return(NA_character_)
      }
      Sys.sleep(retry_delay)
    })
  }
}
