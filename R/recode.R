#' Detect Language of Text in Data Frame Columns
#'
#' @description
#' Detects the language of concatenated text coming from one or more columns of a data frame.
#' Text is normalised (Unicode NFC), cleaned from HTML and URLs, short snippets are filtered
#' out, and CLD3 probabilities are inspected so low-confidence guesses can be rejected.
#' An optional fastText backend can be used when a pre-trained model is supplied.
#'
#' @param df A data frame containing the text columns to analyse.
#' @param variables Character vector naming the columns to combine before detection.
#' @param engine Detection backend to use; currently `"cld3"` (default) or `"fasttext"`.
#' @param min_chars Minimum number of characters required after cleaning to attempt detection.
#' @param min_prob Minimum posterior probability required to accept a language prediction when
#'   `conservative = TRUE`. Set to `NULL` to disable the threshold.
#' @param conservative Logical; when `TRUE` (default) reject low-probability or ambiguous
#'   detections and return `NA`.
#' @param chunk_size Optional integer size for processing rows in batches; defaults to 5000.
#' @param fasttext_model Either a path to a `.bin` model readable by `fastrtext::load_model()`
#'   or a pre-loaded fastText model object when `engine = "fasttext"`.
#' @param return_scores Logical; when `TRUE`, return a data frame with language codes and their
#'   associated probabilities instead of a bare character vector.
#'
#' @return A character vector of language codes (ISO 639-1) matching the number of rows in `df`.
#'   When `return_scores = TRUE`, returns a data frame with `language` and `probability` columns.
#'   When returning a vector, the probabilities are also attached as the `"probability"` attribute.
#'
#' @details
#' 1. Validates inputs and required packages.
#' 2. Normalises and cleans text (Unicode NFC, remove HTML tags/URLs, collapse whitespace).
#' 3. Filters out rows whose combined text is too short to yield reliable detection.
#' 4. Runs the selected backend (`cld3` or `fasttext`) in configurable chunks.
#' 5. Applies probability thresholds to avoid low-confidence labels.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   title = c("Hello world", "Bonjour le monde", "Hola mundo"),
#'   description = c("This is a test", "Ceci est un test", "Esto es una prueba")
#' )
#'
#' # Default CLD3 backend with conservative thresholding
#' mwiR_detectLang(df, c("title", "description"))
#'
#' # Return probabilities and disable conservative filtering
#' mwiR_detectLang(df, "title", conservative = FALSE, return_scores = TRUE)
#' }
#'
#' @export
#' @importFrom cld3 detect_language_probabilities
#' @importFrom stringi stri_trans_general stri_replace_all_regex stri_trim_both stri_length stri_join
mwiR_detectLang <- function(df,
                            variables,
                            engine = c("cld3", "fasttext"),
                            min_chars = 25L,
                            min_prob = 0.7,
                            conservative = TRUE,
                            chunk_size = 5000L,
                            fasttext_model = NULL,
                            return_scores = FALSE) {
  if (!is.data.frame(df)) stop("Input 'df' must be a data frame.")
  if (!is.character(variables) || length(variables) == 0L) {
    stop("'variables' must be a non-empty character vector.")
  }
  engine <- match.arg(engine)
  if (!is.null(min_prob) && (!is.numeric(min_prob) || length(min_prob) != 1L || min_prob < 0 || min_prob > 1)) {
    stop("'min_prob' must be NULL or a single numeric value in [0, 1].")
  }
  if (!is.numeric(min_chars) || length(min_chars) != 1L || min_chars < 0L) {
    stop("'min_chars' must be a single non-negative integer.")
  }
  if (!is.null(chunk_size) && (!is.numeric(chunk_size) || chunk_size <= 0)) {
    stop("'chunk_size' must be NULL or a positive integer.")
  }
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Package 'stringi' is required but not installed.")
  }
  if (!requireNamespace("cld3", quietly = TRUE)) {
    stop("Package 'cld3' is required but not installed.")
  }

  missing_vars <- setdiff(variables, names(df))
  if (length(missing_vars) > 0L) {
    stop("The following variables do not exist in 'df': ", paste(missing_vars, collapse = ", "))
  }

  normalise_column <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- stringi::stri_trans_general(x, "Any-NFC")
    x <- stringi::stri_replace_all_regex(x, "<[^>]+>", " ")
    x <- stringi::stri_replace_all_regex(x, "https?://[^\\s]+", " ")
    x <- stringi::stri_replace_all_regex(x, "[\\p{C}]+", " ")
    x <- stringi::stri_replace_all_regex(x, "\\s+", " ")
    stringi::stri_trim_both(x)
  }

  normalised_cols <- lapply(variables, function(v) normalise_column(df[[v]]))
  combined_text <- do.call(
    stringi::stri_join,
    c(normalised_cols, list(sep = " "))
  )
  combined_text <- stringi::stri_replace_all_regex(combined_text, "\\s+", " ")
  combined_text <- stringi::stri_trim_both(combined_text)
  combined_text[combined_text == ""] <- NA_character_

  valid_mask <- !is.na(combined_text) & stringi::stri_length(combined_text) >= min_chars
  n <- length(combined_text)
  languages <- rep(NA_character_, n)
  probabilities <- rep(NA_real_, n)

  if (engine == "fasttext") {
    if (!requireNamespace("fastrtext", quietly = TRUE)) {
      stop("Package 'fastrtext' is required for the fastText backend.")
    }
    if (is.null(fasttext_model)) {
      stop("Provide 'fasttext_model' (path or loaded model) when engine = 'fasttext'.")
    }
    if (is.character(fasttext_model) && length(fasttext_model) == 1L) {
      ft_model <- fastrtext::load_model(fasttext_model)
    } else {
      ft_model <- fasttext_model
    }
  }

  detect_cld3 <- function(texts) {
    detections <- cld3::detect_language_probabilities(texts)
    lang <- rep(NA_character_, length(texts))
    prob <- rep(NA_real_, length(texts))
    for (i in seq_along(detections)) {
      entry <- detections[[i]]
      if (is.null(entry) || nrow(entry) == 0L) next
      top <- entry[1, , drop = FALSE]
      lang_i <- top$language
      prob_i <- top$probability
      if (conservative && !is.null(min_prob) && !is.na(prob_i) && prob_i < min_prob) {
        lang_i <- NA_character_
      }
      if (conservative && nrow(entry) > 1L) {
        second_prob <- entry$probability[2]
        if (!is.na(prob_i) && !is.na(second_prob) && (prob_i - second_prob) < 0.1) {
          lang_i <- NA_character_
        }
      }
      lang[i] <- lang_i
      prob[i] <- prob_i
    }
    list(language = lang, probability = prob)
  }

  detect_fasttext <- function(texts) {
    preds <- fastrtext::predict(ft_model, texts, k = 2L, prob = TRUE)
    lang <- rep(NA_character_, length(preds))
    prob <- rep(NA_real_, length(preds))
    for (i in seq_along(preds)) {
      entry <- preds[[i]]
      if (length(entry$labels) == 0L) next
      label <- sub("^__label__", "", entry$labels[1])
      prob_i <- entry$probabilities[1]
      if (conservative && !is.null(min_prob) && !is.na(prob_i) && prob_i < min_prob) {
        label <- NA_character_
      }
      if (conservative && length(entry$probabilities) > 1L) {
        second <- entry$probabilities[2]
        if (!is.na(prob_i) && !is.na(second) && (prob_i - second) < 0.1) {
          label <- NA_character_
        }
      }
      lang[i] <- label
      prob[i] <- prob_i
    }
    list(language = lang, probability = prob)
  }

  idx_valid <- which(valid_mask)
  if (length(idx_valid) > 0L) {
    if (is.null(chunk_size) || !is.finite(chunk_size) || chunk_size <= 0) {
      chunk_size <- length(idx_valid)
    }
    chunk_size <- as.integer(chunk_size)
    chunk_ids <- split(idx_valid, ceiling(seq_along(idx_valid) / chunk_size))
    for (idx in chunk_ids) {
      texts <- combined_text[idx]
      result <- if (engine == "cld3") detect_cld3(texts) else detect_fasttext(texts)
      languages[idx] <- result$language
      probabilities[idx] <- result$probability
    }
  }

  probabilities[!valid_mask] <- NA_real_

  if (return_scores) {
    return(data.frame(
      language = languages,
      probability = probabilities,
      stringsAsFactors = FALSE
    ))
  }

  attr(languages, "probability") <- probabilities
  languages
}


#' Visualise Original and Transformed Distributions
#'
#' @description
#' Builds paired density-scaled histograms comparing original numeric variables with a
#' transformed version. Textbook binning rules (Sturges, Freedman–Diaconis, Scott, etc.)
#' are implemented for adaptive resolution, potentially shifted transforms respect the
#' domain constraints of log/sqrt families (cf. Burbidge, Magee & Robb, 1988), and plots
#' can be saved or returned for further composition.
#'
#' @param df Data frame containing the variables to explore.
#' @param variables Optional character vector of column names. When `NULL`, every numeric
#'   column is used.
#' @param trans_type Transformation to apply. Supply a single name (default `"log1p"`),
#'   one value per variable, a named list (optionally with `default`), or a custom
#'   function. Supported names: `"none"`, `"log1p"`, `"log"`, `"sqrt"`, `"zscore"`,
#'   `"rank"`.
#' @param bins Either a single integer (fixed bin count) or one of `"sturges"`,
#'   `"fd"`/`"freedman-diaconis"`, `"scott"`, `"sqrt"`, `"rice"`, `"doane"`, or `"auto"`
#'   (max of Sturges & F-D, per Freedman & Diaconis, 1981).
#' @param colors Length-two character vector used for original and transformed fills.
#' @param alpha Histogram fill transparency (0–1).
#' @param theme ggplot2 theme applied to every panel.
#' @param density Logical flag adding a kernel density overlay (on the same density scale).
#' @param show_rug Logical; when `TRUE`, draws a rug to highlight individual values.
#' @param na_rm Remove `NA`/`NaN`/`Inf` before drawing. When `FALSE`, they are retained
#'   but still excluded from the geoms.
#' @param min_non_missing Minimum number of finite observations required to attempt a plot.
#' @param shift_constant Positive offset used when a transform needs shifting into its
#'   domain (e.g. log of non-positive data).
#' @param display Draw plots to the active graphics device (`TRUE` by default).
#' @param save Persist paired panels with `ggplot2::ggsave()`.
#' @param save_dir Directory for saved graphics (created on demand).
#' @param save_format File format (`"png"`, `"pdf"`, or `"jpg"`).
#' @param save_dpi Resolution when the format supports it.
#' @param device_width,device_height Dimensions (inches) used for saved layouts.
#' @param verbose Emit informational messages (defaults to `interactive()`).
#'
#' @return Invisibly returns a list with two elements:
#'   * `plots`: per-variable lists containing the original panel, transformed panel, and
#'     the combined grob.
#'   * `metadata`: data frame describing data hygiene, transform shifts, and binning.
#'
#' @details
#' - Binning rules follow the literature on histogram estimation (Freedman & Diaconis,
#'   1981; Scott, 1979; Doane, 1976) and fall back to Sturges when a rule is ill-conditioned.
#' - Domain corrections for log-like transforms follow the additive constant strategy
#'   recommended by Burbidge, Magee & Robb (1988).
#'
#' @references
#' Freedman, D., & Diaconis, P. (1981). *On the histogram as a density estimator: L2 theory*.
#' Burbidge, J. B., Magee, L., & Robb, A. L. (1988). *Alternative transformations to handle
#'    extreme values of the dependent variable*. Journal of the American Statistical Association.
#'
#' @import ggplot2
#' @importFrom gridExtra arrangeGrob grid.draw
#' @importFrom stats IQR sd
#' @export
plotlog <- function(df,
                    variables = NULL,
                    trans_type = "log1p",
                    bins = "sturges",
                    colors = c("#1f77b4", "#ff7f0e"),
                    alpha = 0.6,
                    theme = ggplot2::theme_minimal(),
                    density = TRUE,
                    show_rug = FALSE,
                    na_rm = TRUE,
                    min_non_missing = 5L,
                    shift_constant = 1,
                    display = TRUE,
                    save = FALSE,
                    save_dir = "plotlog_output",
                    save_format = c("png", "pdf", "jpg"),
                    save_dpi = 300,
                    device_width = 9,
                    device_height = 4.5,
                    verbose = interactive()) {

  if (!is.data.frame(df)) stop("'df' must be a data frame.")
  if (!is.null(variables) && !is.character(variables)) {
    stop("'variables' must be NULL or a character vector.")
  }
  if (length(colors) != 2L || !all(vapply(colors, is.character, logical(1L)))) {
    stop("'colors' must contain exactly two colour values.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package 'gridExtra' is required but not installed.")
  }

  save_format <- match.arg(save_format)
  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1L))]
  if (!is.null(variables)) {
    numeric_cols <- intersect(variables, numeric_cols)
  }
  if (length(numeric_cols) == 0L) stop("No numeric variables selected for plotting.")

  if (is.character(trans_type) && length(trans_type) > 1L) {
    if (length(trans_type) != length(numeric_cols)) {
      stop("Provide one transform per variable or a single transform for all variables.")
    }
    if (is.null(names(trans_type))) {
      names(trans_type) <- numeric_cols
    }
  }

  resolve_transform <- function(var) {
    if (is.function(trans_type)) return(trans_type)
    if (is.list(trans_type)) {
      if (!is.null(trans_type[[var]])) return(trans_type[[var]])
      if (!is.null(trans_type[["default"]])) return(trans_type[["default"]])
      stop("No transform specified for variable '", var, "' and no 'default' entry supplied.")
    }
    if (is.character(trans_type)) {
      if (length(trans_type) == 1L) return(trans_type[[1L]])
      return(trans_type[[var]])
    }
    stop("'trans_type' must be a character scalar/vector, list, or function.")
  }

  ensure_lower_bound <- function(x, lower, offset) {
    min_val <- min(x, na.rm = TRUE)
    shift <- 0
    if (is.finite(min_val) && min_val <= lower) {
      shift <- (lower - min_val) + abs(offset)
      x <- x + shift
    }
    list(values = x, shift = shift)
  }

  transform_series <- function(x, spec, label, shift_constant) {
    if (is.function(spec)) {
      y <- spec(x)
      if (!is.numeric(y)) stop("Custom transform for '", label, "' must return numeric output.")
      return(list(
        values = as.numeric(y),
        type = "custom",
        shift = 0,
        note = "User-supplied transformation.",
        label = paste0("custom(", label, ")")
      ))
    }

    if (!is.character(spec) || length(spec) != 1L) {
      stop("Transform spec must be a single character value or function.")
    }

    type <- tolower(spec)
    shift <- 0
    note <- NULL
    result <- switch(
      type,
      "none" = {
        list(values = x, label = label)
      },
      "log1p" = {
        adj <- ensure_lower_bound(x, lower = -1, offset = shift_constant)
        shift <- adj$shift
        if (shift > 0) {
          note <- paste0("Shifted by ", format(shift, digits = 4), " before applying log1p().")
        }
        list(values = log1p(adj$values),
             label = if (shift > 0) paste0("log1p(", label, " + ", format(shift, digits = 4), ")")
             else paste0("log1p(", label, ")"))
      },
      "log" = {
        adj <- ensure_lower_bound(x, lower = 0, offset = shift_constant)
        shift <- adj$shift
        if (shift > 0) {
          note <- paste0("Shifted by ", format(shift, digits = 4), " before applying log().")
        }
        list(values = log(adj$values),
             label = if (shift > 0) paste0("log(", label, " + ", format(shift, digits = 4), ")")
             else paste0("log(", label, ")"))
      },
      "sqrt" = {
        adj <- ensure_lower_bound(x, lower = 0, offset = shift_constant)
        shift <- adj$shift
        if (shift > 0) {
          note <- paste0("Shifted by ", format(shift, digits = 4), " before applying sqrt().")
        }
        list(values = sqrt(adj$values),
             label = if (shift > 0) paste0("sqrt(", label, " + ", format(shift, digits = 4), ")")
             else paste0("sqrt(", label, ")"))
      },
      "zscore" = {
        sd_x <- stats::sd(x, na.rm = TRUE)
        if (!is.finite(sd_x) || sd_x < .Machine$double.eps) {
          stop("Standard deviation is zero; cannot compute z-scores for '", label, "'.")
        }
        note <- "Mean-centred and scaled by sample SD."
        list(values = as.numeric(scale(x)), label = paste0("zscore(", label, ")"))
      },
      "rank" = {
        note <- "Average ranks, ties handled via 'average'."
        list(values = rank(x, ties.method = "average"), label = paste0("rank(", label, ")"))
      },
      stop("Unsupported transformation '", spec, "'.")
    )

    list(
      values = as.numeric(result$values),
      type = type,
      shift = shift,
      note = note,
      label = result$label
    )
  }

  calc_bins <- function(values, bins, min_bins = 10L, max_bins = 200L) {
    x <- values[is.finite(values)]
    n <- length(x)
    if (n <= 1L) {
      return(list(
        n = 1L,
        method = "single",
        note = "Insufficient unique observations for binning."
      ))
    }

    if (is.numeric(bins) && length(bins) == 1L) {
      n_bins <- max(1L, as.integer(round(bins)))
      n_bins <- max(min_bins, min(max_bins, n_bins))
      return(list(
        n = n_bins,
        method = "fixed",
        note = "User-specified bin count."
      ))
    }

    if (!is.character(bins) || length(bins) != 1L) {
      stop("'bins' must be a single integer or recognised rule name.")
    }

    rule <- switch(
      tolower(bins),
      "sturges" = "sturges",
      "fd" = "freedman-diaconis",
      "freedman-diaconis" = "freedman-diaconis",
      "freedmandiaconis" = "freedman-diaconis",
      "scott" = "scott",
      "sqrt" = "sqrt",
      "rice" = "rice",
      "doane" = "doane",
      "auto" = "auto",
      stop("Unsupported 'bins' specification: ", bins)
    )

    compute_rule <- function(rule_name) {
      range_x <- diff(range(x))
      if (!is.finite(range_x) || range_x <= 0) return(NA_real_)
      n <- length(x)
      switch(
        rule_name,
        "sturges" = log2(n) + 1,
        "sqrt" = sqrt(n),
        "rice" = 2 * n^(1 / 3),
        "scott" = {
          bw <- 3.49 * stats::sd(x) / (n^(1 / 3))
          if (!is.finite(bw) || bw <= 0) NA_real_ else range_x / bw
        },
        "doane" = {
          z <- (x - mean(x)) / stats::sd(x)
          g1 <- mean(z^3)
          sg1 <- sqrt(6 * (n - 2) / ((n + 1) * (n + 3)))
          1 + log2(n) + log2(1 + abs(g1) / sg1)
        },
        "freedman-diaconis" = {
          bw <- 2 * stats::IQR(x) / (n^(1 / 3))
          if (!is.finite(bw) || bw <= 0) NA_real_ else range_x / bw
        },
        stop("Unimplemented rule: ", rule_name)
      )
    }

    fallback_note <- NULL
    computed <- switch(
      rule,
      "auto" = max(compute_rule("sturges"), compute_rule("freedman-diaconis"), na.rm = TRUE),
      compute_rule(rule)
    )

    if (!is.finite(computed) || computed <= 0) {
      computed <- compute_rule("sturges")
      fallback_note <- "Fell back to Sturges rule because the chosen rule was ill-conditioned."
    }

    n_bins <- max(1L, round(computed))
    n_bins <- max(min_bins, min(max_bins, n_bins))

    note <- c(
      if (rule == "auto") "Auto selects the maximum of Sturges and Freedman-Diaconis." else NULL,
      fallback_note
    )
    list(
      n = as.integer(n_bins),
      method = if (rule == "auto" && !is.null(fallback_note)) "sturges" else rule,
      note = if (length(note)) paste(note, collapse = " ") else NULL
    )
  }

  if (save && !dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  }

  compact <- function(x) {
    x[!(is.null(x) | is.na(x) | x == "")]
  }

  plots <- list()
  meta <- list()

  for (var in numeric_cols) {
    raw <- df[[var]]
    total_n <- length(raw)

    if (!na_rm && any(!is.finite(raw))) {
      warning("Variable '", var, "' contains non-finite values; they will be omitted in plots.")
    }

    clean <- raw[is.finite(raw)]
    removed <- total_n - length(clean)

    if (length(clean) < min_non_missing) {
      note <- paste0("Skipped: only ", length(clean),
                     " finite observations (min_non_missing = ", min_non_missing, ").")
      meta[[var]] <- data.frame(
        variable = var,
        n_total = total_n,
        n_used = length(clean),
        n_removed = removed,
        transformation = NA_character_,
        shift_applied = NA_real_,
        bins_original = NA_integer_,
        bins_transformed = NA_integer_,
        rule_original = NA_character_,
        rule_transformed = NA_character_,
        status = "skipped",
        notes = note,
        stringsAsFactors = FALSE
      )
      if (verbose) message(note)
      next
    }

    transform_spec <- resolve_transform(var)
    trans <- tryCatch(
      transform_series(clean, transform_spec, var, shift_constant),
      error = function(e) {
        warning("Transformation failed for '", var, "': ", e$message)
        NULL
      }
    )

    if (is.null(trans)) {
      meta[[var]] <- data.frame(
        variable = var,
        n_total = total_n,
        n_used = length(clean),
        n_removed = removed,
        transformation = deparse(transform_spec),
        shift_applied = NA_real_,
        bins_original = NA_integer_,
        bins_transformed = NA_integer_,
        rule_original = NA_character_,
        rule_transformed = NA_character_,
        status = "skipped",
        notes = "Transformation error.",
        stringsAsFactors = FALSE
      )
      next
    }

    transformed <- trans$values[is.finite(trans$values)]
    if (length(transformed) < min_non_missing) {
      note <- paste0("Skipped transformed plot for '", var,
                     "'; only ", length(transformed),
                     " finite observations after transformation.")
      meta[[var]] <- data.frame(
        variable = var,
        n_total = total_n,
        n_used = length(clean),
        n_removed = removed,
        transformation = trans$type,
        shift_applied = trans$shift,
        bins_original = NA_integer_,
        bins_transformed = NA_integer_,
        rule_original = NA_character_,
        rule_transformed = NA_character_,
        status = "skipped",
        notes = note,
        stringsAsFactors = FALSE
      )
      if (verbose) message(note)
      next
    }

    bin_orig <- calc_bins(clean, bins)
    bin_trans <- calc_bins(transformed, bins)

    plot_original <- ggplot2::ggplot(
      data = data.frame(value = clean),
      ggplot2::aes(x = value)
    ) +
      ggplot2::geom_histogram(
        ggplot2::aes(y = ggplot2::after_stat(density)),
        bins = bin_orig$n,
        fill = colors[1],
        alpha = alpha,
        colour = NA
      ) +
      ggplot2::labs(
        title = paste0(var, " (original)"),
        subtitle = paste0("n = ", length(clean), "; bins = ", bin_orig$n,
                          " [", bin_orig$method, "]"),
        x = var,
        y = "Density"
      ) +
      theme

    if (density) {
      plot_original <- plot_original + ggplot2::geom_density(
        colour = colors[1],
        linewidth = 0.6,
        alpha = 0.1
      )
    }
    if (show_rug) {
      plot_original <- plot_original + ggplot2::geom_rug(alpha = 0.3)
    }

    plot_transformed <- ggplot2::ggplot(
      data = data.frame(value = transformed),
      ggplot2::aes(x = value)
    ) +
      ggplot2::geom_histogram(
        ggplot2::aes(y = ggplot2::after_stat(density)),
        bins = bin_trans$n,
        fill = colors[2],
        alpha = alpha,
        colour = NA
      ) +
      ggplot2::labs(
        title = paste0(var, " (transformed)"),
        subtitle = paste0("n = ", length(transformed), "; bins = ",
                          bin_trans$n, " [", bin_trans$method, "]"),
        x = trans$label,
        y = "Density"
      ) +
      theme

    if (density) {
      plot_transformed <- plot_transformed + ggplot2::geom_density(
        colour = colors[2],
        linewidth = 0.6,
        alpha = 0.1
      )
    }
    if (show_rug) {
      plot_transformed <- plot_transformed + ggplot2::geom_rug(alpha = 0.3)
    }

    combined <- gridExtra::arrangeGrob(plot_original, plot_transformed, ncol = 2)
    if (display) gridExtra::grid.draw(combined)

    if (save) {
      file <- file.path(save_dir, paste0(var, ".", save_format))
      ggplot2::ggsave(
        filename = file,
        plot = combined,
        dpi = save_dpi,
        width = device_width,
        height = device_height,
        units = "in",
        limitsize = FALSE
      )
    }

    note_vec <- compact(c(
      if (removed > 0) paste0("Removed ", removed, " non-finite value(s).") else NULL,
      trans$note,
      bin_orig$note,
      bin_trans$note
    ))

    meta[[var]] <- data.frame(
      variable = var,
      n_total = total_n,
      n_used = length(clean),
      n_removed = removed,
      transformation = trans$type,
      shift_applied = trans$shift,
      bins_original = bin_orig$n,
      bins_transformed = bin_trans$n,
      rule_original = bin_orig$method,
      rule_transformed = bin_trans$method,
      status = "plotted",
      notes = if (length(note_vec)) paste(note_vec, collapse = " | ") else NA_character_,
      stringsAsFactors = FALSE
    )

    plots[[var]] <- list(
      original = plot_original,
      transformed = plot_transformed,
      combined = combined
    )
  }

  metadata <- if (length(meta)) {
    do.call(rbind, meta)
  } else {
    data.frame()
  }

  result <- list(plots = plots, metadata = metadata)
  class(result) <- c("plotlog_result", class(result))
  invisible(result)
}

# ------------------------------------------------------------------------------
# 1. DIAGNOSTIC DE VARIABLE
# ------------------------------------------------------------------------------
#' @title Transformer un vecteur numérique
#' @description Applique des transformations usuelles en data science pour
#'   stabiliser la variance, rapprocher une distribution de la normalité ou
#'   contrôler les valeurs extrêmes. La fonction embarque un backend
#'   `bestNormalize` pour une sélection data-driven ainsi que des transformations
#'   classiques (centre/réduction, Box-Cox, Yeo-Johnson, normalisation par rang,
#'   logs avec décalage automatique, etc.). Les sorties incluent une fonction
#'   d'inversion afin d'opérer des prédictions/cohortes dans le même espace.
#'
#' @param x Vecteur numérique (NA, Inf, -Inf autorisés).
#' @param method Choix de transformation : `"auto"` (par défaut, via
#'   `bestNormalize`), `"none"`, `"center"`, `"zscore"`, `"robust_z"`,
#'   `"log1p"`, `"log"`, `"sqrt"`, `"boxcox"`, `"yeojohnson"`, `"ranknorm"`.
#'   Un objet fonctionnel peut être fourni pour une transformation custom
#'   (doit retourner un vecteur numérique de même longueur).
#' @param winsorize Proportion (0–0.5) utilisée pour winsoriser les extrêmes
#'   avant la transformation. Exemple : `winsorize = 0.01` tronque au quantile
#'   1 % et 99 %. `NULL` (défaut) désactive l'opération.
#' @param shift_constant Constante strictement positive ajoutée lorsque l'on
#'   doit décaler une série vers le domaine de définition (log, sqrt, log1p).
#' @param handle_na Comment traiter les valeurs manquantes : `"keep"` (défaut)
#'   conserve les NA dans `val`, `"omit"` les supprime avant estimation des
#'   paramètres (recommandé quand on alimente un modèle qui gère lui-même les NA).
#' @param ... Arguments additionnels passés à `bestNormalize::bestNormalize`,
#'   `bestNormalize::boxcox`, `bestNormalize::yeojohnson` ou `bestNormalize::orderNorm`
#'   selon le cas.
#'
#' @return Un objet de classe `mwi_transform` contenant :
#'   * `$val` : vecteur transformé (longueur identique à `x`);
#'   * `$param` : objet paramètres/transformer (utile pour `predict` ou inversion);
#'   * `$info` : liste de métadonnées (méthode retenue, décalage appliqué, winsorisation, etc.);
#'   * `$functions` : liste de deux fermetures `forward(new_x)` et `inverse(new_z)` alignées sur la transformation.
#'
#' @details
#' - L’option `method = "auto"` délègue à `bestNormalize` la sélection de la
#'   meilleure transformation parmi Box-Cox, Yeo-Johnson, normalisation par rang
#'   et centre/réduction (Petersen, 2021).  
#' - Les transformations de type log/sqrt adoptent la stratégie de décalage
#'   recommandée par Burbidge, Magee & Robb (1988) pour garantir la validité
#'   du domaine lorsque la série contient des valeurs non positives.  
#' - Le calcul `winsorize` s’inspire des pratiques robustes en sciences sociales
#'   pour limiter l’influence d’observations extrêmes (Wilcox, 2017).
#'
#' @references
#' Petersen, M. (2021). *bestNormalize: Normalizing transformation functions*.  
#' Burbidge, J. B., Magee, L., & Robb, A. L. (1988). *Alternative transformations
#' to handle extreme values of the dependent variable*. JASA.  
#' Wilcox, R. R. (2017). *Introduction to robust estimation and hypothesis testing*.
#'
#' @export
transform_variable <- function(x,
                               method = c("auto", "none", "center", "zscore", "robust_z",
                                          "log1p", "log", "sqrt", "boxcox",
                                          "yeojohnson", "ranknorm"),
                               winsorize = NULL,
                               shift_constant = 1,
                               handle_na = c("keep", "omit"),
                               ...) {
  if (!is.numeric(x)) stop("'x' doit être numérique.")
  handle_na <- match.arg(handle_na)
  method <- method[1]

  require_if <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Le package '", pkg, "' est requis pour cette transformation.")
    }
  }

  finite_mask <- is.finite(x)
  x_work <- x

  if (!is.null(winsorize)) {
    if (!is.numeric(winsorize) || length(winsorize) != 1L ||
        winsorize < 0 || winsorize >= 0.5) {
      stop("'winsorize' doit être un scalaire dans [0, 0.5).")
    }
    if (sum(finite_mask) > 0L) {
      q_low <- stats::quantile(x[finite_mask], probs = winsorize, type = 7, names = FALSE)
      q_high <- stats::quantile(x[finite_mask], probs = 1 - winsorize, type = 7, names = FALSE)
      x_work[finite_mask] <- pmin(pmax(x_work[finite_mask], q_low), q_high)
    }
  }

  if (handle_na == "omit") {
    finite_mask <- is.finite(x_work)
  }

  ensure_domain <- function(values, lower_bound) {
    finite_vals <- values[finite_mask]
    if (!length(finite_vals)) return(list(values = values, shift = 0))
    min_val <- min(finite_vals, na.rm = TRUE)
    if (is.infinite(min_val)) min_val <- NA_real_
    shift <- 0
    if (!is.na(min_val) && min_val <= lower_bound) {
      shift <- (lower_bound - min_val) + abs(shift_constant)
      values <- values + shift
    }
    list(values = values, shift = shift)
  }

  results <- list(
    val = x_work,
    param = NULL,
    info = list(
      method_requested = method,
      applied_shift = 0,
      winsorize = winsorize,
      handle_na = handle_na,
      n_total = length(x),
      n_finite = sum(is.finite(x)),
      n_missing = sum(!is.finite(x)),
      warnings = character()
    ),
    functions = list(
      forward = NULL,
      inverse = NULL
    )
  )

  add_warning <- function(msg) {
    results$info$warnings <- c(results$info$warnings, msg)
  }

  apply_simple <- function(transform_fun, inverse_fun = NULL, label) {
    out <- x_work
    out[finite_mask] <- transform_fun(x_work[finite_mask])
    results$val <- out
    results$param <- list(method = label)
    results$info$method_applied <- label
    results$functions$forward <- function(new_x) {
      values <- new_x
      idx <- is.finite(values)
      values[idx] <- transform_fun(values[idx])
      values
    }
    results$functions$inverse <- if (is.null(inverse_fun)) {
      NULL
    } else {
      function(new_z) {
        values <- new_z
        idx <- is.finite(values)
        values[idx] <- inverse_fun(values[idx])
        values
      }
    }
    results
  }

  # Identité immédiate pour variance nulle
  if (sum(finite_mask) <= 1L || stats::var(x_work[finite_mask], na.rm = TRUE) == 0) {
    add_warning("Variance nulle ou trop peu d'observations : transformation identitaire.")
    results$info$method_applied <- "none"
    results$functions$forward <- function(new_x) new_x
    results$functions$inverse <- function(new_z) new_z
    return(structure(results, class = c("mwi_transform", class(results))))
  }

  # Transformations personnalisées
  if (is.function(method)) {
    trans <- method(x_work)
    if (!is.numeric(trans) || length(trans) != length(x)) {
      stop("La transformation personnalisée doit retourner un vecteur numérique de même longueur.")
    }
    results$val <- trans
    results$param <- list(method = "custom", fun = method)
    results$info$method_applied <- "custom"
    results$functions$forward <- method
    results$functions$inverse <- NULL
    return(structure(results, class = c("mwi_transform", class(results))))
  }

  method <- match.arg(method,
                      choices = c("auto", "none", "center", "zscore", "robust_z",
                                  "log1p", "log", "sqrt", "boxcox",
                                  "yeojohnson", "ranknorm"))

  out <- switch(
    method,
    none = {
      results$info$method_applied <- "none"
      results$functions$forward <- function(new_x) new_x
      results$functions$inverse <- function(new_z) new_z
      results
    },
    center = {
      mu <- mean(x_work[finite_mask], na.rm = TRUE)
      apply_simple(function(v) v - mu, function(v) v + mu, "center")
    },
    zscore = {
      mu <- mean(x_work[finite_mask], na.rm = TRUE)
      sd_val <- stats::sd(x_work[finite_mask], na.rm = TRUE)
      if (!is.finite(sd_val) || sd_val < .Machine$double.eps) {
        add_warning("Ecart-type quasi nul : impossible de standardiser.")
        return(apply_simple(function(v) v, function(v) v, "none"))
      }
      apply_simple(function(v) (v - mu) / sd_val, function(v) v * sd_val + mu, "zscore")
    },
    robust_z = {
      med <- stats::median(x_work[finite_mask], na.rm = TRUE)
      mad_val <- stats::mad(x_work[finite_mask], constant = 1.4826, na.rm = TRUE)
      if (!is.finite(mad_val) || mad_val < .Machine$double.eps) {
        add_warning("MAD quasi nul : impossibilité de standardiser de manière robuste.")
        return(apply_simple(function(v) v, function(v) v, "none"))
      }
      apply_simple(function(v) (v - med) / mad_val, function(v) v * mad_val + med, "robust_z")
    },
    log1p = {
      adj <- ensure_domain(x_work, lower_bound = -1)
      results$info$applied_shift <- adj$shift
      if (adj$shift > 0) {
        add_warning(sprintf("Décalage de %.4f appliqué avant log1p.", adj$shift))
      }
      apply_simple(function(v) log1p(v), function(v) expm1(v), "log1p")
    },
    log = {
      adj <- ensure_domain(x_work, lower_bound = 0)
      results$info$applied_shift <- adj$shift
      if (adj$shift > 0) {
        add_warning(sprintf("Décalage de %.4f appliqué avant log.", adj$shift))
      }
      apply_simple(function(v) log(v), function(v) exp(v), "log")
    },
    sqrt = {
      adj <- ensure_domain(x_work, lower_bound = 0)
      results$info$applied_shift <- adj$shift
      if (adj$shift > 0) {
        add_warning(sprintf("Décalage de %.4f appliqué avant sqrt.", adj$shift))
      }
      apply_simple(function(v) sqrt(v), function(v) v^2, "sqrt")
    },
    boxcox = {
      require_if("bestNormalize")
      tf <- bestNormalize::boxcox(x_work[finite_mask], ...)
      out <- x_work
      out[finite_mask] <- tf$x.t
      results$val <- out
      results$param <- tf
      results$info$method_applied <- "boxcox"
      results$functions$forward <- function(new_x) {
        predict(tf, newdata = new_x, warn = FALSE)
      }
      results$functions$inverse <- function(new_z) {
        bestNormalize::predict(tf, newdata = new_z, inverse = TRUE, warn = FALSE)
      }
      results
    },
    yeojohnson = {
      require_if("bestNormalize")
      tf <- bestNormalize::yeojohnson(x_work[finite_mask], ...)
      out <- x_work
      out[finite_mask] <- tf$x.t
      results$val <- out
      results$param <- tf
      results$info$method_applied <- "yeojohnson"
      results$functions$forward <- function(new_x) {
        predict(tf, newdata = new_x, warn = FALSE)
      }
      results$functions$inverse <- function(new_z) {
        bestNormalize::predict(tf, newdata = new_z, inverse = TRUE, warn = FALSE)
      }
      results
    },
    ranknorm = {
      require_if("bestNormalize")
      tf <- bestNormalize::orderNorm(x_work[finite_mask], ...)
      out <- x_work
      out[finite_mask] <- tf$x.t
      results$val <- out
      results$param <- tf
      results$info$method_applied <- "ranknorm"
      results$functions$forward <- function(new_x) {
        predict(tf, newdata = new_x, warn = FALSE)
      }
      results$functions$inverse <- function(new_z) {
        bestNormalize::predict(tf, newdata = new_z, inverse = TRUE, warn = FALSE)
      }
      results
    },
    auto = {
      require_if("bestNormalize")
      tf <- bestNormalize::bestNormalize(x_work[finite_mask], ...)
      out <- x_work
      out[finite_mask] <- tf$x.t
      results$val <- out
      results$param <- tf
      results$info$method_applied <- tf$chosen_transform
      results$info$auto_objective <- tf$chosen_transform_obj
      results$functions$forward <- function(new_x) {
        predict(tf, newdata = new_x, warn = FALSE)
      }
      results$functions$inverse <- function(new_z) {
        bestNormalize::predict(tf, newdata = new_z, inverse = TRUE, warn = FALSE)
      }
      results
    }
  )

  structure(out, class = c("mwi_transform", class(out)))
}


# ------------------------------------------------------------------------------
# 3. DISCRÉTISATION
# ------------------------------------------------------------------------------
#' @title Discrétiser un vecteur numérique
#' @description Crée une variable catégorielle ordinale à partir d'un vecteur
#'   numérique. Les stratégies proposées couvrent les usages classiques en data
#'   science et en sciences sociales : classes à effectifs identiques (quantiles),
#'   intervalles réguliers, coupures manuelles, natural breaks de Jenks, clustering
#'   k-means ou GMM (mixtures gaussiennes). Des métadonnées exhaustives (bornes,
#'   effectifs, avertissements) sont attachées à la sortie pour alimenter diagnostics
#'   ou dictionnaires.
#'
#' @param x Vecteur numérique.
#' @param method Méthode de découpage : `"equal_freq"` (quantiles, défaut),
#'   `"equal_width"`, `"quantile"` (synonyme d’`equal_freq`), `"jenks"`,
#'   `"kmeans"`, `"gmm"`, `"manual"`.
#' @param bins Nombre de classes souhaité (ignoré si `method = "manual"`). Doit
#'   être >= 2.
#' @param breaks Bornes explicites si `method = "manual"`. Seront triées et
#'   dédupliquées.
#' @param labels Libellés à associer aux classes. Si `NULL`, des libellés
#'   `"[a, b)"` sont générés automatiquement.
#' @param weights Optionnel, pondérations positives (longueur identique à `x`)
#'   utilisées pour les quantiles ou Jenks pondérés.
#' @param winsorize Proportion (0–0.5) utilisée pour tronquer les extrêmes avant
#'   calcul des bornes. Exemple : `winsorize = 0.01` coupe aux quantiles 1 % et 99 %.
#' @param min_unique Nombre minimal de valeurs distinctes nécessaires pour
#'   tenter la discrétisation.
#' @param right,include.lowest Paramètres transmis à `cut()`.
#' @param na.rm Si `TRUE`, les valeurs non finies sont exclues du calcul des
#'   bornes; elles restent `NA` dans le facteur retourné.
#' @param verbose Logical ; contrôle l’affichage des messages (utile pour Jenks/GMM).
#' @param return_factor Lorsque `TRUE` (défaut), renvoie un facteur avec métadonnées
#'   attachées; `FALSE` renvoie uniquement la liste de métadonnées.
#' @param ... Arguments additionnels transmis aux fonctions internes (`stats::kmeans`,
#'   `classInt::classIntervals`, `find_clusters`, etc.).
#'
#' @return Par défaut un facteur ordonné; l'attribut `discretize_meta` contient
#'   un tibble/list de synthèse (bornes, tailles de classes, méthode, warnings). En
#'   mode `return_factor = FALSE`, la liste de métadonnées est retournée.
#' @export
discretize_variable <- function(x,
                                method = c("equal_freq", "equal_width", "quantile",
                                           "jenks", "kmeans", "gmm", "manual"),
                                bins = 5L,
                                breaks = NULL,
                                labels = NULL,
                                weights = NULL,
                                winsorize = NULL,
                                min_unique = 3L,
                                right = FALSE,
                                include.lowest = TRUE,
                                na.rm = TRUE,
                                verbose = interactive(),
                                return_factor = TRUE,
                                ...) {
  if (!is.numeric(x)) stop("'x' doit être numérique.")
  method <- match.arg(method)

  if (!return_factor && !is.null(labels)) {
    warning("Les libellés sont ignorés lorsqu'on ne retourne pas le facteur.")
  }

  n_total <- length(x)
  finite_idx <- is.finite(x)
  x_finite <- x[finite_idx]

  weights <- if (is.null(weights)) rep(1, n_total) else weights
  if (!is.numeric(weights) || length(weights) != n_total) {
    stop("'weights' doit être numérique et de même longueur que 'x'.")
  }
  if (any(weights < 0, na.rm = TRUE)) stop("'weights' doit être non négatif.")
  weights_finite <- weights[finite_idx]

  meta <- list(
    method_requested = method,
    bins_requested = bins,
    winsorize = winsorize,
    n_total = n_total,
    n_finite = length(x_finite),
    n_missing = n_total - length(x_finite),
    warnings = character(),
    breaks = NA_real_,
    counts = NA_integer_,
    labels = labels,
    call = match.call()
  )

  add_warning <- function(msg) {
    meta$warnings <<- unique(c(meta$warnings, msg))
  }

  if (length(x_finite) < min_unique || length(unique(x_finite)) < min_unique) {
    add_warning("Pas assez de valeurs distinctes pour discrétiser.")
    return(if (return_factor) {
      result <- factor(rep(NA_character_, n_total))
      attr(result, "discretize_meta") <- meta
      result
    } else meta)
  }

  if (!is.null(winsorize)) {
    if (!is.numeric(winsorize) || length(winsorize) != 1L ||
        winsorize < 0 || winsorize >= 0.5) {
      stop("'winsorize' doit être un scalaire dans [0, 0.5).")
    }
    if (winsorize > 0) {
      lower <- stats::quantile(x_finite, probs = winsorize, type = 7, names = FALSE)
      upper <- stats::quantile(x_finite, probs = 1 - winsorize, type = 7, names = FALSE)
      x_finite <- pmin(pmax(x_finite, lower), upper)
      add_warning(sprintf("Winsorisation appliquée aux quantiles %.2f et %.2f.",
                          winsorize, 1 - winsorize))
    }
  }

  compute_quantiles <- function(values, probs, w = NULL) {
    if (is.null(w)) {
      stats::quantile(values, probs = probs, type = 7, names = FALSE)
    } else {
      ord <- order(values)
      values <- values[ord]
      w <- w[ord]
      w <- w / sum(w, na.rm = TRUE)
      cum_w <- cumsum(w)
      sapply(probs, function(p) {
        idx <- which(cum_w >= p)[1]
        if (is.na(idx)) values[length(values)] else values[idx]
      })
    }
  }

  breaks_vec <- switch(
    method,
    equal_width = {
      if (!is.numeric(bins) || bins < 2) stop("'bins' doit être >= 2 pour equal_width.")
      rng <- range(x_finite, na.rm = TRUE)
      if (diff(rng) == 0) {
        add_warning("Amplitude nulle, fallback sur quantiles.")
        compute_quantiles(x_finite, probs = seq(0, 1, length.out = bins + 1), weights_finite)
      } else {
        seq(rng[1], rng[2], length.out = bins + 1)
      }
    },
    equal_freq = ,
    quantile = {
      if (!is.numeric(bins) || bins < 2) stop("'bins' doit être >= 2 pour quantile.")
      compute_quantiles(x_finite, probs = seq(0, 1, length.out = bins + 1), weights_finite)
    },
    jenks = {
      if (!is.numeric(bins) || bins < 2) stop("'bins' doit être >= 2 pour jenks.")
      if (!requireNamespace("classInt", quietly = TRUE)) {
        stop("Package 'classInt' requis pour la méthode 'jenks'.")
      }
      ci <- tryCatch(
        classInt::classIntervals(
          x_finite,
          n = bins,
          style = "fisher",
          weights = weights_finite,
          verbose = verbose,
          ...
        ),
        error = function(e) {
          add_warning(paste("Echec de l'algorithme de Jenks:", e$message))
          NULL
        }
      )
      if (is.null(ci)) {
        compute_quantiles(x_finite, probs = seq(0, 1, length.out = bins + 1), weights_finite)
      } else {
        ci$brks
      }
    },
    kmeans = {
      if (!is.numeric(bins) || bins < 2) stop("'bins' doit être >= 2 pour kmeans.")
      km <- stats::kmeans(x_finite, centers = bins, ...)
      centres <- sort(km$centers)
      stats::quantile(centres, probs = seq(0, 1, length.out = bins + 1))
    },
    gmm = {
      if (!is.numeric(bins) || bins < 2) stop("'bins' doit être >= 2 pour gmm.")
      gmm_res <- find_clusters(
        x,
        max_G = bins,
        criterion = "bic",
        transform = "auto",
        winsorize = winsorize,
        return_breaks = TRUE,
        verbose = isTRUE(verbose),
        ...
      )
      meta$gmm_details <- gmm_res
      if (is.null(gmm_res$breaks) || length(gmm_res$breaks) < 2) {
        add_warning("GMM n'a pas fourni de bornes distinctes. Fallback quantiles.")
        compute_quantiles(x_finite, probs = seq(0, 1, length.out = bins + 1), weights_finite)
      } else {
        gmm_res$breaks
      }
    },
    manual = {
      if (is.null(breaks)) stop("Pour 'manual', fournir l'argument 'breaks'.")
      as.numeric(breaks)
    }
  )

  breaks_vec <- sort(unique(breaks_vec))
  if (length(breaks_vec) < 2) {
    add_warning("Impossible de déterminer des bornes distinctes; retour NA.")
    return(if (return_factor) {
      result <- factor(rep(NA_character_, n_total))
      attr(result, "discretize_meta") <- meta
      result
    } else meta)
  }

  if (is.null(labels)) {
    intervals <- paste0(
      ifelse(right, "(", "["),
      format(head(breaks_vec, -1), digits = 6, trim = TRUE),
      ", ",
      format(tail(breaks_vec, -1), digits = 6, trim = TRUE),
      ifelse(right, "]", ")")
    )
    labels <- intervals
  } else if (length(labels) != length(breaks_vec) - 1) {
    stop("'labels' doit avoir une longueur égale au nombre de classes (breaks - 1).")
  }

  tmp <- cut(
    x,
    breaks = breaks_vec,
    include.lowest = include_lowest,
    right = right,
    labels = labels
  )

  counts <- tabulate(as.integer(tmp), nbins = length(labels))
  meta$method_applied <- method
  meta$breaks <- breaks_vec
  meta$counts <- counts
  meta$labels <- labels
  meta$na_ratio <- mean(is.na(tmp))
  meta$na_rm <- na.rm

  if (return_factor) {
    attr(tmp, "discretize_meta") <- meta
    tmp
  } else {
    meta
  }
}



# ------------------------------------------------------------------------------
# 4. CLUSTERING (1D)
# ------------------------------------------------------------------------------
#' Identifier des clusters latents dans un vecteur numérique (1D)
#'
#' @description Modèles de mélanges gaussiens 1D avec sélection automatique
#'   du nombre de composantes, transformations préalables, et diagnostics
#'   complets retour. Pensé pour la segmentation socio-économique, la création
#'   de typologies, ou la génération de coupures intelligentes pour la
#'   discrétisation.
#'
#' @param x Numérique.
#' @param max_G Maximum de composantes testées (défaut 6).
#' @param criterion Critère d’ajustement : `"bic"` (défaut) ou `"icl"`.
#' @param min_cluster_prop Proportion minimale d’effectifs par cluster (défaut 0.05).
#' @param transform Transformation appliquée avant clustering : `"none"`,
#'   `"log1p"`, `"yeojohnson"`, `"zscore"`, `"auto"` (via `transform_variable`),
#'   ou fonction utilisateur.
#' @param winsorize Proportion de winsorisation (0–0.5).
#' @param seed Graine optionnelle.
#' @param return_breaks Lorsque `TRUE`, inclut un vecteur de coupures optimales
#'   (midpoints entre centres ordonnés) pour alimenter `discretize_variable`.
#' @param verbose Messages informatifs.
#' @param ... Paramètres additionnels transmis à `mclust::Mclust`.
#'
#' @return Liste avec `fit_object`, `best_model`, `criterion_value`, `n_clusters`,
#'   `classification` (facteur sur l’ensemble des observations, NA inclus pour
#'   valeurs manquantes), `posterior` (probabilités), `transform_info`, `breaks`
#'   (si demandé) et `diagnostics`.
#'
#' @references Fraley, C., & Raftery, A. E. (2002). Model-based clustering, discriminant
#'   analysis, and density estimation. *JASA*.  
#'   McLachlan, G., & Peel, D. (2000). *Finite Mixture Models*. Wiley.  
#'   Celeux, G., & Soromenho, G. (1996). The ICL criterion. *Computational Statistics & Data Analysis*.
#' @export
find_clusters <- function(x,
                          max_G = 6,
                          criterion = c("bic", "icl"),
                          min_cluster_prop = 0.05,
                          transform = c("none", "log1p", "yeojohnson", "zscore", "auto"),
                          winsorize = NULL,
                          seed = NULL,
                          return_breaks = TRUE,
                          verbose = interactive(),
                          ...) {
  if (!is.numeric(x)) stop("'x' doit être numérique.")
  if (!requireNamespace("mclust", quietly = TRUE)) {
    stop("Package 'mclust' requis pour le clustering.")
  }

  criterion <- match.arg(criterion)
  transform <- transform[1]

  total_n <- length(x)
  finite_idx <- is.finite(x)
  x_finite <- x[finite_idx]
  if (length(unique(x_finite)) < 2L) {
    stop("Le vecteur doit contenir au moins deux valeurs finies uniques.")
  }

  if (!is.null(seed)) set.seed(seed)
  clean_transform <- function(values) values

  if (!is.null(winsorize)) {
    if (!is.numeric(winsorize) || winsorize < 0 || winsorize >= 0.5) {
      stop("'winsorize' doit être dans [0, 0.5).")
    }
    qs <- stats::quantile(x_finite, probs = c(winsorize, 1 - winsorize), na.rm = TRUE)
    x_finite <- pmin(pmax(x_finite, qs[1]), qs[2])
  }

  transform_info <- list(method = transform, shift = 0, warnings = character())
  if (!identical(transform, "none")) {
    transform_info <- tryCatch(
      transform_variable(x_finite, method = transform, winsorize = NULL),
      error = function(e) {
        warning("Transformation échouée (", transform, "): ", e$message, ". Utilisation des valeurs brutes.")
        list(val = x_finite, info = list(method_applied = "none", warnings = e$message))
      }
    )
    x_transformed <- transform_info$val
    if (!is.null(transform_info$info$warnings)) {
      warning("Transformation: ", paste(transform_info$info$warnings, collapse = "; "))
    }
    clean_transform <- transform_info$functions$forward %||% clean_transform
  } else {
    x_transformed <- x_finite
  }

  unique_vals <- length(unique(x_transformed))
  max_G <- min(max_G, unique_vals)
  if (max_G < 1L) max_G <- 1L

  fit <- mclust::Mclust(x_transformed, G = 1:max_G, ...)
  if (criterion == "icl") {
    best_idx <- which.max(fit$icl)
    fit$G <- fit$Gvalues[best_idx]
    fit$modelName <- fit$modelNames[best_idx]
  }

  classification <- rep(NA_integer_, total_n)
  classification[finite_idx] <- fit$classification
  clusters <- classification[finite_idx]
  k <- fit$G

  counts <- table(clusters)
  min_count <- min(counts)
  if ((min_count / length(x_finite)) < min_cluster_prop) {
    warning("Un cluster contient moins de ", min_cluster_prop * 100, "% des observations.")
  }

  breaks <- NULL
  if (return_breaks && k > 1L) {
    centres <- aggregate(x_finite, by = list(cluster = clusters), FUN = mean)
    centres <- centres[order(centres$x), ]
    midpoints <- stats::filter(centres$x, rep(1, 2), sides = 1)[-1]
    breaks <- unique(c(min(x_finite), midpoints, max(x_finite)))
  }

  list(
    fit_object = fit,
    best_model = fit$modelName,
    criterion_value = if (criterion == "bic") fit$bic else fit$icl,
    criterion = criterion,
    n_clusters = k,
    classification = factor(classification, levels = sort(unique(classification))),
    posterior = {
      post <- matrix(NA_real_, nrow = total_n, ncol = k)
      post[finite_idx, ] <- fit$z
      post
    },
    breaks = breaks,
    transform_info = transform_info,
    diagnostics = list(
      n_total = total_n,
      n_finite = length(x_finite),
      winsorize = winsorize,
      min_cluster_prop = min_cluster_prop,
      entropy = mclust::entropy(fit)
    )
  )
}

# ------------------------------------------------------------------------------
# 5. ANALYSE DE LOI DE PUISSANCE
# ------------------------------------------------------------------------------

#' @title Sélectionner et valider un modèle de queue lourde
#' @description
#' Analyse la queue d'une distribution positive (comptages ou mesures continues)
#' en comparant plusieurs modèles candidats : loi de puissance, log-normale,
#' exponentielle, et (si disponible) Weibull/stretched-exponential. Un résumé
#' exhaustif est retourné : paramètres estimés, critères d'information (AIC/BIC),
#' p-values de bootstrap goodness-of-fit, comparaisons de vraisemblance et
#' diagnostics de queue. Implémente les recommandations de Clauset, Shalizi &
#' Newman (2009) et de la littérature récente sur les lois de puissance.
#'
#' @param x Vecteur numérique (valeurs positives uniquement seront retenues).
#' @param type `"discrete"` (défaut) ou `"continuous"`, selon la nature de `x`.
#' @param candidate_models Ensemble de modèles à tester. Par défaut :
#'   `c("powerlaw", "lognormal", "exponential", "weibull")` pour les données
#'   continues, et `c("powerlaw", "lognormal", "exponential")` pour les données
#'   discrètes (les modèles indisponibles sont ignorés).
#' @param xmin Valeur minimale fixée pour l'ajustement (sinon estimée par
#'   `poweRlaw::estimate_xmin`).
#' @param bootstrap_sims Nombre de replicats bootstrap pour le test de
#'   goodness-of-fit (défaut 300).
#' @param bootstrap_models Modèles sur lesquels exécuter le bootstrap : par
#'   défaut `c("powerlaw", "best")`.
#' @param threads Nombre de threads (passé à `bootstrap_p`). Par défaut 1.
#' @param winsorize Proportion optionnelle (0–0.5) pour winsoriser les extrêmes
#'   avant estimation (utile en présence de valeurs aberrantes).
#' @param min_n Taille minimale d'échantillon (après nettoyage) pour lancer
#'   l'analyse (défaut 50).
#' @param verbose Afficher les messages d'étapes importantes.
#' @param ... Arguments additionnels transmis à `poweRlaw::Mpl` internes si besoin.
#'
#' @return Une liste contenant :
#'   * `data_summary` : effectifs, résumés descriptifs de `x`.
#'   * `best_model` : nom du modèle retenu (BIC minimal).
#'   * `best_fit` : informations détaillées (paramètres, BIC/AIC, objet poweRlaw).
#'   * `candidates` : liste avec diagnostics pour chaque modèle testé.
#'   * `comparisons` : tableau des ratios de log-vraisemblance (Clauset et al.).
#'   * `bootstrap` : résultats du bootstrap goodness-of-fit exécuté.
#'   * `call` : appel original.
#' @details
#' Chaque modèle est ajusté via `poweRlaw`, avec estimation conjointe de `xmin`
#' (sauf si fixé) et des paramètres. Le meilleur modèle est choisi par BIC, puis
#' évalué via bootstrap. Les comparaisons pairées de log-vraisemblance sont
#' fournies lorsqu'au moins deux modèles sont valides.
#' @references
#' Clauset, A., Shalizi, C. R., & Newman, M. E. J. (2009). *Power-law distributions in
#' empirical data*. SIAM Review.  
#' Clauset, A., & Woodard, R. (2013). *Estimating the historical and future probabilities
#' of large terrorist events*. Annals of Applied Statistics.  
#' Newman, M. E. J. (2005). *Power laws, Pareto distributions and Zipf's law*. Contemporary Physics.
#' @export
analyse_powerlaw <- function(x,
                             type = c("discrete", "continuous"),
                             candidate_models = NULL,
                             xmin = NULL,
                             bootstrap_sims = 300L,
                             bootstrap_models = c("powerlaw", "best"),
                             threads = 1L,
                             winsorize = NULL,
                             min_n = 50L,
                             verbose = interactive(),
                             ...) {
  if (!requireNamespace("poweRlaw", quietly = TRUE)) {
    stop("Le package 'poweRlaw' est requis pour cette analyse.")
  }

  type <- match.arg(type)
  bootstrap_models <- unique(bootstrap_models)

  x_pos <- x[is.finite(x) & x > 0]
  if (length(x_pos) < min_n) {
    stop("Échantillon insuffisant : il faut au moins ", min_n,
         " observations positives et finies (n = ", length(x_pos), ").")
  }

  if (!is.null(winsorize)) {
    if (!is.numeric(winsorize) || length(winsorize) != 1L ||
        winsorize < 0 || winsorize >= 0.5) {
      stop("'winsorize' doit être un scalaire dans [0, 0.5).")
    }
    qs <- stats::quantile(x_pos, probs = c(winsorize, 1 - winsorize),
                          names = FALSE, type = 7)
    x_pos <- pmin(pmax(x_pos, qs[1]), qs[2])
    if (isTRUE(verbose)) {
      message("Winsorisation appliquée aux quantiles ",
              format(qs[1], digits = 4), "–", format(qs[2], digits = 4))
    }
  }

  default_candidates <- if (type == "discrete") {
    c("powerlaw", "lognormal", "exponential")
  } else {
    c("powerlaw", "lognormal", "exponential", "weibull")
  }
  if (is.null(candidate_models)) {
    candidate_models <- default_candidates
  }
  candidate_models <- intersect(unique(candidate_models), default_candidates)
  if (length(candidate_models) == 0L) {
    stop("Aucun modèle candidat valide n'a été spécifié.")
  }

  model_generators <- list(
    discrete = list(
      powerlaw   = "displ",
      lognormal  = "dislnorm",
      exponential = "disexp"
    ),
    continuous = list(
      powerlaw   = "conpl",
      lognormal  = "conlnorm",
      exponential = "conexp",
      weibull    = "conweibull"
    )
  )

  build_model <- function(name, data) {
    gen_name <- model_generators[[type]][[name]]
    if (is.null(gen_name)) {
      return(NULL)
    }
    ns <- getNamespace("poweRlaw")
    if (!exists(gen_name, envir = ns, inherits = FALSE)) {
      return(NULL)
    }
    generator <- get(gen_name, envir = ns)
    generator$new(data)
  }

  fit_single_model <- function(model_name) {
    model_obj <- build_model(model_name, x_pos)
    if (is.null(model_obj)) {
      if (isTRUE(verbose)) {
        message("Modèle '", model_name,
                "' indisponible pour le type '", type, "', ignoré.")
      }
      return(NULL)
    }

    tail_xmin <- xmin
    if (is.null(tail_xmin)) {
      est_xmin <- poweRlaw::estimate_xmin(model_obj)
      tail_xmin <- est_xmin$xmin
      model_obj$setXmin(est_xmin)
    } else {
      model_obj$setXmin(tail_xmin)
    }

    est_pars <- poweRlaw::estimate_pars(model_obj)
    model_obj$setPars(est_pars)

    n_tail <- sum(model_obj$dat >= model_obj$getXmin())
    loglik <- as.numeric(poweRlaw::logLik(model_obj))
    k <- length(model_obj$getPars())
    aic <- -2 * loglik + 2 * k
    bic <- -2 * loglik + k * log(n_tail)

    ks <- poweRlaw::calc_ks(model_obj)

    list(
      name = model_name,
      object = model_obj,
      xmin = model_obj$getXmin(),
      pars = model_obj$getPars(),
      n_tail = n_tail,
      loglik = loglik,
      aic = aic,
      bic = bic,
      ks = ks,
      gof = NA_real_
    )
  }

  candidates <- lapply(candidate_models, fit_single_model)
  names(candidates) <- candidate_models
  candidates <- Filter(Negate(is.null), candidates)

  if (length(candidates) == 0L) {
    stop("Aucun modèle n'a pu être ajusté (vérifiez vos paramètres).")
  }

  scores <- sapply(candidates, function(m) m$bic)
  best_idx <- which.min(scores)
  best_name <- names(candidates)[best_idx]
  best_model <- candidates[[best_idx]]

  if (isTRUE(verbose)) {
    message("Modèle retenu (BIC minimal) : ", best_name,
            " | xmin = ", format(best_model$xmin, digits = 4),
            " | n_tail = ", best_model$n_tail)
  }

  comparison_table <- NULL
  if (length(candidates) >= 2L) {
    combos <- utils::combn(names(candidates), 2L)
    comp_rows <- vector("list", ncol(combos))
    for (i in seq_len(ncol(combos))) {
      m1 <- candidates[[combos[1L, i]]]
      m2 <- candidates[[combos[2L, i]]]
      res <- try(
        poweRlaw::compare_distributions(m1$object, m2$object),
        silent = TRUE
      )
      comp_rows[[i]] <- data.frame(
        model_1 = combos[1L, i],
        model_2 = combos[2L, i],
        loglik_ratio = if (inherits(res, "try-error")) NA_real_ else res$test_statistic,
        p_two_sided = if (inherits(res, "try-error")) NA_real_ else res$p_two_sided,
        stringsAsFactors = FALSE
      )
    }
    comparison_table <- do.call(rbind, comp_rows)
  }

  bootstrap_targets <- intersect(unique(c(bootstrap_models, best_name)),
                                 names(candidates))
  bootstrap_results <- NULL
  if (length(bootstrap_targets) > 0L && bootstrap_sims > 0L) {
    bootstrap_rows <- vector("list", length(bootstrap_targets))
    for (i in seq_along(bootstrap_targets)) {
      target <- bootstrap_targets[i]
      mod <- candidates[[target]]$object
      boot <- poweRlaw::bootstrap_p(
        mod,
        no_of_sims = bootstrap_sims,
        threads = threads,
        quiet = !isTRUE(verbose)
      )
      candidates[[target]]$gof <- boot$p
      bootstrap_rows[[i]] <- data.frame(
        model = target,
        p_value = boot$p,
        se = boot$se,
        sims = bootstrap_sims,
        stringsAsFactors = FALSE
      )
    }
    bootstrap_results <- do.call(rbind, bootstrap_rows)
  }

  data_summary <- list(
    n_total = length(x),
    n_positive = length(x_pos),
    min = min(x_pos),
    max = max(x_pos),
    median = stats::median(x_pos),
    mean = mean(x_pos),
    type = type
  )

  list(
    data_summary = data_summary,
    best_model = best_name,
    best_fit = candidates[[best_name]],
    candidates = candidates,
    comparisons = comparison_table,
    bootstrap = bootstrap_results,
    call = match.call()
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


#' Recode Cell Values Using OpenRouter
#'
#' @description
#' Calls an OpenRouter-hosted chat model to transform a single value according
#' to a natural-language prompt. Mirrors `GPT_Recode` but uses the generic
#' OpenRouter REST API, with robust validation, retry logic, and optional
#' response sanity checks.
#'
#' @param prompt Instruction describing the desired transformation.
#' @param cell Scalar character string to recode.
#' @param sysprompt System prompt conditioning the assistant. Defaults to a
#'   “return only the transformed value” directive.
#' @param model OpenRouter model identifier (default `"openrouter/auto/gpt-4"`).
#' @param temperature Sampling temperature in [0, 2].
#' @param max_tokens Maximum tokens to generate.
#' @param max_retries Retry attempts when the API call fails.
#' @param retry_delay Seconds to wait between retries.
#' @param validate Logical; when TRUE, applies simple heuristics to flag odd replies.
#' @param referer Optional string for the `HTTP-Referer` header (OpenRouter requirement).
#' @param title Optional string for the `X-Title` header.
#' @param api_base Override the default API base URL if OpenRouter changes it.
#'
#' @return Character string with the recoded value, or `NA_character_` on failure.
#'
#' @note Requires `OPENROUTER_API_KEY` to be set and the `httr` + `jsonlite`
#'   packages installed. Respect OpenRouter’s usage policies and rate limits.
#'
#' @export
OpenRouter_Recode <- function(prompt,
                              cell,
                              sysprompt = "You are a helpful assistant that recodes dataframe values. Return only the transformed value.",
                              model = "openrouter/auto/gpt-4",
                              temperature = 0.8,
                              max_tokens = 1000,
                              max_retries = 3,
                              retry_delay = 1,
                              validate = TRUE,
                              referer = getOption("openrouter.referer", "https://github.com/MyWebIntelligence"),
                              title = getOption("openrouter.title", "mwiR"),
                              api_base = "https://openrouter.ai/api/v1/chat/completions") {
  if (!is.character(prompt) || length(prompt) != 1L || nchar(prompt) == 0) {
    stop("'prompt' must be a non-empty character string")
  }
  if (!is.character(cell) || length(cell) != 1L) {
    stop("'cell' must be a character string")
  }
  if (!is.numeric(temperature) || temperature < 0 || temperature > 2) {
    stop("'temperature' must be between 0 and 2")
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required for OpenRouter_Recode()")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for OpenRouter_Recode()")
  }

  api_key <- Sys.getenv("OPENROUTER_API_KEY", unset = "")
  if (api_key == "") {
    stop("OPENROUTER_API_KEY environment variable not set. Use Sys.setenv() before calling OpenRouter_Recode().")
  }

  messages <- list(
    list(role = "system", content = sysprompt),
    list(role = "user", content = paste0(prompt, ":\n\n", cell))
  )
  payload <- list(
    model = model,
    messages = messages,
    temperature = temperature,
    max_tokens = max_tokens,
    stream = FALSE
  )

  headers <- httr::add_headers(
    "Authorization" = paste("Bearer", api_key),
    "Content-Type" = "application/json",
    "Accept" = "application/json",
    "HTTP-Referer" = referer %||% "https://github.com/MyWebIntelligence",
    "X-Title" = title %||% "mwiR"
  )

  for (attempt in seq_len(max_retries)) {
    resp <- try(httr::POST(
      url = api_base,
      headers,
      body = jsonlite::toJSON(payload, auto_unbox = TRUE, digits = NA),
      encode = "raw",
      httr::timeout(60)
    ), silent = TRUE)

    if (inherits(resp, "try-error")) {
      if (attempt == max_retries) {
        warning("OpenRouter request failed: ", conditionMessage(attr(resp, "condition")))
        return(NA_character_)
      }
      Sys.sleep(retry_delay)
      next
    }

    status <- httr::status_code(resp)
    body_txt <- httr::content(resp, as = "text", encoding = "UTF-8")
    parsed <- try(jsonlite::fromJSON(body_txt, simplifyVector = FALSE), silent = TRUE)

    if (status >= 200 && status < 300 && !inherits(parsed, "try-error")) {
      choices <- parsed$choices
      if (is.null(choices) || length(choices) == 0) {
        warning("OpenRouter returned an empty choices array.")
        return(NA_character_)
      }
      transformed <- choices[[1]]$message$content %||% ""
      if (validate) {
        ok <- nchar(transformed) > 0 &&
          nchar(transformed) < max(10L, nchar(cell) * 10L) &&
          !grepl("sorry|error|cannot|not able", transformed, ignore.case = TRUE)
        if (!ok) {
          warning("OpenRouter returned a potentially invalid response.")
          return(NA_character_)
        }
      }
      return(transformed)
    } else {
      err_msg <- if (!inherits(parsed, "try-error") && !is.null(parsed$error$message)) {
        parsed$error$message
      } else {
        substr(body_txt, 1, 200)
      }
      if (attempt == max_retries || status %in% c(401, 403)) {
        warning("OpenRouter recoding failed (status ", status, "): ", err_msg)
        return(NA_character_)
      }
      Sys.sleep(retry_delay)
    }
  }

  NA_character_
}