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
    lang <- rep(NA_character_, length(texts))
    prob <- rep(NA_real_, length(texts))
    for (i in seq_along(texts)) {
      entry <- try(cld3::detect_language_mixed(texts[i], size = 3L), silent = TRUE)
      if (inherits(entry, "try-error") || is.null(entry) || nrow(entry) == 0L) {
        next
      }
      entry <- entry[order(entry$probability, decreasing = TRUE), , drop = FALSE]
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
      if (is.na(lang_i)) {
        prob_i <- NA_real_
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
    if (display) grid::grid.draw(combined)

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
#'   1 \% et 99 \%. `NULL` (défaut) désactive l'opération.
#' @param shift_constant Constante strictement positive ajoutée lorsque l'on
#'   doit décaler une série vers le domaine de définition (log, sqrt, log1p).
#' @param handle_na Comment traiter les valeurs manquantes : `"keep"` (défaut)
#'   conserve les NA dans `val`, `"omit"` les supprime avant estimation des
#'   paramètres (recommandé quand on alimente un modèle qui gère lui-même les NA).
#' @param ... Arguments additionnels passés à `bestNormalize::bestNormalize`,
#'   `bestNormalize::boxcox`, `bestNormalize::yeojohnson` ou `bestNormalize::orderNorm`
#'   selon le cas.
#'
#' @return
#' An object of class `mwi_transform` containing:
#' \describe{
#'   \item{val}{numeric vector, transformed version of `x`.}
#'   \item{param}{transformer object or parameters needed to invert the transform.}
#'   \item{info}{list of metadata (method applied, shifts, winsorisation, etc.).}
#'   \item{functions}{list of closures `forward(new_x)` and `inverse(new_z)`.}
#' }
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
# 2b. DIAGNOSTIC DE VARIABLE
# ------------------------------------------------------------------------------
#' Diagnose a Numeric Variable
#'
#' @description Computes descriptive statistics and normality tests for a numeric
#'   vector. Returns a list with count of valid observations, skewness, kurtosis,
#'   and Shapiro-Wilk p-value.
#'
#' @param x Numeric vector to diagnose.
#' @param na.rm Logical; if TRUE, NA values are removed before computation.
#'
#' @return A list with:
#' \describe{
#'   \item{n_valid}{Number of non-NA finite values.}
#'   \item{n_missing}{Number of NA values.}
#'   \item{mean}{Mean of valid values.}
#'   \item{sd}{Standard deviation of valid values.}
#'   \item{min}{Minimum value.}
#'   \item{max}{Maximum value.}
#'   \item{skewness}{Skewness (requires at least 3 valid values).}
#'   \item{kurtosis}{Kurtosis (requires at least 4 valid values).}
#'   \item{shapiro_wilk_p}{Shapiro-Wilk test p-value (requires 3-5000 values).}
#' }
#'
#' @export
diagnose_variable <- function(x, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("'x' must be numeric.")

  }

  x_valid <- x[is.finite(x)]
  n_valid <- length(x_valid)
  n_missing <- sum(is.na(x))

  result <- list(
    n_valid = n_valid,
    n_missing = n_missing,
    mean = if (n_valid > 0) mean(x_valid) else NA_real_,
    sd = if (n_valid > 1) stats::sd(x_valid) else NA_real_,
    min = if (n_valid > 0) min(x_valid) else NA_real_,
    max = if (n_valid > 0) max(x_valid) else NA_real_,
    skewness = NA_real_,
    kurtosis = NA_real_,
    shapiro_wilk_p = NA_real_
  )

  # Skewness requires at least 3 values

if (n_valid >= 3) {
    if (requireNamespace("moments", quietly = TRUE)) {
      result$skewness <- moments::skewness(x_valid)
    }
  }

  # Kurtosis requires at least 4 values
  if (n_valid >= 4) {
    if (requireNamespace("moments", quietly = TRUE)) {
      result$kurtosis <- moments::kurtosis(x_valid)
    }
  }

  # Shapiro-Wilk test requires 3-5000 observations
  if (n_valid >= 3 && n_valid <= 5000) {
    tryCatch({
      sw_test <- stats::shapiro.test(x_valid)
      result$shapiro_wilk_p <- sw_test$p.value
    }, error = function(e) {
      # Keep NA if test fails
    })
  }

  result
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
#'   calcul des bornes. Exemple : `winsorize = 0.01` coupe aux quantiles 1 \% et 99 \%.
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
#' @return
#' By default an ordered factor; the attribute `discretize_meta` stores summary
#' information. When `return_factor = FALSE`, a list is returned with:
#' \describe{
#'   \item{method_applied}{method finally used.}
#'   \item{breaks}{numeric vector of bin edges.}
#'   \item{counts}{class frequencies.}
#'   \item{warnings}{character vector of notes emitted during discretisation.}
#' }
#' 
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
    warning(msg, call. = FALSE)
  }

  if (length(x_finite) < min_unique || length(unique(x_finite)) < min_unique) {
    add_warning("Not enough unique values to discretize.")
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
    include.lowest = include.lowest,
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

  classification_entropy <- function(z) {
    if (is.null(z)) return(NA_real_)
    z <- as.matrix(z)
    if (!is.numeric(z)) return(NA_real_)
    z[z <= 0] <- .Machine$double.eps
    -mean(rowSums(z * log(z)), na.rm = TRUE)
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
      entropy = classification_entropy(fit$z)
    )
  )
}

# ------------------------------------------------------------------------------
# 5. ANALYSE DE LOI DE PUISSANCE
# ------------------------------------------------------------------------------

#' Detect optimal number of threads for parallel processing
#'
#' On Apple Silicon, uses only performance cores (not efficiency cores).
#' On other systems, uses half of available cores minus one.
#'
#' @return Integer number of recommended threads
#' @keywords internal
.detect_optimal_threads <- function() {

  n_cores <- parallel::detectCores(logical = FALSE)

  if (Sys.info()["sysname"] == "Darwin") {
    # Apple Silicon: use only performance cores
    perf_cores <- tryCatch({
      as.integer(system("sysctl -n hw.perflevel0.physicalcpu", intern = TRUE))
    }, error = function(e) NULL, warning = function(w) NULL)

    if (!is.null(perf_cores) && !is.na(perf_cores) && perf_cores > 0) {
      return(max(1L, perf_cores - 1L))  # Leave 1 P-core free
    }
  }

  # Fallback: half of cores - 1
  max(1L, as.integer(floor(n_cores / 2) - 1L))
}

#' Safe bootstrap_p wrapper that fixes poweRlaw parallel bug
#'
#' The poweRlaw package has a bug where `dist_rand` (a reference class method)
#' cannot be found by parallel workers. This wrapper implements a custom
#' sequential bootstrap that avoids the parallel bug entirely.
#'
#' @param model A poweRlaw distribution object with xmin and pars set
#' @param no_of_sims Number of bootstrap simulations
#' @param threads Number of parallel threads (currently ignored, always sequential)
#' @param seed Random seed for reproducibility
#' @param verbose Print progress messages
#' @return List with p-value, se, gof statistic, and bootstrap results
#' @keywords internal
.safe_bootstrap_p <- function(model, no_of_sims, threads = 1L, seed = NULL, verbose = FALSE) {

  if (!is.null(seed)) set.seed(seed)

  m_copy <- model$copy()

  # Check parameters are set
  if (is.null(m_copy$getPars()) || is.null(m_copy$getXmin())) {
    est <- suppressWarnings(poweRlaw::estimate_xmin(m_copy))
    m_copy$setXmin(est)
  }

  # Get model info
  m_data <- m_copy$dat
  m_xmin <- m_copy$getXmin()
  m_pars <- m_copy$getPars()
  x_lower <- m_data[m_data < m_xmin]

  n <- length(m_data)
  ntail <- sum(m_data >= m_xmin)
  ntail_prop <- ntail / n

  # Calculate original GOF (KS statistic)
  gof_original <- tryCatch(
    poweRlaw::get_distance_statistic(m_copy),
    error = function(e) {
      tryCatch(poweRlaw::get_KS_statistic(m_copy), error = function(e2) NA_real_)
    }
  )

  # Run sequential bootstrap
  gof_values <- numeric(no_of_sims)

  for (i in seq_len(no_of_sims)) {
    tryCatch({
      # Sample from empirical distribution below xmin
      n1 <- sum(runif(n) > ntail_prop)

      # Generate bootstrap sample
      sampled_lower <- if (n1 > 0 && length(x_lower) > 0) {
        sample(x_lower, n1, replace = TRUE)
      } else {
        numeric(0)
      }

      # Generate from fitted distribution above xmin
      sampled_upper <- poweRlaw::dist_rand(m_copy, n - n1)
      q <- c(sampled_lower, sampled_upper)

      # Fit model on bootstrap sample
      m_boot <- m_copy$getRefClass()$new(q)
      # Suppress "xmin search space truncated" warnings during bootstrap
      est_boot <- suppressWarnings(poweRlaw::estimate_xmin(m_boot))

      gof_values[i] <- if (!is.null(est_boot$gof) && !is.na(est_boot$gof)) {
        est_boot$gof
      } else {
        NA_real_
      }
    }, error = function(e) {
      gof_values[i] <<- NA_real_
    })
  }

  # Calculate p-value: proportion of bootstrap GOF >= original GOF
  valid_gof <- gof_values[!is.na(gof_values)]
  if (length(valid_gof) > 0) {
    p_value <- sum(valid_gof >= gof_original) / length(valid_gof)
    se_value <- sqrt(p_value * (1 - p_value) / length(valid_gof))
  } else {
    p_value <- NA_real_
    se_value <- NA_real_
  }

  list(
    p = p_value,
    se = se_value,
    gof = gof_original,
    bootstraps = data.frame(gof = gof_values)
  )
}

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
#' @param threads Nombre de threads pour le bootstrap. Par défaut `NULL` pour
#'   auto-détection (utilise les P-cores sur Apple Silicon, sinon moitié des cœurs).
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
                             threads = NULL,
                             winsorize = NULL,
                             min_n = 50L,
                             verbose = interactive(),
                             ...) {
  if (!requireNamespace("poweRlaw", quietly = TRUE)) {
    stop("Le package 'poweRlaw' est requis pour cette analyse.")
  }

  type <- match.arg(type)
  bootstrap_models <- unique(bootstrap_models)


  # Auto-detect optimal thread count if not specified
  if (is.null(threads)) {
    threads <- .detect_optimal_threads()
    if (isTRUE(verbose)) {
      message("[analyse_powerlaw] Utilisation de ", threads, " thread(s) pour le bootstrap")
    }
  }
  threads <- as.integer(max(1L, threads))

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

  if (type == "discrete") {
    x_pos <- round(x_pos)
    x_pos <- x_pos[x_pos >= 1]
    if (length(x_pos) < min_n) {
      stop("Après arrondi et filtrage des valeurs non positives, il reste seulement ",
           length(x_pos), " observations discretes. Augmentez 'min_n' ou revoyez vos données.")
    }
    if (!all(abs(x_pos - round(x_pos)) < .Machine$double.eps^0.5)) {
      stop("Les données doivent être entières pour l'analyse discrète. Utilisez 'round' ou passez en mode continu.")
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
      est_xmin <- suppressWarnings(
        suppressPackageStartupMessages(
          suppressMessages(
            poweRlaw::estimate_xmin(model_obj, xmax = max(x_pos))
          )
        )
      )
      tail_xmin <- est_xmin$xmin
      model_obj$setXmin(est_xmin)
    } else {
      model_obj$setXmin(tail_xmin)
    }

    est_pars <- poweRlaw::estimate_pars(model_obj)
    model_obj$setPars(est_pars)

    n_tail <- sum(model_obj$dat >= model_obj$getXmin())
    loglik <- as.numeric(poweRlaw::dist_ll(model_obj))
    k <- length(model_obj$getPars())
    aic <- -2 * loglik + 2 * k
    bic <- -2 * loglik + k * log(n_tail)

    ks <- tryCatch(
      poweRlaw::distance_ks(model_obj),
      error = function(e) NA_real_
    )

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

  if (isTRUE(verbose)) {
    message("[analyse_powerlaw] Ajustement des modèles candidats (", length(candidate_models), ")...")
  }

  progress_candidates <- NULL
  if (interactive() && length(candidate_models) > 1L && isTRUE(verbose)) {
    progress_candidates <- utils::txtProgressBar(min = 0, max = length(candidate_models), style = 3)
  }

  candidates <- vector("list", length(candidate_models))
  names(candidates) <- candidate_models
  for (i in seq_along(candidate_models)) {
    candidates[[i]] <- fit_single_model(candidate_models[i])
    if (!is.null(progress_candidates)) utils::setTxtProgressBar(progress_candidates, i)
  }
  if (!is.null(progress_candidates)) close(progress_candidates)

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

      # compare_distributions requires both models to have the same xmin
      # We use the xmin from the first model and re-fit the second model
      m2_copy <- m2$object$copy()
      m2_copy$setXmin(m1$object$getXmin())
      m2_pars <- tryCatch(
        poweRlaw::estimate_pars(m2_copy),
        error = function(e) NULL
      )

      res <- NULL
      if (!is.null(m2_pars)) {
        m2_copy$setPars(m2_pars)
        res <- tryCatch(
          poweRlaw::compare_distributions(m1$object, m2_copy),
          error = function(e) NULL
        )
      }

      comp_rows[[i]] <- data.frame(
        model_1 = combos[1L, i],
        model_2 = combos[2L, i],
        loglik_ratio = if (is.null(res)) NA_real_ else res$test_statistic,
        p_two_sided = if (is.null(res)) NA_real_ else res$p_two_sided,
        stringsAsFactors = FALSE
      )
    }
    comparison_table <- do.call(rbind, comp_rows)
  }

  bootstrap_targets <- intersect(unique(c(bootstrap_models, best_name)),
                                 names(candidates))
  bootstrap_results <- NULL
  if (length(bootstrap_targets) > 0L && bootstrap_sims > 0L) {
    if (isTRUE(verbose)) {
      message("[analyse_powerlaw] Bootstrap goodness-of-fit (", bootstrap_sims, " simulations)...")
    }
    progress_boot <- NULL
    if (interactive() && length(bootstrap_targets) > 1L && isTRUE(verbose)) {
      progress_boot <- utils::txtProgressBar(min = 0, max = length(bootstrap_targets), style = 3)
    }
    old_profile <- Sys.getenv("R_PROFILE_USER", unset = NA_character_)
    Sys.setenv(R_PROFILE_USER = "")
    on.exit({
      if (is.na(old_profile)) {
        Sys.unsetenv("R_PROFILE_USER")
      } else {
        Sys.setenv(R_PROFILE_USER = old_profile)
      }
    }, add = TRUE)
    bootstrap_rows <- vector("list", length(bootstrap_targets))
    for (i in seq_along(bootstrap_targets)) {
      target <- bootstrap_targets[i]
      mod <- candidates[[target]]$object
      # Suppress all warnings from poweRlaw during bootstrap
      boot <- suppressWarnings(
        .safe_bootstrap_p(
          model = mod,
          no_of_sims = bootstrap_sims,
          threads = threads,
          verbose = verbose
        )
      )
      p_val <- boot$p
      se_val <- if (!is.null(boot$se)) boot$se else NA_real_
      if (length(p_val) == 0L) p_val <- NA_real_
      if (length(se_val) == 0L) se_val <- NA_real_
      candidates[[target]]$gof <- p_val
      bootstrap_rows[[i]] <- data.frame(
        model = target,
        p_value = p_val,
        se = se_val,
        sims = bootstrap_sims,
        stringsAsFactors = FALSE
      )
      if (!is.null(progress_boot)) utils::setTxtProgressBar(progress_boot, i)
    }
    bootstrap_results <- do.call(rbind, bootstrap_rows)
    if (!is.null(progress_boot)) close(progress_boot)
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

  # Extract convenience fields from the best model
  best <- candidates[[best_name]]

  list(
    data_summary = data_summary,
    best_model = best_name,
    best_fit = best,
    # Top-level convenience fields for common access patterns
    xmin = best$xmin,
    alpha = best$alpha,
    gof_p_value = if (!is.null(best$gof)) best$gof else NA_real_,
    candidates = candidates,
    comparisons = comparison_table,
    bootstrap = bootstrap_results,
    call = match.call()
  )
}



#' Fetch SEO data for a single URL (internal helper)
#'
#' @param url URL to fetch SEO data for
#' @param api_key API key for SEO Rank service
#' @return A data frame with SEO metrics or error information
#' @keywords internal
.fetch_seo_single <- function(url, api_key) {
  api_url <- paste0("https://seo-rank.my-addr.com/api2/moz+sr+fb/", api_key, "/", url)

  tryCatch({
    response <- httr::GET(api_url, httr::timeout(30))
    if (httr::status_code(response) == 200) {
      data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
      # Convert all fields to character and add metadata
      result <- as.data.frame(lapply(data, as.character), stringsAsFactors = FALSE)
      result$url <- url
      result$fetch_status <- "success"
      result
    } else {
      data.frame(
        url = url,
        fetch_status = paste0("http_error_", httr::status_code(response)),
        stringsAsFactors = FALSE
      )
    }
  }, error = function(e) {
    data.frame(
      url = url,
      fetch_status = paste0("error: ", e$message),
      stringsAsFactors = FALSE
    )
  })
}

#' Fetch SEO Rank Data for URLs
#'
#' This function retrieves SEO rank data for a list of URLs using the SEO Rank API
#' and writes the results to a CSV file. Supports parallel processing for faster
#' execution on large URL lists.
#'
#' @param filename A character string specifying the name of the output CSV file
#'   (without extension). If NULL, the function will stop and prompt for input.
#' @param urls A character vector of URLs to fetch SEO rank data for. If NULL,
#'   the function will stop and prompt for input.
#' @param api_key A character string containing the API key for the SEO Rank API.
#'   If NULL, the function will stop and prompt for input.
#' @param parallel Logical. If TRUE (default), use parallel processing with multiple
#'   workers. Set to FALSE for sequential processing.
#' @param workers Integer. Number of parallel workers. If NULL (default), uses
#'   `availableCores() - 1`. Ignored if `parallel = FALSE`.
#' @param progress Logical. If TRUE (default), display a progress bar.
#' @param rate_limit_delay Numeric. Delay in seconds between API calls (default 0.2).
#'   Used only in sequential mode; parallel mode relies on natural distribution.
#'
#' @return Invisibly returns a data frame with all fetched data. Also writes the
#'   results to a CSV file.
#'
#' @details
#' The function supports two processing modes:
#' \itemize{
#'   \item \strong{Parallel mode} (default): Uses `future` and `furrr` packages to
#'     process multiple URLs simultaneously. Significantly faster for large URL lists.
#'   \item \strong{Sequential mode}: Processes URLs one at a time with configurable
#'     delay between requests.
#' }
#'
#' Results are written to CSV in a single batch operation after all URLs are processed,
#' which is more efficient than row-by-row writing.
#'
#' @note
#' This function requires an active internet connection and a valid API key from
#' https://seo-rank.my-addr.com/.
#'
#' @examples
#' \dontrun{
#' # Parallel fetch (default, fastest)
#' mwir_seorank("my_seo_data", c("example.com", "example.org"), "YOUR_API_KEY")
#'
#' # Sequential fetch with progress
#' mwir_seorank("my_seo_data", urls, "YOUR_API_KEY", parallel = FALSE)
#'
#' # Parallel with custom worker count
#' mwir_seorank("my_seo_data", urls, "YOUR_API_KEY", workers = 4)
#' }
#'
#' @importFrom httr GET status_code content timeout
#' @importFrom jsonlite fromJSON
#' @importFrom readr write_csv
#'
#' @export
mwir_seorank <- function(filename = NULL,
                         urls = NULL,
                         api_key = NULL,
                         parallel = TRUE,
                         workers = NULL,
                         progress = TRUE,
                         rate_limit_delay = 0.2) {

  # Input validation
  if (is.null(filename) || !is.character(filename) || nchar(filename) == 0) {
    stop("filename needed for export. Please enter a filename ex. 'myproject' without extension")
  }

  if (is.null(urls) || !is.character(urls) || length(urls) == 0) {
    stop("no urls given")
  }

  if (is.null(api_key) || !is.character(api_key) || nchar(api_key) == 0) {
    stop("need an api key for 'https://seo-rank.my-addr.com/' API services")
  }

  n <- length(urls)
  message("=== SEO Rank Fetch ===")
  message("URLs to process: ", n)
  message("Mode: ", if (parallel) paste0("parallel (", workers %||% (future::availableCores() - 1), " workers)") else "sequential")
  message("----------------------")

  results <- vector("list", n)

  if (parallel && n > 1) {
    # Parallel processing with future + furrr
    if (is.null(workers)) workers <- max(1, future::availableCores() - 1)
    oplan <- future::plan(future::multisession, workers = workers)
    on.exit(future::plan(oplan), add = TRUE)

    if (progress) {
      progressr::handlers(global = TRUE)
      results <- progressr::with_progress({
        p <- progressr::progressor(steps = n)
        furrr::future_map(urls, function(url) {
          p()
          .fetch_seo_single(url, api_key)
        }, .options = furrr::furrr_options(seed = TRUE))
      })
    } else {
      results <- furrr::future_map(urls, function(url) {
        .fetch_seo_single(url, api_key)
      }, .options = furrr::furrr_options(seed = TRUE))
    }
  } else {
    # Sequential processing with progress bar
    pb <- NULL
    if (progress && interactive()) {
      pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
      on.exit(if (!is.null(pb)) close(pb), add = TRUE)
    }

    for (i in seq_len(n)) {
      if (rate_limit_delay > 0 && i > 1) {
        Sys.sleep(rate_limit_delay)
      }

      results[[i]] <- .fetch_seo_single(urls[i], api_key)

      if (!is.null(pb)) utils::setTxtProgressBar(pb, i)
    }
  }

  # Combine all results into a single data frame
  # Handle varying columns by using dplyr::bind_rows or base R equivalent
  all_cols <- unique(unlist(lapply(results, names)))
  combined <- do.call(rbind, lapply(results, function(df) {
    missing_cols <- setdiff(all_cols, names(df))
    for (col in missing_cols) {
      df[[col]] <- NA_character_
    }
    df[, all_cols, drop = FALSE]
  }))

  # Write to CSV (single batch write)
  output_file <- paste0(filename, ".csv")
  readr::write_csv(combined, output_file)

  # Summary
  success_count <- sum(combined$fetch_status == "success", na.rm = TRUE)
  error_count <- n - success_count
  message("----------------------")
  message("Done! ", success_count, "/", n, " URLs fetched successfully")
  if (error_count > 0) {
    message("Errors: ", error_count, " (see fetch_status column)")
  }
  message("Results saved to: ", output_file)

  invisible(combined)
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

# =============================================================================
# LLM_Recode - Unified LLM-based data recoding for social science researchers
# =============================================================================

# -----------------------------------------------------------------------------
# Null-coalescing operator (internal)
# -----------------------------------------------------------------------------
`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0L) {
    if (is.function(a) || !is.na(a[1])) a else b
  } else {
    b
  }
}

# -----------------------------------------------------------------------------
# Provider registry (internal)
# -----------------------------------------------------------------------------
.llm_providers <- list(
  openai = list(
    env_key = "OPENAI_API_KEY",
    default_model = "gpt-4o",
    default_base = "https://api.openai.com/v1/chat/completions"
  ),
  openrouter = list(
    env_key = "OPENROUTER_API_KEY",
    default_model = "openrouter/auto",
    default_base = "https://openrouter.ai/api/v1/chat/completions"
  ),
  anthropic = list(
    env_key = "ANTHROPIC_API_KEY",
    default_model = "claude-sonnet-4-20250514",
    default_base = "https://api.anthropic.com/v1/messages"
  ),
  ollama = list(
    env_key = NULL,
    default_model = "llama3",
    default_base = "http://localhost:11434/api/chat"
  )
)

# -----------------------------------------------------------------------------
# Bilingual messages (internal)
# -----------------------------------------------------------------------------
.llm_messages <- list(
  fr = list(
    no_api_key = "Aucune cl\u00e9 API trouv\u00e9e pour {provider}.",
    ask_key = "Entrez votre cl\u00e9 API (ou appuyez sur Entr\u00e9e pour annuler): ",
    key_saved = "Cl\u00e9 API enregistr\u00e9e pour cette session R.",
    key_permanent = "Pour la rendre permanente, ajoutez \u00e0 votre .Renviron:",
    cancelled = "Op\u00e9ration annul\u00e9e par l'utilisateur.",
    rate_limit = "Limite de requ\u00eates atteinte. Pause de {delay} secondes...",
    retry = "Tentative {attempt}/{max} \u00e9chou\u00e9e: {error}",
    success = "Traitement termin\u00e9: {n} \u00e9l\u00e9ments, {errors} erreurs.",
    missing_vars = "Variables manquantes dans les donn\u00e9es: {vars}",
    processing = "Traitement de {n} \u00e9l\u00e9ments avec {provider}...",
    config_title = "Configuration LLM actuelle:",
    get_key_urls = "Vous pouvez obtenir une cl\u00e9 sur:",
    failed_rows = "Lignes en erreur: {rows}",
    # API error messages
    api_error_title = "[ERREUR API {provider}]",
    api_error_401 = "Cl\u00e9 API invalide ou expir\u00e9e. V\u00e9rifiez votre cl\u00e9 avec LLM_Config(provider = '{provider}', api_key = '...').",
    api_error_403 = "Acc\u00e8s refus\u00e9. Votre cl\u00e9 API n'a pas les permissions n\u00e9cessaires.",
    api_error_404 = "Mod\u00e8le '{model}' non trouv\u00e9. V\u00e9rifiez le nom du mod\u00e8le.",
    api_error_429 = "Limite de requ\u00eates d\u00e9pass\u00e9e. Attendez ou r\u00e9duisez la fr\u00e9quence des appels.",
    api_error_500 = "Erreur serveur du provider. R\u00e9essayez plus tard.",
    api_error_502 = "Le provider est temporairement indisponible (Bad Gateway).",
    api_error_503 = "Le provider est en surcharge. R\u00e9essayez dans quelques minutes.",
    api_error_quota = "Quota d\u00e9pass\u00e9. V\u00e9rifiez votre solde/cr\u00e9dit sur le site du provider.",
    api_error_model = "Le mod\u00e8le '{model}' n'existe pas ou n'est pas accessible avec votre cl\u00e9.",
    api_error_context = "Le texte d\u00e9passe la limite de tokens du mod\u00e8le ({model}).",
    api_error_timeout = "Timeout: le serveur n'a pas r\u00e9pondu dans le d\u00e9lai ({timeout}s).",
    api_error_network = "Erreur r\u00e9seau: impossible de contacter {provider}.",
    api_error_unknown = "Erreur HTTP {status}: {message}",
    api_check_key = "V\u00e9rifiez votre cl\u00e9: Sys.getenv('{env_key}')",
    api_check_model = "Mod\u00e8les disponibles: {models}"
  ),
  en = list(
    no_api_key = "No API key found for {provider}.",
    ask_key = "Enter your API key (or press Enter to cancel): ",
    key_saved = "API key saved for this R session.",
    key_permanent = "To make it permanent, add to your .Renviron:",
    cancelled = "Operation cancelled by user.",
    rate_limit = "Rate limit reached. Waiting {delay} seconds...",
    retry = "Attempt {attempt}/{max} failed: {error}",
    success = "Processing complete: {n} items, {errors} errors.",
    missing_vars = "Missing variables in data: {vars}",
    processing = "Processing {n} items with {provider}...",
    config_title = "Current LLM configuration:",
    get_key_urls = "You can get a key at:",
    failed_rows = "Failed rows: {rows}",
    # API error messages
    api_error_title = "[API ERROR {provider}]",
    api_error_401 = "Invalid or expired API key. Check your key with LLM_Config(provider = '{provider}', api_key = '...').",
    api_error_403 = "Access denied. Your API key doesn't have the required permissions.",
    api_error_404 = "Model '{model}' not found. Check the model name.",
    api_error_429 = "Rate limit exceeded. Wait or reduce request frequency.",
    api_error_500 = "Provider server error. Try again later.",
    api_error_502 = "Provider temporarily unavailable (Bad Gateway).",
    api_error_503 = "Provider is overloaded. Try again in a few minutes.",
    api_error_quota = "Quota exceeded. Check your balance/credits on the provider's website.",
    api_error_model = "Model '{model}' doesn't exist or isn't accessible with your key.",
    api_error_context = "Text exceeds the model's token limit ({model}).",
    api_error_timeout = "Timeout: server didn't respond within {timeout}s.",
    api_error_network = "Network error: unable to reach {provider}.",
    api_error_unknown = "HTTP Error {status}: {message}",
    api_check_key = "Check your key: Sys.getenv('{env_key}')",
    api_check_model = "Available models: {models}"
  )
)

# -----------------------------------------------------------------------------
# Message helper (internal)
# -----------------------------------------------------------------------------
.msg <- function(key, ...) {
  lang <- getOption("mwiR.llm.lang", "fr")
  if (!lang %in% names(.llm_messages)) lang <- "fr"
  template <- .llm_messages[[lang]][[key]] %||% .llm_messages[["en"]][[key]] %||% key
  glue::glue(template, ..., .envir = parent.frame())
}

# -----------------------------------------------------------------------------
# Format API error with clear diagnostic (internal)
# -----------------------------------------------------------------------------
.format_api_error <- function(status_code, error_body, provider, model, config = NULL) {
  # Parse error body to extract message
  error_msg <- tryCatch({
    if (is.character(error_body)) {
      parsed <- jsonlite::fromJSON(error_body, simplifyVector = FALSE)
      # Different providers have different error structures
      parsed$error$message %||% parsed$error %||% parsed$message %||% error_body
    } else {
      as.character(error_body)
    }
  }, error = function(e) as.character(error_body))

  # Truncate long messages
  if (nchar(error_msg) > 200) error_msg <- paste0(substr(error_msg, 1, 200), "...")

  env_key <- .llm_providers[[provider]]$env_key %||% "API_KEY"

  # Build diagnostic message based on status code
  diagnostic <- switch(as.character(status_code),
    "401" = .msg("api_error_401", provider = provider),
    "403" = {
      # Check if it's a quota/billing issue (common in error messages)
      if (grepl("quota|billing|credit|insufficient", error_msg, ignore.case = TRUE)) {
        .msg("api_error_quota")
      } else {
        .msg("api_error_403")
      }
    },
    "404" = {
      if (grepl("model|not found", error_msg, ignore.case = TRUE)) {
        .msg("api_error_model", model = model)
      } else {
        .msg("api_error_404", model = model)
      }
    },
    "429" = .msg("api_error_429"),
    "500" = .msg("api_error_500"),
    "502" = .msg("api_error_502"),
    "503" = .msg("api_error_503"),
    .msg("api_error_unknown", status = status_code, message = error_msg)
  )

  # Detect specific error patterns in message
  if (grepl("invalid.*key|api.?key|authentication|unauthorized", error_msg, ignore.case = TRUE)) {
    diagnostic <- .msg("api_error_401", provider = provider)
  } else if (grepl("model.*not.*found|does not exist|invalid model", error_msg, ignore.case = TRUE)) {
    diagnostic <- .msg("api_error_model", model = model)
  } else if (grepl("context.*length|token.*limit|max.*tokens", error_msg, ignore.case = TRUE)) {
    diagnostic <- .msg("api_error_context", model = model)
  } else if (grepl("quota|billing|credit|insufficient.*funds", error_msg, ignore.case = TRUE)) {
    diagnostic <- .msg("api_error_quota")
  }

  # Provider-specific model suggestions
  model_suggestions <- switch(provider,
    openai = "gpt-4o, gpt-4o-mini, gpt-3.5-turbo",
    openrouter = "openai/gpt-4o, anthropic/claude-3.5-sonnet, google/gemini-pro",
    anthropic = "claude-sonnet-4-20250514, claude-3-5-sonnet-20241022, claude-3-haiku-20240307",
    ollama = "llama3, mistral, codellama",
    "check provider documentation"
  )

  # Build full error message
  lines <- c(
    .msg("api_error_title", provider = toupper(provider)),
    paste0("  Status: HTTP ", status_code),
    paste0("  Model: ", model),
    paste0("  Message: ", error_msg),
    "",
    paste0("  -> ", diagnostic)
  )

  # Add helpful hints based on error type
  if (status_code %in% c(401, 403)) {
    lines <- c(lines, paste0("  -> ", .msg("api_check_key", env_key = env_key)))
  }
  if (status_code == 404 || grepl("model", error_msg, ignore.case = TRUE)) {
    lines <- c(lines, paste0("  -> ", .msg("api_check_model", models = model_suggestions)))
  }

  paste(lines, collapse = "\n")
}

# -----------------------------------------------------------------------------
# Auto-detect available provider (internal)
# -----------------------------------------------------------------------------
.auto_detect_provider <- function() {
  available <- c()
  if (nzchar(Sys.getenv("OPENAI_API_KEY", ""))) available <- c(available, "openai")
  if (nzchar(Sys.getenv("OPENROUTER_API_KEY", ""))) available <- c(available, "openrouter")
  if (nzchar(Sys.getenv("ANTHROPIC_API_KEY", ""))) available <- c(available, "anthropic")

  # Check if Ollama is running locally
  ollama_running <- tryCatch({
    resp <- httr::GET("http://localhost:11434/api/tags", httr::timeout(2))
    httr::status_code(resp) == 200
  }, error = function(e) FALSE)
  if (ollama_running) available <- c(available, "ollama")

  if (length(available) == 0) return(NULL)
  if (length(available) == 1) return(available)

  # Return preferred provider from session or first available
  pref <- getOption("mwiR.llm.preferred_provider")
  if (!is.null(pref) && pref %in% available) return(pref)
  available[1]
}

# -----------------------------------------------------------------------------
# Interactive API key request (internal)
# -----------------------------------------------------------------------------
.ask_for_api_key <- function(provider) {
  if (!interactive()) {
    env_key <- .llm_providers[[provider]]$env_key
    stop(.msg("no_api_key", provider = provider), "\n",
         "Sys.setenv(", env_key, " = 'your-key')")
  }

  message("\n=== Configuration ", toupper(provider), " ===")
  message(.msg("no_api_key", provider = provider))
  message(.msg("get_key_urls"))
  message("  - OpenAI: https://platform.openai.com/api-keys")
  message("  - OpenRouter: https://openrouter.ai/keys")
  message("  - Anthropic: https://console.anthropic.com/")

  key <- readline(.msg("ask_key"))
  if (!nzchar(key)) stop(.msg("cancelled"))

  env_key <- .llm_providers[[provider]]$env_key
  do.call(Sys.setenv, setNames(list(key), env_key))
  message(.msg("key_saved"))
  message(.msg("key_permanent"))
  message("  ", env_key, "=", substr(key, 1, 10), "...")

  key
}

# -----------------------------------------------------------------------------
# Resolve API key (internal)
# -----------------------------------------------------------------------------
.resolve_api_key <- function(provider, api_key) {
  if (!is.null(api_key) && nzchar(api_key)) return(api_key)
  if (provider == "ollama") return("")

  env_key <- .llm_providers[[provider]]$env_key
  key <- Sys.getenv(env_key, unset = "")
  if (nzchar(key)) return(key)

  .ask_for_api_key(provider)
}

# -----------------------------------------------------------------------------
# Render glue template (internal)
# -----------------------------------------------------------------------------
.render_prompt <- function(template, data_row) {
  glue::glue_data(.x = as.list(data_row), template, .null = "NA", .envir = emptyenv())
}

# -----------------------------------------------------------------------------
# Prepare data for batch processing (internal)
# -----------------------------------------------------------------------------
.prepare_data <- function(data, prompt) {
  matches <- regmatches(prompt, gregexpr("\\{([^}]+)\\}", prompt))[[1]]
  vars <- gsub("[{}]", "", matches)

  if (is.vector(data) && !is.data.frame(data) && !is.list(data)) {
    data <- data.frame(value = data, stringsAsFactors = FALSE)
  }
  if (is.list(data) && !is.data.frame(data)) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }

  if (length(vars) > 0) {
    missing <- setdiff(vars, names(data))
    if (length(missing) > 0) {
      stop(.msg("missing_vars", vars = paste(missing, collapse = ", ")))
    }
  }
  data
}

# -----------------------------------------------------------------------------
# OpenAI API call (internal)
# -----------------------------------------------------------------------------
.call_openai <- function(prompt, sysprompt, config) {
  messages <- list(
    list(role = "system", content = sysprompt),
    list(role = "user", content = prompt)
  )

  payload <- list(
    model = config$model,
    messages = messages,
    temperature = config$temperature,
    max_tokens = config$max_tokens
  )

  headers <- httr::add_headers(
    "Authorization" = paste("Bearer", config$api_key),
    "Content-Type" = "application/json"
  )

  resp <- httr::POST(
    url = config$api_base,
    headers,
    body = jsonlite::toJSON(payload, auto_unbox = TRUE),
    encode = "raw",
    httr::timeout(config$timeout)
  )

  status <- httr::status_code(resp)
  body <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(
    jsonlite::fromJSON(body, simplifyVector = FALSE),
    error = function(e) list()
  )

  list(
    status_code = status,
    content = if (status == 200) parsed$choices[[1]]$message$content else NULL,
    error = if (status != 200) (parsed$error$message %||% body) else NULL,
    error_body = if (status != 200) body else NULL,
    tokens = parsed$usage$total_tokens %||% NA_integer_
  )
}

# -----------------------------------------------------------------------------
# OpenRouter API call (internal)
# -----------------------------------------------------------------------------
.call_openrouter <- function(prompt, sysprompt, config) {
  messages <- list(
    list(role = "system", content = sysprompt),
    list(role = "user", content = prompt)
  )

  payload <- list(
    model = config$model,
    messages = messages,
    temperature = config$temperature,
    max_tokens = config$max_tokens,
    stream = FALSE
  )

  referer <- getOption("mwiR.llm.referer", "https://github.com/MyWebIntelligence")
  title <- getOption("mwiR.llm.title", "mwiR")

  headers <- httr::add_headers(
    "Authorization" = paste("Bearer", config$api_key),
    "Content-Type" = "application/json",
    "HTTP-Referer" = referer,
    "X-Title" = title
  )

  if (!is.null(config$extra_headers)) {
    headers <- httr::add_headers(.headers = c(headers, config$extra_headers))
  }

  resp <- httr::POST(
    url = config$api_base,
    headers,
    body = jsonlite::toJSON(payload, auto_unbox = TRUE),
    encode = "raw",
    httr::timeout(config$timeout)
  )

  status <- httr::status_code(resp)
  body <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(
    jsonlite::fromJSON(body, simplifyVector = FALSE),
    error = function(e) list()
  )

  list(
    status_code = status,
    content = if (status == 200) parsed$choices[[1]]$message$content else NULL,
    error = if (status != 200) (parsed$error$message %||% body) else NULL,
    error_body = if (status != 200) body else NULL,
    tokens = parsed$usage$total_tokens %||% NA_integer_
  )
}

# -----------------------------------------------------------------------------
# Anthropic API call (internal)
# -----------------------------------------------------------------------------
.call_anthropic <- function(prompt, sysprompt, config) {
  payload <- list(
    model = config$model,
    max_tokens = config$max_tokens,
    system = sysprompt,
    messages = list(
      list(role = "user", content = prompt)
    )
  )

  headers <- httr::add_headers(
    "x-api-key" = config$api_key,
    "anthropic-version" = "2023-06-01",
    "Content-Type" = "application/json"
  )

  resp <- httr::POST(
    url = config$api_base,
    headers,
    body = jsonlite::toJSON(payload, auto_unbox = TRUE),
    encode = "raw",
    httr::timeout(config$timeout)
  )

  status <- httr::status_code(resp)
  body <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(
    jsonlite::fromJSON(body, simplifyVector = FALSE),
    error = function(e) list()
  )

  list(
    status_code = status,
    content = if (status == 200) parsed$content[[1]]$text else NULL,
    error = if (status != 200) (parsed$error$message %||% body) else NULL,
    error_body = if (status != 200) body else NULL,
    tokens = (parsed$usage$input_tokens %||% 0L) + (parsed$usage$output_tokens %||% 0L)
  )
}

# -----------------------------------------------------------------------------
# Ollama API call (internal)
# -----------------------------------------------------------------------------
.call_ollama <- function(prompt, sysprompt, config) {
  payload <- list(
    model = config$model,
    messages = list(
      list(role = "system", content = sysprompt),
      list(role = "user", content = prompt)
    ),
    stream = FALSE,
    options = list(
      temperature = config$temperature,
      num_predict = config$max_tokens
    )
  )

  resp <- httr::POST(
    url = config$api_base,
    httr::add_headers("Content-Type" = "application/json"),
    body = jsonlite::toJSON(payload, auto_unbox = TRUE),
    encode = "raw",
    httr::timeout(config$timeout)
  )

  status <- httr::status_code(resp)
  body <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(
    jsonlite::fromJSON(body, simplifyVector = FALSE),
    error = function(e) list()
  )

  list(
    status_code = status,
    content = if (status == 200) parsed$message$content else NULL,
    error = if (status != 200) (parsed$error %||% body) else NULL,
    error_body = if (status != 200) body else NULL,
    tokens = (parsed$prompt_eval_count %||% 0L) + (parsed$eval_count %||% 0L)
  )
}

# -----------------------------------------------------------------------------
# LLM dispatcher (internal)
# -----------------------------------------------------------------------------
.call_llm <- function(prompt, sysprompt, provider, config) {
  switch(provider,
    openai = .call_openai(prompt, sysprompt, config),
    openrouter = .call_openrouter(prompt, sysprompt, config),
    anthropic = .call_anthropic(prompt, sysprompt, config),
    ollama = .call_ollama(prompt, sysprompt, config),
    stop("Unknown provider: ", provider)
  )
}

# -----------------------------------------------------------------------------
# Call with retry logic (internal)
# -----------------------------------------------------------------------------
.call_with_retry <- function(prompt, sysprompt, provider, config) {
  delay <- config$retry_delay
  last_error <- NULL
  last_error_body <- NULL
  last_status <- NA
  verbose <- getOption("mwiR.llm.verbose", TRUE)

  for (attempt in seq_len(config$max_retries)) {
    result <- tryCatch({
      .call_llm(prompt, sysprompt, provider, config)
    }, error = function(e) {
      # Network/timeout errors
      err_msg <- conditionMessage(e)
      if (grepl("timeout|timed out", err_msg, ignore.case = TRUE)) {
        err_msg <- .msg("api_error_timeout", timeout = config$timeout)
      } else if (grepl("resolve|connection|network", err_msg, ignore.case = TRUE)) {
        err_msg <- .msg("api_error_network", provider = provider)
      }
      list(status_code = NA, content = NULL, error = err_msg, error_body = NULL, tokens = NA)
    })

    if (!is.na(result$status_code) && result$status_code == 200 && !is.null(result$content)) {
      return(list(
        value = result$content,
        status = "ok",
        attempts = attempt,
        tokens = result$tokens,
        error_message = NA_character_
      ))
    }

    last_error <- result$error %||% "Unknown error"
    last_error_body <- result$error_body
    last_status <- result$status_code

    # On first error, show detailed diagnostic (only once)
    if (verbose && attempt == 1 && !is.na(last_status)) {
      formatted_error <- .format_api_error(last_status, last_error_body %||% last_error,
                                           provider, config$model, config)
      message("\n", formatted_error, "\n")
    }

    if (verbose && attempt < config$max_retries) {
      message(.msg("retry", attempt = attempt, max = config$max_retries, error = substr(last_error, 1, 50)))
    }

    # Don't retry on fatal errors (invalid key, model not found)
    if (!is.na(last_status) && last_status %in% c(401, 403, 404)) {
      break
    }

    if (!is.na(last_status) && last_status == 429) {
      delay <- min(delay * config$backoff_multiplier, 60)
      if (verbose) message(.msg("rate_limit", delay = round(delay, 1)))
    }

    if (attempt < config$max_retries) {
      Sys.sleep(delay)
      delay <- delay * config$backoff_multiplier
    }
  }

  list(
    value = NA_character_,
    status = "failed",
    attempts = config$max_retries,
    tokens = NA_integer_,
    error_message = last_error
  )
}

# -----------------------------------------------------------------------------
# Process single row (internal)
# -----------------------------------------------------------------------------
.process_single_row <- function(data_row, prompt_template, sysprompt, provider, config, on_error) {
  tryCatch({
    rendered_prompt <- .render_prompt(prompt_template, data_row)
    result <- .call_with_retry(rendered_prompt, sysprompt, provider, config)

    if (config$validate && result$status == "ok") {
      content <- result$value
      refusal_pattern <- "sorry|error|cannot|not able|do not understand|unable|je ne peux pas"
      if (!nzchar(content) || grepl(refusal_pattern, content, ignore.case = TRUE)) {
        result$status <- "invalid"
        result$value <- NA_character_
      }
    }
    result
  }, error = function(e) {
    if (on_error == "stop") stop(e)
    list(
      value = NA_character_,
      status = "error",
      attempts = 0L,
      tokens = NA_integer_,
      error_message = conditionMessage(e)
    )
  })
}

# -----------------------------------------------------------------------------
# Batch processing (internal)
# -----------------------------------------------------------------------------
.process_batch <- function(data, prompt_template, sysprompt, provider, config,
                           parallel, workers, progress, on_error) {
  n <- nrow(data)
  verbose <- getOption("mwiR.llm.verbose", TRUE)

  if (verbose) message(.msg("processing", n = n, provider = provider))

  if (parallel && n > 1) {
    if (is.null(workers)) workers <- max(1, future::availableCores() - 1)
    oplan <- future::plan(future::multisession, workers = workers)
    on.exit(future::plan(oplan), add = TRUE)

    if (progress) {
      progressr::handlers(global = TRUE)
      results <- progressr::with_progress({
        p <- progressr::progressor(steps = n)
        furrr::future_map(seq_len(n), function(i) {
          p()
          .process_single_row(data[i, , drop = FALSE], prompt_template, sysprompt, provider, config, on_error)
        }, .options = furrr::furrr_options(seed = TRUE))
      })
    } else {
      results <- furrr::future_map(seq_len(n), function(i) {
        .process_single_row(data[i, , drop = FALSE], prompt_template, sysprompt, provider, config, on_error)
      }, .options = furrr::furrr_options(seed = TRUE))
    }
  } else {
    results <- vector("list", n)
    pb <- NULL

    if (progress && interactive()) {
      pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
      on.exit(if (!is.null(pb)) close(pb), add = TRUE)
    }

    for (i in seq_len(n)) {
      if (config$rate_limit_delay > 0 && i > 1) {
        Sys.sleep(config$rate_limit_delay)
      }

      results[[i]] <- .process_single_row(
        data[i, , drop = FALSE], prompt_template, sysprompt, provider, config, on_error
      )

      if (!is.null(pb)) utils::setTxtProgressBar(pb, i)
    }
  }

  results
}

# -----------------------------------------------------------------------------
# Format results (internal)
# -----------------------------------------------------------------------------
.format_results <- function(results, return_metadata) {
  if (!return_metadata) {
    vapply(results, function(r) r$value %||% NA_character_, character(1))
  } else {
    data.frame(
      row_index = seq_along(results),
      value = vapply(results, function(r) r$value %||% NA_character_, character(1)),
      status = vapply(results, function(r) r$status %||% "unknown", character(1)),
      attempts = vapply(results, function(r) as.integer(r$attempts %||% NA_integer_), integer(1)),
      tokens_used = vapply(results, function(r) as.integer(r$tokens %||% NA_integer_), integer(1)),
      error_message = vapply(results, function(r) r$error_message %||% NA_character_, character(1)),
      stringsAsFactors = FALSE
    )
  }
}

# -----------------------------------------------------------------------------
# Print summary (internal)
# -----------------------------------------------------------------------------
.print_summary <- function(results, verbose) {
  if (!verbose) return(invisible(NULL))

  n_total <- length(results)
  n_success <- sum(vapply(results, function(r) r$status == "ok", logical(1)))
  n_failed <- n_total - n_success

  message("\n", .msg("success", n = n_total, errors = n_failed))

  if (n_failed > 0) {
    failed_idx <- which(vapply(results, function(r) r$status != "ok", logical(1)))
    if (length(failed_idx) <= 10) {
      message(.msg("failed_rows", rows = paste(failed_idx, collapse = ", ")))
    } else {
      message(.msg("failed_rows", rows = paste(c(head(failed_idx, 10), "..."), collapse = ", ")))
    }
  }

  invisible(NULL)
}

#' Configure LLM Settings for Session
#'
#' @description
#' Configure default settings for LLM_Recode that persist for the R session.
#' This function is designed for social science researchers who want to set up
#' their configuration once and reuse it across multiple analyses.
#'
#' If a provider is specified and no API key is found, the function will
#' interactively prompt for the key (in interactive sessions) and store it
#' for the duration of the R session.
#'
#' @param provider Default LLM provider: "openai", "openrouter", "anthropic", or "ollama".
#'   When specified, checks if the corresponding API key is set. If missing and
#'   in an interactive session, prompts user to enter it.
#' @param model Default model to use (e.g., "gpt-4o", "claude-sonnet-4-20250514").
#'   NULL uses the provider's default model.
#' @param api_key API key to store for the session. If NULL and provider is
#'   specified, uses existing environment variable or prompts interactively.
#' @param verbose Show progress messages (TRUE/FALSE).
#' @param lang Language for messages: "fr" (French) or "en" (English).
#'
#' @return Invisibly returns NULL. Prints current configuration.
#'
#' @examples
#' \dontrun{
#' # Configure for French users with OpenAI (will prompt for API key if missing)
#' LLM_Config(provider = "openai", lang = "fr")
#'
#' # Configure with specific model
#' LLM_Config(provider = "openai", model = "gpt-4o-mini", lang = "fr")
#'
#' # Configure with API key directly
#' LLM_Config(provider = "openai", api_key = "sk-...")
#'
#' # View current configuration
#' LLM_Config()
#' }
#'
#' @export
LLM_Config <- function(provider = NULL, model = NULL, api_key = NULL,
                       verbose = NULL, lang = NULL) {

  # Set lang first so messages are in the right language
  if (!is.null(lang)) {
    if (!lang %in% c("fr", "en")) stop("lang must be 'fr' or 'en'")
    options(mwiR.llm.lang = lang)
  }

  if (!is.null(provider)) {
    if (!provider %in% names(.llm_providers)) {
      stop("Provider must be one of: ", paste(names(.llm_providers), collapse = ", "))
    }
    options(mwiR.llm.preferred_provider = provider)

    # Check if API key exists for selected provider, ask if missing
    env_key <- .llm_providers[[provider]]$env_key
    if (!is.null(env_key)) {
      current_key <- Sys.getenv(env_key, "")

      if (!is.null(api_key)) {
        # User provided api_key argument - use it
        do.call(Sys.setenv, setNames(list(api_key), env_key))
        message(.msg("key_saved"))
      } else if (!nzchar(current_key)) {
        # No API key set - ask interactively if possible
        if (interactive()) {
          message(.msg("no_api_key", provider = provider))
          user_key <- readline(prompt = .msg("ask_key"))
          if (nzchar(trimws(user_key))) {
            do.call(Sys.setenv, setNames(list(trimws(user_key)), env_key))
            message(.msg("key_saved"))
          }
        } else {
          warning(.msg("no_api_key", provider = provider), call. = FALSE)
        }
      }
    }
  } else if (!is.null(api_key)) {
    # api_key provided without provider - warn user
    warning("api_key provided but no provider specified. Use: LLM_Config(provider = 'openai', api_key = '...')",
            call. = FALSE)
  }

  if (!is.null(model)) options(mwiR.llm.default_model = model)
  if (!is.null(verbose)) options(mwiR.llm.verbose = verbose)

  message(.msg("config_title"))
  message("  Provider: ", getOption("mwiR.llm.preferred_provider", "auto-detect"))
  message("  Model: ", getOption("mwiR.llm.default_model", "provider default"))
  message("  Verbose: ", getOption("mwiR.llm.verbose", TRUE))
  message("  Lang: ", getOption("mwiR.llm.lang", "fr"))

  available <- c()
  if (nzchar(Sys.getenv("OPENAI_API_KEY", ""))) available <- c(available, "openai")
  if (nzchar(Sys.getenv("OPENROUTER_API_KEY", ""))) available <- c(available, "openrouter")
  if (nzchar(Sys.getenv("ANTHROPIC_API_KEY", ""))) available <- c(available, "anthropic")
  message("  Available: ", if (length(available) > 0) paste(available, collapse = ", ") else "none (configure API keys)")

  invisible(NULL)
}

#' Recode Data Using Large Language Models
#'
#' @description
#' Transform data values using LLMs (OpenAI, OpenRouter, Anthropic, Ollama).
#' Designed for social science researchers: simple to use, interactive configuration,
#' resilient error handling, and French/English messages.
#'
#' Uses glue-style templates for prompts: `"Translate {value} to French"`.
#' When passing a simple vector, use `{value}` as the placeholder.
#' When passing a data frame, use column names as placeholders.
#'
#' @param data A character vector or data frame to process. For vectors, use
#'   `{value}` in your prompt. For data frames, use column names.
#' @param prompt A glue-style template string. Example: `"Translate to French: {value}"`.
#' @param provider LLM provider: "openai", "openrouter", "anthropic", or "ollama".
#'   If NULL, auto-detects based on available API keys.
#' @param model Model identifier. NULL uses provider default (e.g., "gpt-4o" for OpenAI).
#' @param temperature Sampling temperature (0-2). Lower = more deterministic. Default: 0.7.
#' @param max_tokens Maximum tokens per response. Default: 1000.
#' @param sysprompt System prompt guiding model behavior.
#' @param api_key API key. If NULL, reads from environment or asks interactively.
#' @param api_base Custom API endpoint URL (for local models or proxies).
#' @param timeout Request timeout in seconds. Default: 60.
#' @param max_retries Maximum retry attempts per request. Default: 3.
#' @param retry_delay Initial delay between retries (seconds). Default: 1.
#' @param backoff_multiplier Multiplier for exponential backoff. Default: 2.
#' @param rate_limit_delay Delay between requests (seconds). Default: 0.
#' @param validate Apply heuristics to detect invalid responses. Default: TRUE.
#' @param return_metadata If TRUE, returns data frame with metadata. Default: FALSE.
#' @param on_error "continue" (default) returns NA and proceeds, "stop" halts on error.
#' @param parallel Enable parallel processing. Default: FALSE.
#' @param workers Number of parallel workers. NULL = auto.
#' @param progress Show progress bar. Default: TRUE.
#' @param extra_headers Named character vector of additional HTTP headers.
#'
#' @return By default, a character vector of same length as input.
#'   If `return_metadata = TRUE`, a data frame with columns:
#'   row_index, value, status, attempts, tokens_used, error_message.
#'
#' @examples
#' \dontrun{
#' # Simple translation (auto-detects provider from API keys)
#' result <- LLM_Recode(
#'   data = c("Hello world", "Good morning"),
#'   prompt = "Translate to French: {value}"
#' )
#'
#' # With data frame
#' df <- data.frame(
#'   title = c("Article 1", "Article 2"),
#'   abstract = c("This paper explores...", "We analyze...")
#' )
#' summaries <- LLM_Recode(
#'   data = df,
#'   prompt = "Summarize in 10 words: {title} - {abstract}",
#'   provider = "openai"
#' )
#'
#' # Get detailed metadata
#' results <- LLM_Recode(
#'   data = my_texts,
#'   prompt = "Classify sentiment (positive/negative/neutral): {value}",
#'   return_metadata = TRUE
#' )
#' # Check failures
#' failed <- results[results$status != "ok", ]
#'
#' # Configure once for the session
#' LLM_Config(provider = "openai", lang = "fr")
#' # Then use simply
#' LLM_Recode(my_data, "Traduire: {value}")
#' }
#'
#' @seealso \code{\link{LLM_Config}} for session configuration.
#'
#' @importFrom httr POST GET add_headers timeout status_code content headers
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom glue glue glue_data
#' @importFrom future plan multisession sequential availableCores
#' @importFrom furrr future_map furrr_options
#' @importFrom progressr handlers with_progress progressor
#' @export
LLM_Recode <- function(data,
                       prompt,
                       provider = NULL,
                       model = NULL,
                       temperature = 0.7,
                       max_tokens = 1000,
                       sysprompt = "You are a helpful assistant that recodes dataframe values. Return only the transformed value.",
                       api_key = NULL,
                       api_base = NULL,
                       timeout = 60,
                       max_retries = 3,
                       retry_delay = 1,
                       backoff_multiplier = 2,
                       rate_limit_delay = 0,
                       validate = TRUE,
                       return_metadata = FALSE,
                       on_error = c("continue", "stop"),
                       parallel = FALSE,
                       workers = NULL,
                       progress = TRUE,
                       extra_headers = NULL) {

  on_error <- match.arg(on_error)

  if (missing(data) || is.null(data)) {
    stop("'data' is required")
  }
  if ((is.vector(data) && !is.data.frame(data) && length(data) == 0) ||
      (is.data.frame(data) && nrow(data) == 0)) {
    stop("'data' cannot be empty")
  }
  if (missing(prompt) || !is.character(prompt) || length(prompt) != 1 || !nzchar(prompt)) {
    stop("'prompt' must be a non-empty character string with {variable} placeholders")
  }
  if (!is.numeric(temperature) || temperature < 0 || temperature > 2) {
    stop("'temperature' must be between 0 and 2")
  }
  if (!is.numeric(max_tokens) || max_tokens <= 0) {
    stop("'max_tokens' must be a positive number")
  }
  if (!is.numeric(timeout) || timeout <= 0) {
    stop("'timeout' must be positive")
  }

  if (is.null(provider)) {
    provider <- .auto_detect_provider()
    if (is.null(provider)) {
      provider <- getOption("mwiR.llm.preferred_provider", "openai")
    }
  } else {
    provider <- match.arg(provider, names(.llm_providers))
  }

  prov_config <- .llm_providers[[provider]]

  if (is.null(model)) {
    model <- getOption("mwiR.llm.default_model") %||% prov_config$default_model
  }

  resolved_key <- .resolve_api_key(provider, api_key)

  if (is.null(api_base)) {
    api_base <- prov_config$default_base
  }

  prepared_data <- .prepare_data(data, prompt)

  config <- list(
    model = model,
    api_key = resolved_key,
    api_base = api_base,
    temperature = temperature,
    max_tokens = max_tokens,
    timeout = timeout,
    max_retries = max_retries,
    retry_delay = retry_delay,
    backoff_multiplier = backoff_multiplier,
    rate_limit_delay = rate_limit_delay,
    validate = validate,
    extra_headers = extra_headers
  )

  results <- .process_batch(
    data = prepared_data,
    prompt_template = prompt,
    sysprompt = sysprompt,
    provider = provider,
    config = config,
    parallel = parallel,
    workers = workers,
    progress = progress,
    on_error = on_error
  )

  .print_summary(results, getOption("mwiR.llm.verbose", TRUE))

  .format_results(results, return_metadata)
}

# -----------------------------------------------------------------------------
# UTILITY: SYSTEM INFO FOR PARALLELIZATION
# -----------------------------------------------------------------------------

#' Display system information for parallelization
#'
#' Shows system capabilities for parallel processing, including CPU core count,
#' Apple Silicon detection, and recommended worker count.
#'
#' @return Invisibly returns a list with system configuration details
#' @export
#' @examples
#' \dontrun{
#' mwir_system_info()
#' }
mwir_system_info <- function() {

  config <- list(
    os = as.character(Sys.info()["sysname"]),
    total_cores = parallel::detectCores(),
    recommended_workers = .detect_optimal_threads(),
    is_apple_silicon = FALSE,
    performance_cores = NA_integer_,
    efficiency_cores = NA_integer_
  )

  if (config$os == "Darwin") {
    # Check for Apple Silicon
    arch <- tryCatch(
      system("uname -m", intern = TRUE),
      error = function(e) "unknown"
    )
    config$is_apple_silicon <- arch %in% c("arm64", "arm64e")

    if (config$is_apple_silicon) {
      # Get performance core count
      config$performance_cores <- tryCatch({
        as.integer(system("sysctl -n hw.perflevel0.physicalcpu", intern = TRUE))
      }, error = function(e) NA_integer_)

      # Get efficiency core count
      config$efficiency_cores <- tryCatch({
        as.integer(system("sysctl -n hw.perflevel1.physicalcpu", intern = TRUE))
      }, error = function(e) NA_integer_)
    }
  }

  cat("=== mwiR System Information ===\n")
  cat("OS:", config$os, "\n")
  cat("Total cores:", config$total_cores, "\n")

  if (config$is_apple_silicon) {
    cat("Apple Silicon: Yes\n")
    if (!is.na(config$performance_cores)) {
      cat("Performance cores (P):", config$performance_cores, "\n")
    }
    if (!is.na(config$efficiency_cores)) {
      cat("Efficiency cores (E):", config$efficiency_cores, "\n")
    }
  } else if (config$os == "Darwin") {
    cat("Apple Silicon: No (Intel Mac)\n")
  }

  cat("Recommended workers:", config$recommended_workers, "\n")
  cat("================================\n")

  invisible(config)
}
