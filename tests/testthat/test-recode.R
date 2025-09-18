test_that("mwiR_detectLang returns languages and probability attribute", {
  skip_if_not_installed("cld3")
  skip_if_not_installed("stringi")

  df <- data.frame(
    title = c(
      "Hello world this sentence is deliberately long enough for detection.",
      "Bonjour le monde ceci est une phrase suffisamment longue pour la détection.",
      "Hola mundo esta oración es lo bastante larga para la detección."
    ),
    description = c(
      "This is an additional English description.",
      "Ceci est une description française supplémentaire.",
      "Esta es una descripción adicional en español."
    ),
    stringsAsFactors = FALSE
  )

  res <- mwiR_detectLang(df, c("title", "description"), min_chars = 10L, chunk_size = 2L)
  expect_equal(res, c("en", "fr", "es"))

  probs <- attr(res, "probability")
  expect_type(probs, "double")
  expect_equal(length(probs), nrow(df))
  expect_true(all(probs >= 0 & probs <= 1, na.rm = TRUE))

  # Single column path
  res_title <- mwiR_detectLang(df, "title", min_chars = 10L, chunk_size = 1L)
  expect_equal(res_title, c("en", "fr", "es"))

  # Variable existence error
  expect_error(
    mwiR_detectLang(df, c("title", "missing_col")),
    "The following variables do not exist in 'df': missing_col"
  )
})

test_that("mwiR_detectLang enforces min_chars and preserves long-form detection", {
  skip_if_not_installed("cld3")
  skip_if_not_installed("stringi")

  df <- data.frame(
    text = c(
      "Hi",  # too short for default threshold
      paste(rep("Bonjour le monde", 6), collapse = " ")
    ),
    stringsAsFactors = FALSE
  )

  res <- mwiR_detectLang(df, "text")
  expect_true(is.na(res[1]))
  expect_equal(res[2], "fr")
})

test_that("mwiR_detectLang drops low-confidence predictions when conservative", {
  skip_if_not_installed("mockery")
  skip_if_not_installed("stringi")

  fake_detect <- function(texts) {
    replicate(
      length(texts),
      data.frame(language = "en", probability = 0.25, stringsAsFactors = FALSE),
      simplify = FALSE
    )
  }

  mockery::stub(mwiR_detectLang, "cld3::detect_language_probabilities", fake_detect)
  mockery::stub(mwiR_detectLang, "requireNamespace", function(pkg, quietly) TRUE)

  df <- data.frame(
    text = "This is an English sentence that is long enough for detection.",
    stringsAsFactors = FALSE
  )

  res <- mwiR_detectLang(df, "text", min_chars = 10L, min_prob = 0.8, chunk_size = 1L)
  expect_true(all(is.na(res)))
})

test_that("mwiR_detectLang can return probability data frames", {
  skip_if_not_installed("cld3")
  skip_if_not_installed("stringi")

  df <- data.frame(
    text = c(
      "This is a long piece of English text intended for language detection.",
      "Ceci est un long texte français pour la détection de langue."
    ),
    stringsAsFactors = FALSE
  )

  scored <- mwiR_detectLang(df, "text", return_scores = TRUE, min_chars = 10L)
  expect_s3_class(scored, "data.frame")
  expect_named(scored, c("language", "probability"))
  expect_equal(nrow(scored), nrow(df))
  expect_true(all(scored$probability >= 0 & scored$probability <= 1, na.rm = TRUE))
})

test_that("mwiR_detectLang reports missing cld3 dependency", {
  skip_if_not_installed("mockery")

  mockery::stub(mwiR_detectLang, "requireNamespace", function(pkg, quietly) pkg != "cld3")

  df <- data.frame(text = "Fallback sentence.", stringsAsFactors = FALSE)
  expect_error(
    mwiR_detectLang(df, "text"),
    "Package 'cld3' is required but not installed."
  )
})

test_that("mwiR_detectLang requires a fastText model when requested", {
  skip_if_not_installed("fastrtext")
  skip_if_not_installed("stringi")

  df <- data.frame(
    text = "Yet another sufficiently long English sentence for testing.",
    stringsAsFactors = FALSE
  )

  expect_error(
    mwiR_detectLang(df, "text", engine = "fasttext"),
    "Provide 'fasttext_model' \\(path or loaded model\\) when engine = 'fasttext'."
  )
})

# --- Additional tests for recode.R major functions ---

test_that("diagnose_variable returns correct structure and handles small samples", {
  x <- c(1, 2, 3, 4, 5)
  res <- diagnose_variable(x)
  expect_true(all(c("n_valid", "skewness", "kurtosis", "shapiro_wilk_p") %in% names(res)))
  expect_equal(res$n_valid, 5)
  expect_true(is.numeric(res$skewness))
  expect_true(is.numeric(res$kurtosis))
  expect_true(is.numeric(res$shapiro_wilk_p))
  # Small sample
  res2 <- diagnose_variable(c(1, NA, NA))
  expect_equal(res2$n_valid, 1)
  expect_true(is.na(res2$skewness))
})

test_that("transform_variable applies transformations and handles errors", {
  x <- c(1, 2, 3, 4, 5)
  res <- transform_variable(x, method = "log1p")
  expect_true(is.list(res))
  expect_true("val" %in% names(res))
  expect_equal(length(res$val), length(x))
  # log with negative values
  expect_warning(transform_variable(c(-1, 0, 1), method = "log"))
  # method none
  res2 <- transform_variable(x, method = "none")
  expect_equal(res2$val, x)
})

test_that("discretize_variable works for equal_freq and manual", {
  x <- 1:10
  f1 <- discretize_variable(x, method = "equal_freq", bins = 2)
  expect_true(is.factor(f1))
  expect_equal(length(f1), length(x))
  # manual breaks
  f2 <- discretize_variable(x, method = "manual", breaks = c(0, 5, 10))
  expect_true(is.factor(f2))
  # not enough unique breaks
  expect_warning(discretize_variable(rep(1, 10), method = "equal_freq", bins = 2))
})

test_that("find_clusters identifies clusters or errors on constant input", {
  x <- c(1, 2, 2, 3, 3, 3, 4, 4, 5)
  res <- find_clusters(x, max_G = 3)
  expect_true(is.list(res))
  expect_true("n_clusters" %in% names(res))
  expect_true("classification" %in% names(res))
  # constant input
  expect_error(find_clusters(rep(1, 10)))
})

test_that("analyse_powerlaw works for valid input and errors for small samples", {
  set.seed(123)
  x <- rpois(100, lambda = 10) + 1
  res <- analyse_powerlaw(x, type = "discrete")
  expect_true(is.list(res))
  expect_true("xmin" %in% names(res))
  expect_true("alpha" %in% names(res))
  expect_true("gof_p_value" %in% names(res))
  # too small sample
  expect_error(analyse_powerlaw(1:10, type = "discrete"))
})

test_that("mwir_seorank errors on missing arguments", {
  expect_error(mwir_seorank(NULL, "example.com", "key"))
  expect_error(mwir_seorank("file", NULL, "key"))
  expect_error(mwir_seorank("file", "example.com", NULL))
})

test_that("annotatedData errors on missing table or fields", {
  skip_on_cran()
  # Create a temporary SQLite DB
  db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db)
  DBI::dbExecute(con, "CREATE TABLE test (id INTEGER, value TEXT)")
  DBI::dbDisconnect(con)
  # Wrong table
  expect_error(annotatedData(data.frame(id = 1, value = "a"), "notable", "value", "id", labase = db))
  # Wrong field
  expect_error(annotatedData(data.frame(id = 1, value = "a"), "test", "notfield", "id", labase = db))
  # Wrong key
  expect_error(annotatedData(data.frame(id = 1, value = "a"), "test", "value", "notkey", labase = db))
  unlink(db)
})

test_that("GPT_Recode errors on missing API key or invalid input", {
  expect_error(GPT_Recode("Translate to French", "Hello world", model = "gpt-4o", temperature = 0.8, max_tokens = 1000, max_retries = 1, retry_delay = 0.1, validate = FALSE))
  expect_error(GPT_Recode("", "Hello world"))
  expect_error(GPT_Recode("Translate", 123))
  expect_error(GPT_Recode("Translate", "Hello", temperature = -1))
})
