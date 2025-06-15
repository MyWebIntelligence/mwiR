test_that("mwiR_detectLang detects languages correctly", {
  df <- data.frame(
    title = c("Hello world", "Bonjour le monde", "Hola mundo"),
    description = c("This is a test", "Ceci est un test", "Esto es una prueba")
  )
  languages <- mwiR_detectLang(df, c("title", "description"))
  expect_equal(languages, c("en", "fr", "es"))
  languages_title <- mwiR_detectLang(df, "title")
  expect_equal(languages_title, c("en", "fr", "es"))
  languages_description <- mwiR_detectLang(df, "description")
  expect_equal(languages_description, c("en", "fr", "es"))
  expect_error(mwiR_detectLang(df, c("title", "non_existent_column")),
               "Some specified variables do not exist in the data frame.")
})

test_that("mwiR_detectLang handles non-installed cld3 package", {
  unloadNamespace("cld3")
  expect_error(mwiR_detectLang(data.frame(title = "test"), "title"),
               "Package 'cld3' is required but not installed.")
  library(cld3)
})

test_that("mwiR_detectLang works with empty data frame", {
  df_empty <- data.frame(title = character(0), description = character(0))
  languages_empty <- mwiR_detectLang(df_empty, c("title", "description"))
  expect_equal(length(languages_empty), 0)
})

test_that("plotlog functions correctly", {
  df_test <- data.frame(
    numeric1 = c(1, 2, 3, 4, 5),
    numeric2 = c(10, 100, 1000, 10000, 100000),
    character = c("a", "b", "c", "d", "e")
  )
  expect_error(plotlog(df_test), NA)
  expect_error(plotlog(df_test, c("numeric1", "numeric2")), NA)
  expect_error(plotlog(df_test, c("numeric1", "character")))
  expect_error(plotlog(df_test, c("numeric1", "non_existent")))
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
