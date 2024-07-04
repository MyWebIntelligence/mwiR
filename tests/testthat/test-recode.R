test_that("mwiR_detectLang detects languages correctly", {
  # Create a sample data frame
  df <- data.frame(
    title = c("Hello world", "Bonjour le monde", "Hola mundo"),
    description = c("This is a test", "Ceci est un test", "Esto es una prueba")
  )

  # Test language detection using both title and description columns
  languages <- mwiR_detectLang(df, c("title", "description"))
  expect_equal(languages, c("en", "fr", "es"))

  # Test language detection using only the title column
  languages_title <- mwiR_detectLang(df, "title")
  expect_equal(languages_title, c("en", "fr", "es"))

  # Test language detection using only the description column
  languages_description <- mwiR_detectLang(df, "description")
  expect_equal(languages_description, c("en", "fr", "es"))

  # Test with missing columns
  expect_error(mwiR_detectLang(df, c("title", "non_existent_column")),
               "Some specified variables do not exist in the data frame.")
})

test_that("mwiR_detectLang handles non-installed cld3 package", {
  # Temporarily unload cld3 package
  unloadNamespace("cld3")

  # Test for the error when cld3 is not installed
  expect_error(mwiR_detectLang(data.frame(title = "test"), "title"),
               "Package 'cld3' is required but not installed.")

  # Reload cld3 package for further tests
  library(cld3)
})

test_that("mwiR_detectLang works with empty data frame", {
  # Create an empty data frame
  df_empty <- data.frame(title = character(0), description = character(0))

  # Test with empty data frame
  languages_empty <- mwiR_detectLang(df_empty, c("title", "description"))
  expect_equal(length(languages_empty), 0)
})

test_that("plotlog functions correctly", {
  # Create test data
  df_test <- data.frame(
    numeric1 = c(1, 2, 3, 4, 5),
    numeric2 = c(10, 100, 1000, 10000, 100000),
    character = c("a", "b", "c", "d", "e")
  )

  # Test the function with all numeric variables
  expect_error(plotlog(df_test), NA)

  # Test the function with specific variables
  expect_error(plotlog(df_test, c("numeric1", "numeric2")), NA)

  # Test that the function generates an error with non-numeric variables
  expect_error(plotlog(df_test, c("numeric1", "character")))

  # Test that the function generates an error with non-existent variables
  expect_error(plotlog(df_test, c("numeric1", "non_existent")))

  # You can add other tests here...
})
