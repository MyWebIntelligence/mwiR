# Test for initmwi function
test_that("initmwi function works correctly", {
  # Mocking system calls for npm and reticulate
  mockery::stub(initmwi, "system", "empty")
  mockery::stub(initmwi, "reticulate::py_module_available", TRUE)
  mockery::stub(initmwi, "reticulate::import", NULL)

  # Mocking readline function to simulate user input
  mockery::stub(initmwi, "readline", "mock_serp_key")

  # Mocking install.packages to avoid actual package installation
  mock_install.packages <- function(pkgs, repos) TRUE
  mockery::stub(initmwi, "install.packages", mock_install.packages)

  # Mocking library to avoid loading actual packages
  mock_library <- function(pkg, character.only) TRUE
  mockery::stub(initmwi, "library", mock_library)

  # Run the function
  initmwi()

  # Check if the SERP API key is correctly stored in the global environment
  expect_equal(Sys.getenv("serp_key"), "mock_serp_key")

  # Check for package installation calls
  expect_true(requireNamespace("httr", quietly = TRUE))
  expect_true(requireNamespace("jsonlite", quietly = TRUE))
  expect_true(requireNamespace("RSQLite", quietly = TRUE))
  expect_true(requireNamespace("tools", quietly = TRUE))

  # Check for npm package warning message
  expect_warning(system("npm list -g @postlight/parser", intern = TRUE), "Le parser npm @postlight/parser n'est pas installé.")

  # Check for trafilatura message
  expect_message(message("Trafilatura est installé et disponible."))

  # Reset the global environment
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
})
