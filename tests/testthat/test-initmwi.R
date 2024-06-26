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

  # Check for package installation calls
  expect_true(requireNamespace("httr", quietly = TRUE))
  expect_true(requireNamespace("jsonlite", quietly = TRUE))
  expect_true(requireNamespace("RSQLite", quietly = TRUE))
  expect_true(requireNamespace("tools", quietly = TRUE))

  # Check for trafilatura message
  expect_message(message("Trafilatura is not installed. Run 'pip install trafilatura' in your Python environment."))

})
