# Tests for python_setup.R

test_that("mwi_python_env returns valid path", {
  path <- mwi_python_env()
  expect_type(path, "character")
  expect_true(nchar(path) > 0)
  expect_true(grepl("mwiR", path))
  expect_true(grepl("python_venv", path))
})

test_that("check_python_status returns proper structure", {
  # Mock reticulate functions to avoid actual Python dependency in tests
  mockery::stub(check_python_status, "reticulate::py_available", TRUE)
  mockery::stub(check_python_status, "reticulate::use_virtualenv", NULL)
  mockery::stub(check_python_status, "reticulate::py_config", list(version = "3.10.0"))
  mockery::stub(check_python_status, "reticulate::py_module_available", FALSE)
  mockery::stub(check_python_status, "dir.exists", FALSE)

  status <- check_python_status()

  expect_s3_class(status, "mwi_python_status")
  expect_true("python_available" %in% names(status))
  expect_true("virtualenv_exists" %in% names(status))
  expect_true("trafilatura_installed" %in% names(status))
  expect_true("python_version" %in% names(status))
  expect_true("trafilatura_version" %in% names(status))
  expect_true("virtualenv_path" %in% names(status))
})

test_that("check_python_status print method works", {
  status <- list(
    python_available = TRUE,
    virtualenv_exists = TRUE,
    trafilatura_installed = TRUE,
    python_version = "3.10.0",
    trafilatura_version = "1.6.0",
    virtualenv_path = "/test/path"
  )
  class(status) <- c("mwi_python_status", "list")

  expect_output(print(status), "mwiR Python Environment Status")
  expect_output(print(status), "Trafilatura installed: Yes")
})

test_that("find_system_python finds python or returns NULL", {
  # This test checks the function works without error
  # Actual result depends on system configuration
  result <- find_system_python()
  expect_true(is.null(result) || is.character(result))
})

test_that("is_python3 correctly identifies Python 3", {
  # Mock system2 for testing
  mockery::stub(is_python3, "system2", "Python 3.10.0")
  expect_true(is_python3("/fake/python"))

  mockery::stub(is_python3, "system2", "Python 2.7.18")
  expect_false(is_python3("/fake/python"))
})

test_that("clear_trafilatura_cache works", {
  # Should not error even when cache is empty
  expect_silent(clear_trafilatura_cache())

  # Set something in cache and clear it
  assign("trafilatura", "test", envir = mwiR:::.mwi_python_cache)
  clear_trafilatura_cache()
  expect_false(exists("trafilatura", envir = mwiR:::.mwi_python_cache))
})

test_that("ensure_trafilatura handles missing module correctly", {
  # Mock to simulate trafilatura not installed and no auto_setup
  mockery::stub(ensure_trafilatura, "exists", FALSE)
  mockery::stub(ensure_trafilatura, "dir.exists", FALSE)
  mockery::stub(ensure_trafilatura, "reticulate::py_module_available", FALSE)

  expect_error(
    ensure_trafilatura(auto_setup = FALSE),
    "Trafilatura is not installed"
  )
})

test_that("setup_python validates inputs", {
  # Test that force parameter is respected
  expect_type(formals(setup_python)$force, "logical")
  expect_type(formals(setup_python)$quiet, "logical")
})

test_that("remove_python_env handles non-existent env", {
  mockery::stub(remove_python_env, "dir.exists", FALSE)
  expect_message(remove_python_env(), "No mwiR Python environment found")
})
