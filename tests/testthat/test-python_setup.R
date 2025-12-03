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

# Tests for Windows Store Python detection

test_that("is_windows returns correct platform detection", {
  result <- is_windows()
  expect_type(result, "logical")
  expect_equal(result, .Platform$OS.type == "windows")
})

test_that("is_windows_store_python detects Windows Store paths", {
  # Mock to simulate Windows environment
  mockery::stub(is_windows_store_python, "is_windows", TRUE)

  # Test WindowsApps paths (App Execution Aliases)
  mockery::stub(is_windows_store_python, "normalizePath", function(p, ...) {
    "C:/Users/Test/AppData/Local/Microsoft/WindowsApps/python.exe"
  })
  mockery::stub(is_windows_store_python, "file.info",
                data.frame(size = 1000, stringsAsFactors = FALSE))
  expect_true(is_windows_store_python(
    "C:/Users/Test/AppData/Local/Microsoft/WindowsApps/python.exe"
  ))

  # Test PythonSoftwareFoundation paths (Windows Store install)
  mockery::stub(is_windows_store_python, "normalizePath", function(p, ...) {
    "C:/Program Files/WindowsApps/PythonSoftwareFoundation.Python.3.11/python.exe"
  })
  expect_true(is_windows_store_python(
    "C:/Program Files/WindowsApps/PythonSoftwareFoundation.Python.3.11/python.exe"
  ))
})

test_that("is_windows_store_python allows valid Python paths", {
  # Mock Windows environment
  mockery::stub(is_windows_store_python, "is_windows", TRUE)
  mockery::stub(is_windows_store_python, "file.info",
                data.frame(size = 100000, stringsAsFactors = FALSE))

  # Test standard Python.org installation path
  mockery::stub(is_windows_store_python, "normalizePath", function(p, ...) {
    "C:/Python311/python.exe"
  })
  expect_false(is_windows_store_python("C:/Python311/python.exe"))

  # Test Program Files installation
  mockery::stub(is_windows_store_python, "normalizePath", function(p, ...) {
    "C:/Program Files/Python311/python.exe"
  })
  expect_false(is_windows_store_python("C:/Program Files/Python311/python.exe"))

  # Test LOCALAPPDATA Programs path
  mockery::stub(is_windows_store_python, "normalizePath", function(p, ...) {
    "C:/Users/Test/AppData/Local/Programs/Python/Python311/python.exe"
  })
  expect_false(is_windows_store_python(
    "C:/Users/Test/AppData/Local/Programs/Python/Python311/python.exe"
  ))
})

test_that("is_windows_store_python returns FALSE on non-Windows", {
  mockery::stub(is_windows_store_python, "is_windows", FALSE)

  # Even Windows Store paths should return FALSE on non-Windows
  expect_false(is_windows_store_python(
    "C:/Users/Test/AppData/Local/Microsoft/WindowsApps/python.exe"
  ))
})

test_that("is_windows_store_python detects zero-byte stub files", {
  mockery::stub(is_windows_store_python, "is_windows", TRUE)
  # Use a path that doesn't match other patterns
  mockery::stub(is_windows_store_python, "normalizePath", function(p, ...) {
    "C:/SomePath/python.exe"
  })
  # Zero-byte file is a Windows Store stub
  mockery::stub(is_windows_store_python, "file.info",
                data.frame(size = 0, stringsAsFactors = FALSE))
  expect_true(is_windows_store_python("C:/SomePath/python.exe"))
})

test_that("is_windows_store_python detects Windows 8.3 short name paths", {
  mockery::stub(is_windows_store_python, "is_windows", TRUE)
  # normalizePath does NOT expand short names (returns input as-is)
  mockery::stub(is_windows_store_python, "normalizePath", function(p, ...) p)
  mockery::stub(is_windows_store_python, "file.info",
                data.frame(size = 1000, stringsAsFactors = FALSE))

  # Short name variants that should be detected
  expect_true(is_windows_store_python(
    "C:/Users/Test/AppData/Local/MICROS1/WINDOW1/python3.exe"
  ))
  expect_true(is_windows_store_python(
    "C:/Users/Test/AppData/Local/MICROS~1/WINDOW~1/python3.exe"
  ))
  # Backslash variant
  expect_true(is_windows_store_python(
    "C:\\Users\\Test\\AppData\\Local\\MICROS~1\\WINDOW~1\\python3.exe"
  ))
})

test_that("can_create_virtualenv rejects Windows Store Python", {
  mockery::stub(can_create_virtualenv, "is_windows_store_python", TRUE)
  expect_false(can_create_virtualenv("/fake/python"))
})

test_that("can_create_virtualenv accepts valid Python with venv", {
  mockery::stub(can_create_virtualenv, "is_windows_store_python", FALSE)
  mockery::stub(can_create_virtualenv, "system2", "ok")
  expect_true(can_create_virtualenv("/fake/python"))
})

test_that("can_create_virtualenv rejects Python without venv module", {
  mockery::stub(can_create_virtualenv, "is_windows_store_python", FALSE)
  # Simulate venv import failure (non-zero status)
  mock_result <- "Error: No module named venv"
  attr(mock_result, "status") <- 1
  mockery::stub(can_create_virtualenv, "system2", mock_result)
  expect_false(can_create_virtualenv("/fake/python"))
})

test_that("find_system_python skips Windows Store Python", {
  # This test verifies the filtering logic works
  mockery::stub(find_system_python, "is_windows", TRUE)

  # First candidate is Windows Store, second is valid
  mockery::stub(find_system_python, "Sys.which", function(name) {
    if (name == "python3") {
      return("C:/Users/Test/AppData/Local/Microsoft/WindowsApps/python3.exe")
    } else if (name == "python") {
      return("C:/Python311/python.exe")
    }
    return("")
  })

  mockery::stub(find_system_python, "is_windows_store_python", function(path) {
    grepl("WindowsApps", path)
  })
  mockery::stub(find_system_python, "is_python3", TRUE)
  mockery::stub(find_system_python, "can_create_virtualenv", TRUE)
  mockery::stub(find_system_python, "dir.exists", FALSE)
  # normalizePath needs to accept all arguments
  mockery::stub(find_system_python, "normalizePath", function(path, ...) path)

  result <- find_system_python()
  # Should return the valid Python, not the Windows Store one
  expect_false(grepl("WindowsApps", result))
  expect_true(grepl("Python311", result))
})
