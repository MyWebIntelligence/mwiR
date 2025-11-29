#' Python Environment Management for mwiR
#'
#' Functions to manage Python environment and trafilatura installation.
#' Provides automatic setup, caching, and diagnostics for seamless integration.
#'
#' @name python_setup
#' @keywords internal
NULL

# Package-level environment for caching Python objects
.mwi_python_cache <- new.env(parent = emptyenv())

#' Get mwiR Virtual Environment Path
#'
#' Returns the path to the dedicated Python virtual environment for mwiR.
#' The virtualenv is stored in the user's R cache directory for persistence.
#'
#' @return Character string with the path to the mwiR virtualenv.
#' @export
#' @examples
#' \dontrun{
#' mwi_python_env()
#' }
mwi_python_env <- function() {
 # Use R's standard cache location (platform-independent)
  cache_dir <- tools::R_user_dir("mwiR", which = "cache")
  file.path(cache_dir, "python_venv")
}

#' Check Python and Trafilatura Status
#'
#' Provides diagnostic information about the Python environment and trafilatura
#' installation status. Useful for troubleshooting.
#'
#' @return A list with status information (python_available, virtualenv_exists,
#'         trafilatura_installed, python_version, trafilatura_version).
#' @export
#' @examples
#' \dontrun{
#' status <- check_python_status()
#' print(status)
#' }
check_python_status <- function() {
  status <- list(
    python_available = FALSE,
    virtualenv_exists = FALSE,
    trafilatura_installed = FALSE,
    python_version = NA_character_,
    trafilatura_version = NA_character_,
    virtualenv_path = mwi_python_env()
  )

  # Check if reticulate can find Python
  tryCatch({
    status$python_available <- reticulate::py_available(initialize = FALSE)
  }, error = function(e) {
    status$python_available <- FALSE
  })

  # Check if our virtualenv exists
  venv_path <- mwi_python_env()
  status$virtualenv_exists <- dir.exists(venv_path)

  # If virtualenv exists, try to use it
 if (status$virtualenv_exists) {
    tryCatch({
      reticulate::use_virtualenv(venv_path, required = FALSE)

      # Get Python version
      py_config <- reticulate::py_config()
      status$python_version <- py_config$version

      # Check trafilatura
      if (reticulate::py_module_available("trafilatura")) {
        status$trafilatura_installed <- TRUE
        tryCatch({
          traf <- reticulate::import("trafilatura", delay_load = FALSE)
          status$trafilatura_version <- traf$`__version__`
        }, error = function(e) {
          status$trafilatura_version <- "unknown"
        })
      }
    }, error = function(e) {
      # Silent fail - status already set to FALSE
    })
  }

  class(status) <- c("mwi_python_status", "list")
  return(status)
}

#' Print Python Status
#'
#' @param x A mwi_python_status object.
#' @param ... Additional arguments (ignored).
#' @export
print.mwi_python_status <- function(x, ...) {
  cat("mwiR Python Environment Status\n")
  cat("================================\n")
  cat("Virtualenv path:", x$virtualenv_path, "\n")
  cat("Virtualenv exists:", ifelse(x$virtualenv_exists, "Yes", "No"), "\n")
  cat("Python available:", ifelse(x$python_available, "Yes", "No"), "\n")

  if (!is.na(x$python_version)) {
    cat("Python version:", x$python_version, "\n")
  }

  cat("Trafilatura installed:", ifelse(x$trafilatura_installed, "Yes", "No"), "\n")

  if (!is.na(x$trafilatura_version) && x$trafilatura_version != "unknown") {
    cat("Trafilatura version:", x$trafilatura_version, "\n")
  }

  if (!x$trafilatura_installed) {
    cat("\nTo install trafilatura, run: setup_python()\n")
  }

  invisible(x)
}

#' Setup Python Environment for mwiR
#'
#' Creates a dedicated virtual environment and installs trafilatura.
#' This function is idempotent - safe to call multiple times.
#'
#' @param force Logical. If TRUE, recreates the virtualenv even if it exists.
#' @param quiet Logical. If TRUE, suppresses progress messages.
#' @return Logical indicating success (invisible).
#' @export
#' @examples
#' \dontrun{
#' setup_python()
#' setup_python(force = TRUE)  # Reinstall everything
#' }
setup_python <- function(force = FALSE, quiet = FALSE) {
  venv_path <- mwi_python_env()

  # Step 1: Ensure cache directory exists
  cache_dir <- dirname(venv_path)
  if (!dir.exists(cache_dir)) {
    if (!quiet) message("Creating mwiR cache directory...")
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Step 2: Check if we need to create virtualenv
  venv_exists <- dir.exists(venv_path)

  if (force && venv_exists) {
    if (!quiet) message("Removing existing virtualenv (force=TRUE)...")
    unlink(venv_path, recursive = TRUE)
    venv_exists <- FALSE
  }

  # Step 3: Create virtualenv if needed
  if (!venv_exists) {
    if (!quiet) message("Creating Python virtual environment...")

    # Find system Python
    python_path <- find_system_python()

    if (is.null(python_path)) {
      stop(
        "Python 3 not found on your system.\n",
        "Please install Python 3:\n",
        "  - macOS: brew install python3\n",
        "  - Ubuntu/Debian: sudo apt install python3 python3-venv\n",
        "  - Windows: https://www.python.org/downloads/",
        call. = FALSE
      )
    }

    if (!quiet) message("Using Python: ", python_path)

    # Create virtualenv using reticulate
    tryCatch({
      reticulate::virtualenv_create(venv_path, python = python_path)
    }, error = function(e) {
      stop(
        "Failed to create virtual environment: ", e$message, "\n",
        "Try installing python3-venv:\n",
        "  - Ubuntu/Debian: sudo apt install python3-venv\n",
        "  - macOS: brew reinstall python3",
        call. = FALSE
      )
    })
  }

  # Step 4: Activate the virtualenv
  if (!quiet) message("Activating virtual environment...")
  reticulate::use_virtualenv(venv_path, required = TRUE)

  # Step 5: Install trafilatura if needed
  if (!reticulate::py_module_available("trafilatura")) {
    if (!quiet) message("Installing trafilatura (this may take a minute)...")

    tryCatch({
      reticulate::py_install(
        packages = "trafilatura",
        envname = venv_path,
        pip = TRUE
      )
    }, error = function(e) {
      stop(
        "Failed to install trafilatura: ", e$message, "\n",
        "Try manually: pip3 install trafilatura",
        call. = FALSE
      )
    })

    if (!quiet) message("Trafilatura installed successfully!")
  } else {
    if (!quiet) message("Trafilatura already installed.")
  }

  # Clear cache to force reimport
  if (exists("trafilatura", envir = .mwi_python_cache)) {
    rm("trafilatura", envir = .mwi_python_cache)
  }

  if (!quiet) {
    message("Python environment ready!")
    message("Virtualenv: ", venv_path)
  }

  invisible(TRUE)
}

#' Find System Python 3
#'
#' Searches for Python 3 executable on the system.
#'
#' @return Path to Python 3 executable or NULL if not found.
#' @keywords internal
find_system_python <- function() {
  # Common Python 3 executable names
  python_names <- c("python3", "python")

  # Platform-specific paths
  if (.Platform$OS.type == "windows") {
    extra_paths <- c(
      file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "Python"),
      "C:/Python3",
      "C:/Python39",
      "C:/Python310",
      "C:/Python311",
      "C:/Python312"
    )
  } else {
    extra_paths <- c(
      "/usr/bin",
      "/usr/local/bin",
      "/opt/homebrew/bin",  # macOS ARM
      "/opt/local/bin"      # MacPorts
    )
  }

  # Try each name in PATH first
  for (name in python_names) {
    path <- Sys.which(name)
    if (path != "" && is_python3(path)) {
      return(path)
    }
  }

  # Try extra paths
  for (dir in extra_paths) {
    if (dir.exists(dir)) {
      for (name in python_names) {
        path <- file.path(dir, name)
        if (.Platform$OS.type == "windows") {
          path <- paste0(path, ".exe")
        }
        if (file.exists(path) && is_python3(path)) {
          return(path)
        }
      }
    }
  }

  return(NULL)
}

#' Check if Python is Version 3
#'
#' @param python_path Path to Python executable.
#' @return Logical indicating if it's Python 3.
#' @keywords internal
is_python3 <- function(python_path) {
  tryCatch({
    result <- system2(python_path, "--version", stdout = TRUE, stderr = TRUE)
    grepl("^Python 3", result[1])
  }, error = function(e) FALSE)
}

#' Get Trafilatura Module (Cached)
#'
#' Returns the trafilatura Python module, using a cached version if available.
#' Automatically sets up the Python environment if needed.
#'
#' This function is the main entry point for using trafilatura in mwiR.
#' It handles all the complexity of Python environment management.
#'
#' @param auto_setup Logical. If TRUE (default), automatically runs setup_python()
#'        if trafilatura is not available.
#' @return The trafilatura Python module object.
#' @export
#' @examples
#' \dontrun{
#' traf <- ensure_trafilatura()
#' content <- traf$extract(html_content, output_format = "markdown")
#' }
ensure_trafilatura <- function(auto_setup = TRUE) {
  # Check cache first
  if (exists("trafilatura", envir = .mwi_python_cache)) {
    cached <- get("trafilatura", envir = .mwi_python_cache)
    # Verify the cached module is still valid
    if (!is.null(cached)) {
      tryCatch({
        # Quick validation - try to access a known attribute
        cached$`__version__`
        return(cached)
      }, error = function(e) {
        # Cache invalid, remove it
        rm("trafilatura", envir = .mwi_python_cache)
      })
    }
  }

  # Ensure virtualenv is active
  venv_path <- mwi_python_env()

  if (dir.exists(venv_path)) {
    tryCatch({
      reticulate::use_virtualenv(venv_path, required = FALSE)
    }, error = function(e) {
      # Virtualenv might be corrupted
      if (auto_setup) {
        message("Virtualenv appears corrupted, recreating...")
        setup_python(force = TRUE, quiet = FALSE)
      }
    })
  }

  # Check if trafilatura is available
  if (!reticulate::py_module_available("trafilatura")) {
    if (auto_setup) {
      message("Trafilatura not found. Setting up Python environment...")
      setup_python(quiet = FALSE)
    } else {
      stop(
        "Trafilatura is not installed.\n",
        "Run setup_python() to install it automatically, or:\n",
        "  pip3 install trafilatura",
        call. = FALSE
      )
    }
  }

  # Import trafilatura
  trafilatura <- tryCatch({
    reticulate::import("trafilatura", delay_load = FALSE)
  }, error = function(e) {
    stop(
      "Failed to import trafilatura: ", e$message, "\n",
      "Try running: setup_python(force = TRUE)",
      call. = FALSE
    )
  })

  # Cache the module
  assign("trafilatura", trafilatura, envir = .mwi_python_cache)

  return(trafilatura)
}

#' Clear Trafilatura Cache
#'
#' Clears the cached trafilatura module. Useful if you need to
#' reload the module after an update.
#'
#' @return NULL (invisible).
#' @export
#' @examples
#' \dontrun{
#' clear_trafilatura_cache()
#' }
clear_trafilatura_cache <- function() {
  if (exists("trafilatura", envir = .mwi_python_cache)) {
    rm("trafilatura", envir = .mwi_python_cache)
  }
  invisible(NULL)
}

#' Remove mwiR Python Environment
#'
#' Completely removes the mwiR Python virtual environment.
#' Use this to clean up or troubleshoot Python issues.
#'
#' @return Logical indicating if removal was successful (invisible).
#' @export
#' @examples
#' \dontrun{
#' remove_python_env()
#' setup_python()  # Reinstall from scratch
#' }
remove_python_env <- function() {
  venv_path <- mwi_python_env()

  if (dir.exists(venv_path)) {
    message("Removing mwiR Python environment...")
    unlink(venv_path, recursive = TRUE)
    clear_trafilatura_cache()
    message("Done. Run setup_python() to reinstall.")
    return(invisible(TRUE))
  } else {
    message("No mwiR Python environment found.")
    return(invisible(FALSE))
  }
}
