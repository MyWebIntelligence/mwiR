% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initmwi.R
\name{initmwi}
\alias{initmwi}
\title{Initialize Environment and Install Required Packages with User Prompt}
\usage{
initmwi()
}
\value{
Returns NULL. The function is called for its side effects, primarily to set up the environment.
}
\description{
This function prepares the R environment by ensuring all necessary packages are installed and loaded. It also verifies the presence of the `@postlight/parser` npm package and the `trafilatura` Python module, issuing warnings if they are missing. Additionally, it interactively prompts the user to input their SERP API key, which is then stored in the global environment for use in subsequent operations.
}
\details{
The function executes the following steps:
\enumerate{
  \item Checks for required R packages and installs any that are missing.
  \item Loads all required R packages into the session.
  \item Verifies if the `@postlight/parser` npm package is installed; if not, it warns the user.
  \item Checks for the `trafilatura` Python module and warns if it is not available.
  \item Prompts the user to enter their SERP API key and saves it in the global environment for easy access.
}
}
\examples{
\dontrun{
# Initialize environment and install packages
initmwi()
}
}
