% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initmwi.R
\name{initmwi}
\alias{initmwi}
\title{Initialize Environment and Install Required Packages}
\usage{
initmwi()
}
\value{
NULL. The function is called for its side effects.
}
\description{
This function initializes the R environment by installing and loading the required packages.
It also checks the installation of the `@postlight/parser` npm package and the `trafilatura` Python module.
Additionally, it prompts the user to enter their SERP API key and stores it in the global environment.
}
\details{
The function performs the following steps:
\enumerate{
  \item Checks for the required R packages and installs them if they are missing.
  \item Loads the required R packages.
  \item Checks if the `@postlight/parser` npm package is installed and provides a warning if it is not.
  \item Checks if the `trafilatura` Python module is available and provides a warning if it is not.
  \item Prompts the user to enter their SERP API key and stores it in the global environment.
}
}
\examples{
\dontrun{
initmwi()
}
}
