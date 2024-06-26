% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crawl.R
\name{get_last_memento_url}
\alias{get_last_memento_url}
\title{Get the most recent archived URL from Archive.org}
\usage{
get_last_memento_url(url)
}
\arguments{
\item{url}{A character string representing the URL to be archived.}
}
\value{
A character string containing the most recent archived URL, or NULL if not available.
}
\description{
This function retrieves the most recent archived URL from Archive.org for a given URL.
}
