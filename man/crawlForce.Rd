% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crawl.R
\name{crawlForce}
\alias{crawlForce}
\title{Force crawl for a specific land}
\usage{
crawlForce(land_name, db_name = "mwi.db")
}
\arguments{
\item{land_name}{A character string representing the name of the land.}

\item{db_name}{A character string representing the name of the database (default is "mwi.db").}
}
\description{
This function forces a crawl by resetting the fetched status for URLs that have not been approved. So next time the crawl will rework on this url.
}
