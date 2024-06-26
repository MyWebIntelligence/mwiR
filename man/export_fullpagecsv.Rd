% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export.R
\name{export_fullpagecsv}
\alias{export_fullpagecsv}
\title{Export Full Page CSV}
\usage{
export_fullpagecsv(con, land_name, minimum_relevance, filename)
}
\arguments{
\item{con}{A database connection object.}

\item{land_name}{A character string specifying the name of the land.}

\item{minimum_relevance}{A numeric value specifying the minimum relevance score for inclusion in the export.}

\item{filename}{A character string specifying the name of the output CSV file.}
}
\value{
An integer indicating the number of rows written to the CSV file.
}
\description{
This function exports detailed data from the specified land to a CSV file based on a minimum relevance score.
}
