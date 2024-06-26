% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crawl.R
\name{get_relevance}
\alias{get_relevance}
\title{Calculate relevance of text}
\usage{
get_relevance(text, weight, dictionary, language = "en")
}
\arguments{
\item{text}{A character string representing the text to be analyzed.}

\item{weight}{A numeric value representing the weight of the relevance.}

\item{dictionary}{A character vector of terms to be used for relevance calculation.}

\item{language}{A character string specifying the language for stemming (default is "en").}
}
\value{
A numeric vector of relevance scores.
}
\description{
This function calculates the relevance of a text based on a dictionary of terms.
}
