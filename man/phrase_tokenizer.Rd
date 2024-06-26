% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crawl.R, R/land.R
\name{phrase_tokenizer}
\alias{phrase_tokenizer}
\title{Tokenize phrases}
\usage{
phrase_tokenizer(text)

phrase_tokenizer(text)
}
\arguments{
\item{text}{A string specifying the phrase to tokenize.}
}
\value{
A character vector of tokenized phrases.

A vector of tokens.
}
\description{
This function splits a text into phrases based on commas.

This function tokenizes a phrase by splitting it into tokens based on commas.
}
\details{
The function splits the input text into tokens using commas as delimiters.
}
\examples{
\dontrun{
phrase_tokenizer("term1, term2, term3")
}
}
