% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crawl.R
\name{expression_relevance}
\alias{expression_relevance}
\title{Calculate Relevance Score for an Expression}
\usage{
expression_relevance(dictionary, expression, language = "fr")
}
\arguments{
\item{dictionary}{A character vector of terms that serve as the reference for calculating relevance. These terms are used to assess how closely the expression matches the desired concepts.}

\item{expression}{A data frame containing the title and text fields of the expression to be evaluated. The content of these fields is analyzed to compute the relevance score.}

\item{language}{A character string specifying the language used for stemming and text processing. The default is "fr" (French), which affects how words are reduced to their root forms for comparison.}
}
\value{
Returns a numeric value representing the calculated relevance score of the expression. This score quantifies how relevant the expression is with respect to the provided dictionary terms.
}
\description{
This function computes a relevance score for a given expression by analyzing its title and text content. It uses the provided dictionary of terms to determine how closely the expression matches the key concepts. The language parameter controls the stemming process, which reduces words to their base forms to improve matching accuracy. This relevance score can be used to filter or rank expressions based on their importance or pertinence.
}
