% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crawl.R, R/land.R
\name{stem_word}
\alias{stem_word}
\title{Perform Stemming on a Word}
\usage{
stem_word(word, language = "en")
}
\arguments{
\item{word}{A character string representing the word to be stemmed.}

\item{language}{A character string specifying the language to use for stemming. Default is "en" (English).}
}
\value{
Returns the stemmed version of the input word as a character string.
}
\description{
This function applies a stemming algorithm to the input word, reducing it to its root or base form based on the specified language. Stemming helps in normalizing words for text analysis by grouping different forms of a word under a common base.
}
\details{
The function first converts the input word to lowercase, then applies the stemming process according to the language rules. This improves matching and comparison in natural language processing tasks.
}
\examples{
\dontrun{
# Stem the word "running" in English
stem_word("running", "en")
}
}
