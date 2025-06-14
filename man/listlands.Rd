% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/land.R
\name{listlands}
\alias{listlands}
\title{List Research Projects (Lands) with Detailed Information}
\usage{
listlands(land_name = NULL, db_name = "mwi.db")
}
\arguments{
\item{land_name}{A string specifying the name of a specific land to list. If NULL, the function lists all lands. Default is NULL.}

\item{db_name}{A string specifying the filename of the SQLite database to query. Default is "mwi.db".}
}
\value{
This function is called for its side effects and does not return a value.
}
\description{
This function retrieves and displays detailed information about research projects (lands) stored in the specified SQLite database. It can list all lands or a specific land, providing insights into each project's metadata and status.
}
\details{
For each land, the function retrieves and displays the following information:
- Name of the land (research project)
- Creation date of the land
- Description of the land
- Terms included in the land's dictionary
- Total number of expressions associated with the land
- Number of expressions still to be fetched
- HTTP status codes of the expressions, indicating their retrieval status
}
\examples{
\dontrun{
# List all lands
listlands()

# List details for a specific land
listlands("Climate Research", "my_database.db")
}
}
