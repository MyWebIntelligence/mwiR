% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export.R
\name{export_nodegexf}
\alias{export_nodegexf}
\title{Export Domain Node Data to GEXF Format for Network Analysis}
\usage{
export_nodegexf(con, land_name, minimum_relevance, filename)
}
\arguments{
\item{con}{A database connection object. This connection is used to access the database containing domain and expression data.}

\item{land_name}{A character string specifying the name of the land (dataset or domain) from which node data will be exported. This helps filter the relevant subset of data.}

\item{minimum_relevance}{A numeric value specifying the minimum relevance score threshold. Only nodes with an average relevance score equal to or above this threshold will be included in the export. This allows focusing on the most significant nodes.}

\item{filename}{A character string specifying the name (and path) of the output GEXF file where the exported node data will be saved.}
}
\description{
This function exports domain node data from a specified land (dataset) to a GEXF file, which is a standard XML-based format used for representing network graphs. The exported data includes nodes and their attributes filtered by a minimum relevance score threshold. This GEXF file can be used for network analysis and visualization in compatible tools such as Gephi.
}
