% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export.R
\name{export_land}
\alias{export_land}
\title{Gérer l'export des données d'un territoire}
\usage{
export_land(land_name, export_type, minimum_relevance = 1, labase = "mwi.db")
}
\arguments{
\item{land_name}{Chaîne de caractères indiquant le nom du territoire (land) dont on souhaite exporter les données.}
\item{export_type}{Type d'export à réaliser. Les options possibles sont : \code{"pagecsv"}, \code{"fullpagecsv"}, \code{"nodecsv"}, \code{"mediacsv"}, \code{"pagegexf"}, \code{"nodegexf"}, ou \code{"corpus"}. Chaque type correspond à un format ou un contenu d'export spécifique.}
\item{minimum_relevance}{Valeur numérique définissant le seuil minimal de pertinence pour inclure les données dans l'export. Par défaut : 1.}
\item{labase}{Nom du fichier de la base de données à utiliser. Par défaut : \code{"mwi.db"}.}
}
\description{
Cette fonction centralise la gestion de l'export des données d'un territoire selon le type d'export spécifié. 
Elle permet de choisir facilement le format et le contenu des données exportées, en filtrant les données selon un seuil de pertinence.
Cette gestion facilite l'automatisation et la standardisation des exports pour différents usages (analyse, visualisation, archivage).
}
