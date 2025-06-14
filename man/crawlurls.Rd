% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crawl.R
\name{crawlurls}
\alias{crawlurls}
\title{Crawler les URLs d'un projet ("land") et mettre à jour la base}
\usage{
crawlurls(
  land_name,
  urlmax = 50,
  limit = NULL,
  http_status = NULL,
  db_name = "mwi.db"
)
}
\arguments{
\item{land_name}{Nom du projet ("land") pour lequel crawler les URLs (chaîne de caractères). Exemple : "Climat".}

\item{urlmax}{Nombre maximal d'URLs à traiter lors de l'appel (entier, par défaut 50).}

\item{limit}{Limite optionnelle sur le nombre total d'URLs à crawler (entier).}

\item{http_status}{Filtre optionnel sur le statut HTTP des URLs à traiter (chaîne de caractères, ex : "404", "200").}

\item{db_name}{Nom du fichier de base de données SQLite à utiliser. Par défaut : "mwi.db".}
}
\value{
Un entier indiquant le nombre d'URLs effectivement traitées.
}
\description{
Cette fonction explore ("crawl") les URLs associées à un projet ("land") donné, met à jour la base de données avec les résultats, et calcule des scores de pertinence pour chaque URL.
\strong{Utilité :} automatiser la collecte, l'analyse et la qualification des contenus web d'un projet thématique.
}
\details{
\subsection{Fonctionnement}{
- Sélectionne les URLs du projet selon les filtres et limites spécifiés.
- Pour chaque URL, tente d'extraire le contenu et les métadonnées.
- Met à jour la base avec les résultats du crawl (statut, texte, métadonnées, etc.).
- Calcule et enregistre un score de pertinence pour chaque URL.
}
\subsection{Conseils d'utilisation}{
- Adaptez \code{urlmax} et \code{limit} selon la taille de votre corpus et la puissance de votre machine.
- Utilisez \code{http_status} pour cibler des URLs problématiques ou à re-crawler.
- Pratique pour maintenir à jour un corpus thématique, relancer des crawls partiels, ou auditer la qualité des contenus.
- Vérifiez la cohérence de la base après un crawl massif.
}
}
\examples{
# Exemple : Crawler 20 URLs pour le projet "Climat"
crawlurls("Climat", urlmax = 20)

# Exemple : Limiter à 100 URLs et ne traiter que les erreurs 404
crawlurls("Climat", urlmax = 50, limit = 100, http_status = "404")

# Exemple : Utiliser une base alternative
crawlurls("Santé publique", urlmax = 10, db_name = "autre_base.db")
}
