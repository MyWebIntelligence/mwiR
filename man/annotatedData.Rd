% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recode.R
\name{annotatedData}
\alias{annotatedData}
\title{Mettre à jour une table de la base de données avec des données modifiées à l'extérieur du projet}
\usage{
annotatedData(dataplus, table, champ, by, labase = "mwi.db")
}
\arguments{
\item{dataplus}{Un data frame contenant les données modifiées à réintégrer dans la base. Par exemple, un tableau exporté, annoté ou corrigé dans un tableur, puis réimporté dans R.}

\item{table}{Nom de la table à mettre à jour dans la base de données (chaîne de caractères). Exemple : "expression".}

\item{champ}{Nom du champ (colonne) à mettre à jour (chaîne de caractères). Exemple : "description".}

\item{by}{Nom du champ clé (colonne identifiant unique) utilisé pour faire la correspondance entre les lignes du data frame et celles de la base. Exemple : "id".}

\item{labase}{Nom du fichier de base de données SQLite à utiliser. Par défaut : "mwi.db".}
}
\value{
Cette fonction ne retourne pas de valeur. Elle met à jour la table spécifiée dans la base de données et affiche un message de succès à la fin.
}
\description{
Cette fonction permet de réintégrer dans la base de données du projet des données qui ont été modifiées à l'extérieur de l'environnement R (par exemple, par des collaborateurs ou via un tableur). 
\strong>Utilité :</strong> faciliter les workflows collaboratifs ou open science où les données sont exportées, annotées, puis réimportées pour mise à jour centralisée.
}
\details{
\subsection{Fonctionnement étape par étape}{
\itemize{
  \item Établit une connexion à la base de données SQLite spécifiée.
  \item Vérifie l'existence de la table et des champs concernés.
  \item Prépare une requête SQL UPDATE pour chaque ligne à mettre à jour.
  \item Exécute les mises à jour dans une transaction (sécurité et performance).
  \item Valide les changements si tout s'est bien passé, sinon annule (rollback).
}
}
\subsection{Conseils et bonnes pratiques}{
- Vérifiez que la structure de \code{dataplus} correspond bien à la table cible, notamment les colonnes \code{champ} et \code{by}.
- La fonction applique le principe "tout ou rien" : en cas d'erreur, aucune modification n'est enregistrée (transaction).
- Sauvegardez votre base avant toute opération massive.
- Pratique pour intégrer des corrections, annotations ou enrichissements faits par des tiers.
}
}
\examples{
# Exemple : Mettre à jour la colonne "description" de la table "expression"
# à partir d'un data frame modifié dans un tableur puis réimporté dans R
modifs <- data.frame(id = c(1, 2), description = c("Nouveau texte 1", "Nouveau texte 2"))
annotatedData(dataplus = modifs,
              table = "expression",
              champ = "description",
              by = "id",
              labase = "mwi.db")
}
