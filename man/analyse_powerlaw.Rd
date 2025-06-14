% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recode.R
\name{analyse_powerlaw}
\alias{analyse_powerlaw}
\title{Tester si une distribution suit une loi de puissance (power law)}
\usage{
analyse_powerlaw(x, type = "discrete")
}
\arguments{
\item{x}{Vecteur de données numériques positives à analyser. Par exemple, des tailles de groupes, des degrés de sommets dans un réseau, des fréquences d’occurrence, etc.}

\item{type}{Type de données à analyser :
\describe{
  \item{"discrete"}{Pour des données de type compte (entiers, ex : nombre de liens, nombre d’occurrences).}
  \item{"continuous"}{Pour des mesures continues (ex : tailles, durées, valeurs réelles).}
}
Le choix du type est crucial car il détermine la méthode d’estimation statistique utilisée.}
}
\value{
Une liste contenant :
\itemize{
  \item \code{xmin} : Valeur minimale à partir de laquelle la loi de puissance est ajustée.
  \item \code{alpha} : Exposant estimé de la loi de puissance.
  \item \code{gof_p} : p-value du test de bootstrap (goodness-of-fit) indiquant la qualité de l’ajustement.
  \item \code{poweRlaw} : L’objet complet retourné par le package poweRlaw, pour analyses avancées.
}
}
\description{
Cette fonction permet de tester si la queue (partie extrême) d’une distribution de données suit une loi de puissance (power law), ce qui est fréquent dans de nombreux phénomènes naturels et sociaux (ex : réseaux, tailles de villes, distribution de richesses).
\strong{Attention :} il ne s’agit pas d’une fonction de transformation, mais d’un outil d’analyse statistique.
}
\details{
\subsection{Conseils d’utilisation}{
- Utilisez cette fonction pour explorer la présence de lois de puissance dans vos données, notamment pour les distributions très asymétriques.
- Vérifiez la nature de vos données (comptes ou mesures continues) avant de choisir le type.
- Une p-value élevée (\code{gof_p} > 0.1) indique que l’hypothèse de loi de puissance n’est pas rejetée.
- Pour des analyses avancées, exploitez l’objet \code{poweRlaw} retourné.
}
}
\examples{
# Exemple : Tester une loi de puissance sur des degrés de réseau (entiers)
degres <- c(1, 2, 2, 3, 5, 8, 13, 21, 34)
resultat <- analyse_powerlaw(degres, type = "discrete")
print(resultat$alpha)
print(resultat$gof_p)

# Exemple : Tester sur des tailles continues
tailles <- rexp(100, rate = 0.1)
analyse_powerlaw(tailles, type = "continuous")
}
